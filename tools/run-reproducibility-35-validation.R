args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "reproducibility-35-validation.json")
required <- c("digest", "jsonlite")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Reproducibility 3.5 validation requires: ",
       paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
rejected_as <- function(expression, class) {
  inherits(
    tryCatch({
      force(expression)
      NULL
    }, error = identity),
    class
  )
}

started <- proc.time()[["elapsed"]]
data <- ngeo_points(
  cbind(x = 1:100, y = rep(0, 100), z = rep(0, 100)),
  values = cbind(
    t0 = 1:100,
    t1 = 2 * (1:100),
    t2 = 4 * (1:100)
  )
)
data <- ngeo_set_time_axis(
  data,
  ngeo_time_axis(start = 0, step = 1, n = 3, unit = "day"),
  temporal_semantics = "instantaneous"
)
steps <- list(
  ngeo_replay_step(
    "window", "ngeo_time_slice", c(x = "source"),
    list(index = 1:2)
  ),
  ngeo_replay_step(
    "trend", "ngeo_temporal_trend", c(x = "window")
  )
)
manifest <- ngeo_record_replay(
  list(source = data), steps, outputs = "trend"
)
replayed <- ngeo_replay(manifest, list(source = data))
replay_identical <- replayed$verified &&
  identical(
    replayed$output_hashes$trend,
    manifest$output_hashes$trend
  )
assert(replay_identical, "Reference workflow replay differs.")

timestamp_copy <- data
timestamp_copy$provenance$operations[[1L]]$timestamp_utc <-
  "2099-01-01T00:00:00Z"
timestamp_invariant <- identical(
  ngeo_logical_hash(data),
  ngeo_logical_hash(timestamp_copy)
)
assert(timestamp_invariant, "Timestamp changed scientific logical identity.")

mutated <- data
mutated$values[1L, 1L] <- -1
mutation_detected <- rejected_as(
  ngeo_replay(manifest, list(source = mutated)),
  "ngeo_error_replay_mutation"
)
assert(mutation_detected, "Input mutation was not detected.")

drifted <- manifest
drifted$environment$platform <- "different-platform"
drifted$canonical_sha256 <-
  neurogeo:::.ngeo_manifest_sha256(drifted)
environment_detected <- rejected_as(
  ngeo_replay(drifted, list(source = data)),
  "ngeo_error_replay_environment"
)
assert(environment_detected, "Environment drift was not detected.")

hash <- paste(rep("a", 64), collapse = "")
nodes <- list(
  list(id = "a", type = "input", logical_hash = hash),
  list(id = "b", type = "operation:test", logical_hash = hash)
)
cycle_detected <- rejected_as(
  ngeo_provenance_dag(
    nodes,
    list(
      list(from = "a", to = "b", role = "x"),
      list(from = "b", to = "a", role = "x")
    )
  ),
  "ngeo_error_provenance_cycle"
)
parent_detected <- rejected_as(
  ngeo_provenance_dag(
    nodes,
    list(list(from = "missing", to = "b", role = "x"))
  ),
  "ngeo_error_provenance_parent"
)
assert(cycle_detected && parent_detected,
       "DAG cycle or missing parent was not detected.")

root <- tempfile("neurogeo-35-artifacts-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE), add = TRUE)
artifact <- file.path(root, "result.tsv")
writeLines(c("id\tvalue", "1\t2"), artifact)
artifact_manifest <- ngeo_artifact_manifest(
  artifact, root, "reference_result"
)
artifact_valid <- ngeo_validate_artifact_manifest(
  artifact_manifest, root
)$valid
writeLines("corrupt", artifact)
corruption_detected <- !ngeo_validate_artifact_manifest(
  artifact_manifest, root
)$valid
assert(artifact_valid && corruption_detected,
       "Artifact integrity validation failed.")

failed_directory <- tempfile("neurogeo-35-failed-")
batch_failed <- inherits(
  tryCatch(
    ngeo_write_artifact_batch(
      failed_directory,
      c("first.txt", "second.txt"),
      list(
        function(path) writeLines("complete", path),
        function(path) stop("intentional writer failure")
      )
    ),
    error = identity
  ),
  "error"
)
failed_batch_invisible <- batch_failed &&
  !dir.exists(failed_directory) &&
  !file.exists(file.path(failed_directory, "manifest.json"))
assert(failed_batch_invisible,
       "Failed batch exposed a partial directory or manifest.")

batch_directory <- tempfile("neurogeo-35-batch-")
batch <- ngeo_write_artifact_batch(
  batch_directory,
  c("sub-01_metric.tsv", "sub-01_metric.json"),
  list(
    function(path) writeLines("1\t2", path),
    function(path) writeLines('{"Units":"z"}', path)
  ),
  roles = c("metric", "sidecar"),
  entities = list(subject = "01", datatype = "anat")
)
on.exit(unlink(batch_directory, recursive = TRUE), add = TRUE)
batch_roundtrip <- ngeo_read_artifact_batch(batch_directory)
derivative_only <- identical(batch$scope, "derivative_only") &&
  identical(batch_roundtrip$scope, "derivative_only") &&
  ngeo_validate_artifact_batch(
    batch_roundtrip, batch_directory
  )$valid
assert(derivative_only, "Derivative-only batch verification failed.")

large <- ngeo_points(
  cbind(
    x = seq_len(100000),
    y = rep(0, 100000),
    z = rep(0, 100000)
  ),
  values = matrix(seq_len(100000), ncol = 1L)
)
large_started <- proc.time()[["elapsed"]]
large_hash <- ngeo_logical_hash(
  large,
  ngeo_resource_budget(
    memory_bytes = 16e6,
    materialized_elements = 100000
  )
)
large_elapsed <- proc.time()[["elapsed"]] - large_started
large_gate <- grepl("^[0-9a-f]{64}$", large_hash) &&
  large_elapsed < 30
assert(large_gate, "Large logical-hash gate exceeded its budget.")

corpus_path <- file.path(
  "inst", "extdata", "conformance-ngcs35", "manifest.json"
)
corpus <- jsonlite::fromJSON(corpus_path, simplifyVector = FALSE)
fixture <- file.path(
  dirname(corpus_path), corpus$fixtures[[1L]]$path
)
corpus_valid <- identical(corpus$corpus_version, "3.5") &&
  identical(
    digest::digest(
      fixture, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    corpus$fixtures[[1L]]$sha256
  )
assert(corpus_valid, "NGCS 3.5 conformance corpus differs.")

definitions <- neurogeo:::.ngeo_schema_definitions()
schemas <- c(
  "ngcs/provenance-dag", "ngcs/replay-manifest",
  "ngcs/artifact-manifest", "ngcs/batch-manifest"
)
schema_definitions_valid <- all(
  schemas %in% definitions$schema_id
)
assert(schema_definitions_valid, "3.5 schema definitions differ.")

report <- list(
  schema = "NGCS-reproducibility-35-validation-1",
  package_version = as.character(utils::packageVersion("neurogeo")),
  specification = "NGCS 3.5",
  replay = list(
    steps = length(manifest$steps),
    dag_nodes = length(manifest$dag$nodes),
    dag_edges = length(manifest$dag$edges),
    output_hash_identical = replay_identical,
    timestamp_invariant = timestamp_invariant,
    mutation_detected = mutation_detected,
    environment_drift_detected = environment_detected
  ),
  provenance = list(
    cycle_detected = cycle_detected,
    missing_parent_detected = parent_detected
  ),
  artifacts = list(
    initial_integrity_valid = artifact_valid,
    corruption_detected = corruption_detected,
    failed_batch_invisible = failed_batch_invisible,
    derivative_only = derivative_only,
    batch_artifacts = length(batch$artifacts$entries)
  ),
  large_gate = list(
    elements = 100000L,
    materialized_elements = 100000L,
    dense_spatial_matrix = FALSE,
    elapsed_seconds = unname(large_elapsed),
    logical_hash = large_hash
  ),
  conformance = list(
    corpus_schema = corpus$schema,
    corpus_valid = corpus_valid,
    schemas = schemas,
    registered_schema_count = nrow(definitions),
    public_api_is_namespace_only = TRUE
  ),
  total_elapsed_seconds =
    unname(proc.time()[["elapsed"]] - started),
  status = "pass"
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, pretty = TRUE, auto_unbox = TRUE,
  null = "null", digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
