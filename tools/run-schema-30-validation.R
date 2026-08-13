args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "schema-30-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("NGCS schema validation requires jsonlite and digest.")
}
suppressPackageStartupMessages(library(neurogeo))

definitions <- neurogeo:::.ngeo_schema_definitions()
required_schema_ids <- c(
  "ngcs/ngeo-surface", "ngcs/ngeo-volume", "ngcs/ngeo-point",
  "ngcs/ngeo-grayordinate", "ngcs/ngeo-parcellation",
  "ngcs/coordinate_space",
  "ngcs/transform", "ngcs/spatial_weights", "ngcs/partition",
  "ngcs/support-map", "ngcs/support-covariance",
  "ngcs/support-ensemble", "ngcs/coordinate_space-registry",
  "ngcs/transform-graph", "ngcs/resource-budget"
)
definitions_complete <- all(
  required_schema_ids %in% definitions$schema_id
) && all(lengths(definitions$invariants) > 0L)
obsolete_schemas_absent <- !any(c(
  "ngcs/block-support-map",
  "ngcs/delayed-values",
  "ngcs/execution-plan"
) %in% definitions$schema_id)

space <- ngeo_coordinate_space(
  "reference-points", source_metadata = list(dimension = 2L)
)
x <- ngeo_point(
  cbind(x = c(0, 1, 0), y = c(0, 0, 1)),
  values = cbind(signal = c(1, 2, 3)),
  measures = ngeo_measure(support_behavior = "intensive"),
  coordinate_space = space
)
ngeo_validate(x)
manifest_a <- ngeo_object_manifest(x)
manifest_b <- ngeo_object_manifest(x)
deterministic_manifest <- identical(
  manifest_a$canonical_sha256,
  manifest_b$canonical_sha256
)
manifest_path <- tempfile(fileext = ".json")
atomic_output <- write_ngeo_manifest(manifest_a, manifest_path)
restored <- read_ngeo_manifest(manifest_path)
manifest_roundtrip <- ngeo_validate_manifest(restored, x)$valid &&
  identical(
    restored$canonical_sha256,
    manifest_a$canonical_sha256
  ) &&
  file.exists(atomic_output$path)

corrupt <- restored
corrupt$metadata$element_count <- 999L
corruption_report <- ngeo_validate_manifest(corrupt)
corruption_rejected <- !corruption_report$valid &&
  identical(corruption_report$issues$code, "MANIFEST_HASH")

invalid <- x
invalid$values <- invalid$values[-1L, , drop = FALSE]
first_error <- tryCatch(
  {
    ngeo_validate(invalid)
    NULL
  },
  error = identity
)
second_error <- tryCatch(
  {
    ngeo_validate(invalid)
    NULL
  },
  error = identity
)
adversarial_deterministic <- inherits(
  first_error, "ngeo_error_alignment"
) && identical(conditionMessage(first_error), conditionMessage(second_error))

exports <- getNamespaceExports("neurogeo")
removed_api <- c(
  "ngeo_metric", "ngeo_delayed_values", "ngeo_block_support_map",
  "ngeo_execution_plan", "ngeo_execute", "ngeo_cache",
  "ngeo_atomic_write", "ngeo_gwr_batched", "ngeo_kriging_batched",
  "ngeo_schema_registry", "ngeo_schema", "ngeo_validate_schema",
  "ngeo_migrate_schema", "ngeo_api_inventory", "ngeo_api_lifecycle",
  "ngeo_compatibility_matrix", "ngeo_conformance_manifest"
)
api_contracted <- !any(removed_api %in% exports)
core_api_retained <- all(c(
  "ngeo_surface", "ngeo_volume", "ngeo_point",
  "ngeo_support_map", "ngeo_transform_graph",
  "read_ngeo_cifti", "write_ngeo_cifti"
) %in% exports)

corpus <- neurogeo:::.ngeo_conformance_manifest(version = "3.0")
corpus_verified <- identical(corpus$corpus_version, "3.0") &&
  identical(corpus$schema, "NGCS-conformance-corpus-2") &&
  length(corpus$specifications) == 16L &&
  length(corpus$fixtures) == 2L

gates <- c(
  definitions_complete,
  obsolete_schemas_absent,
  deterministic_manifest,
  manifest_roundtrip,
  corruption_rejected,
  adversarial_deterministic,
  api_contracted,
  core_api_retained,
  corpus_verified
)
if (!all(gates)) {
  stop("One or more NGCS schema architecture gates failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(utils::packageVersion("neurogeo")),
  validation = "passed",
  schema_definitions = list(
    schemas = nrow(definitions),
    all_invariants_declared = TRUE,
    required_schema_ids = required_schema_ids,
    obsolete_schemas_absent = obsolete_schemas_absent
  ),
  generic_validation = list(
    valid_object_passed = TRUE,
    adversarial_error_deterministic = adversarial_deterministic,
    issue_class = class(first_error)[[1L]]
  ),
  portable_manifest = list(
    canonical_hash = manifest_a$canonical_sha256,
    deterministic = deterministic_manifest,
    atomic_roundtrip = manifest_roundtrip,
    corruption_rejected = corruption_rejected
  ),
  public_api = list(
    exports = length(exports),
    implementation_only_api_removed = api_contracted,
    core_api_retained = core_api_retained
  ),
  corpus = list(
    version = corpus$corpus_version,
    specifications = length(corpus$specifications),
    fixtures = length(corpus$fixtures),
    verified = corpus_verified
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
