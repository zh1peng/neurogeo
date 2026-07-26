args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "schema-30-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("NGCS 3.0 validation requires jsonlite and digest.")
}
suppressPackageStartupMessages(library(neurogeo))
if (utils::compareVersion(
  as.character(utils::packageVersion("neurogeo")), "3.0.0"
) < 0L) {
  stop("NGCS 3.0 validation requires neurogeo 3.0.0 or later.")
}

registry <- ngeo_schema_registry()
required_schema_ids <- c(
  "ngcs/ngeo-surface", "ngcs/ngeo-volume", "ngcs/ngeo-points",
  "ngcs/ngeo-grayordinates", "ngcs/ngeo-regions", "ngcs/space",
  "ngcs/transform", "ngcs/weights", "ngcs/partition",
  "ngcs/support-map", "ngcs/block-support-map",
  "ngcs/support-covariance", "ngcs/support-ensemble",
  "ngcs/delayed-values", "ngcs/space-registry",
  "ngcs/transform-graph", "ngcs/execution-plan",
  "ngcs/resource-budget"
)
registry_complete <- all(
  required_schema_ids %in% registry$schemas$schema_id
) && all(lengths(registry$schemas$invariants) > 0L)

space <- ngeo_space("reference-points", source_metadata = list(dimension = 2L))
x <- ngeo_points(
  cbind(x = c(0, 1, 0), y = c(0, 0, 1)),
  values = cbind(signal = c(1, 2, 3)),
  measures = ngeo_measure(spatial_semantics = "intensive"),
  space = space
)
valid_report <- ngeo_validate_schema(x)
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
first_issue <- ngeo_validate_schema(invalid)
second_issue <- ngeo_validate_schema(invalid)
adversarial_deterministic <- !first_issue$valid &&
  identical(first_issue$issues, second_issue$issues) &&
  identical(
    first_issue$issues$condition_class,
    "ngeo_error_alignment"
  )
classed_error <- inherits(tryCatch(
  {
    ngeo_validate_schema(invalid, mode = "error")
    NULL
  },
  error = identity
), "ngeo_error_schema_validation")

migrated <- ngeo_migrate_schema(x)
migration <- attr(migrated, "ngeo_schema_migration")
migration_auditable <- identical(migration$target_version, "3.0") &&
  identical(migration$schema_id, "ngcs/ngeo-points") &&
  isTRUE(migration$valid)

lifecycle <- ngeo_api_lifecycle()
exports <- sort(getNamespaceExports("neurogeo"))
api_complete <- identical(lifecycle$api, exports) &&
  all(lifecycle$lifecycle == "stable") &&
  all(lifecycle$planned_action == "retain") &&
  !any(!is.na(lifecycle$replacement))
old_exports_retained <- all(c(
  "ngeo_surface", "ngeo_volume", "ngeo_points",
  "ngeo_support_map", "ngeo_transform_graph",
  "read_ngeo_cifti", "write_ngeo_cifti"
) %in% lifecycle$api)

corpus <- ngeo_conformance_manifest(version = "3.0")
corpus_verified <- identical(corpus$corpus_version, "3.0") &&
  identical(corpus$schema, "NGCS-conformance-corpus-2") &&
  length(corpus$specifications) == 16L &&
  length(corpus$fixtures) == 2L

gates <- c(
  registry_complete,
  valid_report$valid,
  deterministic_manifest,
  manifest_roundtrip,
  corruption_rejected,
  adversarial_deterministic,
  classed_error,
  migration_auditable,
  api_complete,
  old_exports_retained,
  corpus_verified
)
if (!all(gates)) {
  stop("One or more NGCS 3.0 schema gates failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(utils::packageVersion("neurogeo")),
  validation = "passed",
  registry = list(
    schemas = nrow(registry$schemas),
    all_invariants_declared = TRUE,
    required_schema_ids = required_schema_ids
  ),
  validation_report = list(
    valid_object_passed = valid_report$valid,
    adversarial_issue_deterministic = adversarial_deterministic,
    classed_error = classed_error,
    issue_class = first_issue$issues$condition_class
  ),
  portable_manifest = list(
    canonical_hash = manifest_a$canonical_sha256,
    canonical_json_without_r_serialization = TRUE,
    deterministic = deterministic_manifest,
    atomic_roundtrip = manifest_roundtrip,
    corruption_rejected = corruption_rejected
  ),
  lifecycle = list(
    public_exports = nrow(lifecycle),
    stable_exports = sum(lifecycle$lifecycle == "stable"),
    planned_removals = sum(lifecycle$planned_action != "retain"),
    legacy_exports_retained = old_exports_retained,
    schema_migration_auditable = migration_auditable
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
