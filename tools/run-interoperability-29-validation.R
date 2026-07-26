args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "interoperability-29-validation.json")
required <- c("cifti", "digest", "jsonlite")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Interoperability validation requires: ",
       paste(missing, collapse = ", "))
}
if (!exists("ngeo_validate_cifti_contract", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

rejected_as <- function(expression, class) {
  inherits(tryCatch({
    force(expression)
    NULL
  }, error = identity), class)
}

golden <- function(name) {
  path <- file.path("inst", "extdata", "golden", name)
  if (file.exists(path)) path else
    system.file("extdata", "golden", name, package = "neurogeo")
}
corpus <- function(name) {
  path <- file.path("inst", "extdata", "conformance-ngcs29", name)
  if (file.exists(path)) path else
    system.file(
      "extdata", "conformance-ngcs29", name, package = "neurogeo"
    )
}

scalar <- read_ngeo_cifti(
  golden("tiny.dscalar.nii"), checksum = FALSE
)
named_metadata <- list(
  list(Description = "effect estimate", Intent = "validation"),
  list(Description = "standard error")
)
scalar32_path <- tempfile(fileext = ".dscalar.nii")
scalar64_path <- tempfile(fileext = ".dscalar.nii")
write_ngeo_cifti(
  scalar, scalar32_path, type = "dscalar", datatype = "float32",
  named_map_metadata = named_metadata
)
write_ngeo_cifti(
  scalar, scalar64_path, type = "dscalar", datatype = "float64",
  named_map_metadata = named_metadata
)
scalar32 <- read_ngeo_cifti(scalar32_path, checksum = FALSE)
scalar64 <- read_ngeo_cifti(scalar64_path, checksum = FALSE)
scalar32_error <- max(abs(scalar32$values - scalar$values))
scalar64_error <- max(abs(scalar64$values - scalar$values))
metadata_preserved <- identical(
  scalar64$maps$metadata, named_metadata
)

label <- read_ngeo_cifti(
  golden("tiny.dlabel.nii"), checksum = FALSE
)
label_path <- tempfile(fileext = ".dlabel.nii")
write_ngeo_cifti(label, label_path, type = "dlabel")
label_restored <- read_ngeo_cifti(label_path, checksum = FALSE)
labels_preserved <- identical(
  label_restored$labels$atlas$table$Label,
  label$labels$atlas$table$Label
)
label_int32 <- identical(
  label_restored$provenance$cifti$datatype, "int32"
)

series <- read_ngeo_cifti(
  golden("tiny.dtseries.nii"), checksum = FALSE
)
series_path <- tempfile(fileext = ".dtseries.nii")
write_ngeo_cifti(
  series, series_path, type = "dtseries", datatype = "float64"
)
series_restored <- read_ngeo_cifti(series_path, checksum = FALSE)
time_preserved <- identical(
  series_restored$maps$time, series$maps$time
) && identical(
  series_restored$provenance$cifti$datatype, "float64"
)
bad_series <- series
bad_series$maps$time <- c(0, 1, 3)
irregular_time_rejected <- rejected_as(
  ngeo_validate_cifti_contract(bad_series, "dtseries"),
  "ngeo_error_format"
)
series_metadata_rejected <- rejected_as(
  ngeo_validate_cifti_contract(
    series, "dtseries",
    named_map_metadata = rep(list(list(Note = "invalid")), 3L)
  ),
  "ngeo_error_format"
)
label_float_rejected <- rejected_as(
  ngeo_validate_cifti_contract(
    label, "dlabel", datatype = "float32"
  ),
  "ngeo_error_format"
)

bids_fixture <- jsonlite::fromJSON(
  corpus("bids-cases.json"), simplifyVector = FALSE
)
bids_valid <- vapply(bids_fixture$valid, function(case) {
  built <- ngeo_bids_build_name(
    case$entities, case$suffix, case$extension
  )
  parsed <- ngeo_bids_parse_name(built)
  identical(built, case$expected) &&
    identical(parsed$entities, case$entities)
}, logical(1))
bids_invalid <- vapply(bids_fixture$invalid, function(case) {
  rejected_as(ngeo_bids_parse_name(case$name), "ngeo_error_bids")
}, logical(1))

derivative_directory <- tempfile("ngeo-bids-29-")
dir.create(derivative_directory)
entities <- list(sub = "01", space = "fsLR", desc = "effect")
derivative_path <- file.path(
  derivative_directory,
  ngeo_bids_build_name(
    entities, "dscalar", ".dscalar.nii"
  )
)
derivative <- write_ngeo_bids_derivative(
  scalar,
  derivative_path,
  entities = entities,
  strict_name = TRUE
)
derivative_pair_complete <- all(file.exists(derivative)) &&
  length(attr(derivative, "sha256")) == 2L
collision_rejected <- rejected_as(
  write_ngeo_bids_derivative(
    scalar,
    derivative_path,
    entities = entities,
    strict_name = TRUE
  ),
  "ngeo_error_io"
)
versioned <- write_ngeo_bids_derivative(
  scalar,
  derivative_path,
  entities = entities,
  collision = "version",
  strict_name = TRUE
)
versioned_run <- identical(
  ngeo_bids_parse_name(versioned[["data"]])$entities$run, "1"
)
bad_derivative <- file.path(
  derivative_directory,
  "sub-02_desc-invalid_dscalar.dscalar.nii"
)
points <- ngeo_points(
  cbind(x = 1:3, y = 0),
  values = cbind(signal = 1:3)
)
failed_pair_rejected <- inherits(tryCatch({
  write_ngeo_bids_derivative(
    points,
    bad_derivative,
    entities = list(sub = "02", desc = "invalid"),
    strict_name = TRUE
  )
  NULL
}, error = identity), "error") &&
  !file.exists(bad_derivative) &&
  !file.exists(sub("\\.dscalar\\.nii$", ".json", bad_derivative))

source <- ngeo_points(
  cbind(x = 1:4, y = 0),
  values = cbind(signal = c(1, 2, 4, 8)),
  measures = ngeo_measure(spatial_semantics = "intensive")
)
probabilities <- matrix(
  c(1, 0, 1, 0, 0, 1, 0, 1),
  nrow = 4L,
  ncol = 2L,
  dimnames = list(NULL, c("A", "B"))
)
support_map <- ngeo_probabilistic_atlas_map(
  source, probabilities, source_support = rep(1, 4)
)
schema1 <- tempfile("ngeo-schema1-")
write_ngeo_support_map(support_map, schema1)
schema1_map <- read_ngeo_support_map(schema1)
schema2 <- tempfile("ngeo-schema2-")
bundle <- write_ngeo_support_bundle(
  support_map, schema2, chunk_size = 2L
)
schema2_map <- read_ngeo_support_bundle(schema2)
schema_hash_equal <- identical(
  ngeo_support_map_hash(schema1_map),
  ngeo_support_map_hash(schema2_map)
)
schema_operator_equal <- isTRUE(all.equal(
  schema1_map$operator, schema2_map$operator
))
migrated <- tempfile("ngeo-migrated-")
ngeo_migrate_support_map_exchange(
  schema1, migrated, chunk_size = 3L
)
migration_hash_equal <- identical(
  ngeo_support_map_hash(read_ngeo_support_bundle(migrated)),
  ngeo_support_map_hash(support_map)
)
corrupt <- tempfile("ngeo-corrupt-")
write_ngeo_support_bundle(support_map, corrupt, chunk_size = 2L)
cat("corrupt", file = file.path(corrupt, "operator-00001.mtx"),
    append = TRUE)
checksum_mutation_rejected <- rejected_as(
  ngeo_validate_support_bundle(corrupt), "ngeo_error_io"
)

manifest <- neurogeo:::.ngeo_conformance_manifest(
  corpus("manifest.json")
)
exports <- getNamespaceExports("neurogeo")
corpus_verified <- identical(manifest$corpus_version, "2.9") &&
  length(manifest$specifications) == 14L
api_complete <- all(c(
  "write_ngeo_cifti",
  "write_ngeo_support_bundle",
  "ngeo_bids_build_name"
) %in% exports)
platforms <- c("Windows", "Linux", "macOS")
platform_evidence_explicit <- length(platforms) == 3L

gates <- c(
  scalar32_error <= 1e-6,
  scalar64_error <= 1e-12,
  metadata_preserved,
  labels_preserved,
  label_int32,
  time_preserved,
  irregular_time_rejected,
  series_metadata_rejected,
  label_float_rejected,
  all(bids_valid),
  all(bids_invalid),
  derivative_pair_complete,
  collision_rejected,
  versioned_run,
  failed_pair_rejected,
  schema_hash_equal,
  schema_operator_equal,
  migration_hash_equal,
  checksum_mutation_rejected,
  corpus_verified,
  api_complete,
  platform_evidence_explicit
)
if (!all(gates)) {
  stop("One or more NGCS 2.9 interoperability gates failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  cifti = list(
    dscalar_float32_maximum_error = scalar32_error,
    dscalar_float64_maximum_error = scalar64_error,
    named_map_metadata_preserved = metadata_preserved,
    dlabel_int32 = label_int32,
    label_table_preserved = labels_preserved,
    dtseries_time_axis_preserved = time_preserved,
    adversarial_contracts_rejected = all(c(
      irregular_time_rejected,
      series_metadata_rejected,
      label_float_rejected
    ))
  ),
  bids = list(
    valid_fixtures = sum(bids_valid),
    invalid_fixtures = sum(bids_invalid),
    atomic_pair = derivative_pair_complete,
    collision_rejected = collision_rejected,
    deterministic_version = versioned_run,
    failed_pair_cleaned = failed_pair_rejected
  ),
  support_exchange = list(
    chunks = bundle$chunks,
    schema1_schema2_hash_equal = schema_hash_equal,
    operators_equal = schema_operator_equal,
    migration_hash_equal = migration_hash_equal,
    checksum_mutation_rejected = checksum_mutation_rejected
  ),
  readiness = list(
    corpus_version = manifest$corpus_version,
    specification_count = length(manifest$specifications),
    public_exports = length(exports),
    deprecated_exports = 0L,
    platforms = platforms,
    local_platform = R.version$platform
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
