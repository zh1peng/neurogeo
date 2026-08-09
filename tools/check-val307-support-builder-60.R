args <- commandArgs(trailingOnly = TRUE)
result_path <- if (length(args)) args[[1L]] else
  file.path("check-output", "val307-support-builder-full-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-307 result checking requires: ", paste(missing, collapse = ", "))
if (!file.exists(result_path)) stop("VAL-307 result does not exist: ", result_path)

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
if (!identical(design_hash, locked_hash)) stop("Phase 3 design hash lock failed.")
design <- jsonlite::fromJSON(design_path, simplifyVector = FALSE)
validation <- Filter(
  function(x) identical(x$id, "VAL-307"), design$validations
)[[1L]]
result <- jsonlite::fromJSON(result_path, simplifyVector = FALSE)

scalar <- function(x) length(x) == 1L && !is.null(x) && !is.na(x)
stopifnot(
  identical(result$schema, "neurogeo/phase3-validation/1"),
  identical(result$validation_id, validation$id),
  identical(result$simulation_id, validation$simulation_id),
  identical(result$design_sha256, design_hash),
  identical(result$package_version, "6.0.0"),
  length(result$dependency_versions) > 0L,
  all(vapply(result$dependency_versions, scalar, logical(1))),
  scalar(result$platform),
  scalar(result$r_version),
  identical(result$run_mode, "full"),
  identical(result$seed_base, validation$seed_base),
  identical(result$query_count_per_geometry_size, 64L),
  isTRUE(result$primary_evidence_eligible),
  identical(result$validation, "passed"),
  isTRUE(result$registered_cell_coverage_complete)
)

geometries <- unlist(validation$factors$geometry, use.names = FALSE)
sizes <- unlist(validation$factors$size, use.names = FALSE)
candidate_faces <- as.character(unlist(
  validation$factors$candidate_faces, use.names = FALSE
))
expected <- expand.grid(
  geometry = geometries,
  size = sizes,
  candidate_faces = candidate_faces,
  stringsAsFactors = FALSE
)
expected_keys <- with(expected, paste(geometry, size, candidate_faces, sep = "|"))
observed_keys <- vapply(result$cells, function(cell) paste(
  cell$geometry, cell$size, cell$candidate_faces, sep = "|"
), character(1))
stopifnot(
  length(result$cells) == nrow(expected),
  identical(result$registered_cell_count, nrow(expected)),
  identical(result$observed_cell_count, nrow(expected)),
  !anyDuplicated(observed_keys),
  setequal(observed_keys, expected_keys)
)

for (cell in result$cells) {
  metrics <- c(
    cell$candidate_miss_rate,
    cell$maximum_weight_error,
    cell$coverage_error,
    cell$simplex_error
  )
  stopifnot(
    identical(cell$seed, validation$seed_base + as.integer(cell$size) +
      match(cell$geometry, geometries)),
    all(vapply(metrics, scalar, logical(1))),
    all(is.finite(as.numeric(metrics))),
    cell$candidate_miss_rate <= 1e-4,
    cell$maximum_weight_error <= 1e-10,
    cell$coverage_error <= 1e-10,
    cell$simplex_error <= 1e-12,
    isTRUE(cell$pass)
  )
  expected_engine <- if (identical(cell$geometry, "volume-partial-mask")) {
    "exact_axis_aligned_overlap"
  } else if (identical(as.character(cell$candidate_faces), "exact")) {
    "exact_all_faces"
  } else {
    "dbscan_candidate_faces"
  }
  if (!identical(cell$search_engine, expected_engine)) {
    stop("Unexpected search engine in cell ", observed_keys[[match(
      paste(cell$geometry, cell$size, cell$candidate_faces, sep = "|"),
      observed_keys
    )]], ".")
  }
}

cat(
  "VAL-307 full result:", length(result$cells),
  "registered cells passed against design", design_hash, "\n"
)
