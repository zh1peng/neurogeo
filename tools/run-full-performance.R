args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("check-output", "full-performance.json")
}

if (!requireNamespace("testthat", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("Full performance validation requires testthat, jsonlite, and digest.")
}

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
Sys.setenv(
  NOT_CRAN = "true",
  NEUROGEO_FULL_PERF = "true"
)
started <- Sys.time()
timing <- system.time(
  testthat::test_dir(
    "tests/testthat",
    filter = "performance-full",
    reporter = "summary",
    load_package = "source",
    stop_on_failure = TRUE
  )
)

result <- list(
  schema = "neurogeo/core-performance/1",
  validation_id = "VAL-308-partial",
  design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  started_at_utc = format(
    started,
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  validation = "passed",
  full_val308_evidence = FALSE,
  elapsed_seconds = unname(timing[["elapsed"]]),
  cases = list(
    surface_164k = list(
      elements = 164025L,
      topology_seconds_limit = 90,
      statistic_seconds_limit = 10,
      sparse_matrix_mib_limit = 100
    ),
    grayordinates_91k = list(
      elements = 91592L,
      topology_seconds_limit = 90,
      sparse_matrix_mib_limit = 60,
      cross_component_edges = 0L
    ),
    surface_diagnostic_32k = list(
      elements = 32400L,
      elapsed_seconds_limit = 30,
      output_mib_limit = 10,
      complex_geometry_per_vertex = FALSE
    ),
    coordinate_knn_100k = list(
      elements = 100000L,
      k = 4L,
      elapsed_seconds_limit = 30,
      sparse_matrix_mib_limit = 15
    ),
    support_change_100k_by_1k = list(
      source_elements = 100000L,
      target_elements = 1000L,
      nonzero = 100000L,
      elapsed_seconds_limit = 30,
      sparse_matrix_mib_limit = 10,
      extensive_conservation = TRUE
    ),
    affine_support_builder_100k = list(
      source_elements = 100000L,
      target_elements = 26010L,
      nonzero = 100000L,
      elapsed_seconds_limit = 30,
      sparse_matrix_mib_limit = 10,
      diagnostics_sparse = TRUE,
      conservative = TRUE
    ),
    uncertain_support_100k = list(
      source_elements = 100000L,
      target_elements = 1000L,
      uncertain_nonzero = 100000L,
      elapsed_seconds_limit = 30,
      sparse_matrix_mib_limit = 10,
      analytic_diagonal_covariance = TRUE
    )
  ),
  evidence_boundary = paste(
    "These seven tests establish bounded sparse core workflows only.",
    "They do not instantiate the frozen 36-cell VAL-308 factorial, report",
    "per-cell observed metrics, or satisfy its release gate."
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
