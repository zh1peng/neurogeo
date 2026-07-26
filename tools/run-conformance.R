args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "conformance.json")
}

if (!requireNamespace("testthat", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Conformance validation requires testthat and jsonlite.")
}

fixture_dir <- file.path("inst", "extdata", "conformance")
fixtures <- list.files(
  fixture_dir,
  pattern = "\\.json$",
  full.names = TRUE
)
versions <- vapply(
  fixtures,
  function(path) {
    jsonlite::fromJSON(path, simplifyVector = FALSE)$spec_version
  },
  character(1)
)
if (!length(fixtures) || any(versions != "1.0")) {
  stop("All conformance fixtures must declare NGCS 1.0.")
}

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
testthat::test_dir(
  "tests/testthat",
  filter = paste(
    "surface|volume|grayordinates|partition|",
    "topology-distance-weights",
    sep = ""
  ),
  reporter = "summary",
  load_package = "source",
  stop_on_failure = TRUE
)

result <- list(
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  specification = "NGCS 1.0",
  validation = "passed",
  fixtures = lapply(fixtures, function(path) {
    list(
      file = basename(path),
      md5 = unname(tools::md5sum(path)),
      spec_version = "1.0"
    )
  }),
  platform = R.version$platform,
  r_version = R.version.string
)
jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
