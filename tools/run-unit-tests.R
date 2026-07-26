if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}

arguments <- commandArgs(trailingOnly = TRUE)

testthat::test_package(
  "neurogeo",
  reporter = "summary",
  filter = if (length(arguments)) arguments[[1L]] else NULL,
  stop_on_failure = TRUE
)
