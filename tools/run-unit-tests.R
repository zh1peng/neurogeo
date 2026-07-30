if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}

arguments <- commandArgs(trailingOnly = TRUE)

filter <- if (length(arguments)) arguments[[1L]] else NULL
if (file.exists("DESCRIPTION") && dir.exists(file.path("tests", "testthat"))) {
  testthat::test_local(
    ".",
    reporter = "summary",
    filter = filter,
    stop_on_failure = TRUE
  )
} else {
  testthat::test_package(
    "neurogeo",
    reporter = "summary",
    filter = filter,
    stop_on_failure = TRUE
  )
}
