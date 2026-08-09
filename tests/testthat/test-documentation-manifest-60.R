test_that("documentation manifest covers current sources and route identities", {
  manifest_path <- test_path(
    "..", "..", "inst", "spec", "documentation-manifest-6.0.csv"
  )
  skip_if_not(file.exists(manifest_path), "repository-only documentation audit")
  manifest <- read.csv(
    manifest_path,
    stringsAsFactors = FALSE
  )
  expect_identical(anyDuplicated(manifest$source), 0L)
  expect_identical(anyDuplicated(manifest$route), 0L)
  expect_true(all(file.exists(test_path("..", "..", manifest$source))))
  expect_true(all(nzchar(manifest$source_sha256)))
  expect_true(all(nzchar(manifest$route_sha256)))
  code_rows <- nzchar(manifest$code_source)
  expect_true(all(file.exists(test_path("..", "..", manifest$code_source[code_rows]))))
  expect_true(all(nzchar(manifest$code_sha256[code_rows])))
  expect_true(all(manifest$counterpart_route[
    manifest$translation_status == "paired"
  ] %in% manifest$route))
})

test_that("installed teaching corpus is licensed and hash-pinned", {
  manifest_path <- system.file(
    "extdata", "tutorial-fixtures-6.0.csv",
    package = "neurogeo", mustWork = TRUE
  )
  corpus <- read.csv(manifest_path, stringsAsFactors = FALSE)
  expect_setequal(
    corpus$workflow,
    c("quickstart", "volume", "surface", "grayordinate", "ROI/cohort")
  )
  expect_true(all(corpus$license == "CC0-1.0"))
  expect_true(all(corpus$redistribution == "bundled"))
  installed_paths <- system.file(
    "extdata", "golden", basename(corpus$path),
    package = "neurogeo", mustWork = TRUE
  )
  observed <- vapply(installed_paths, function(path) {
    digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
  }, character(1))
  expect_identical(unname(observed), corpus$sha256)
})
