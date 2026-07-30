test_that("installed conformance resources are self-contained", {
  skip_if_not_installed("jsonlite")
  manifest_path <- system.file(
    "extdata", "conformance-ngcs29", "manifest.json",
    package = "neurogeo"
  )
  formats_path <- system.file(
    "spec", "supported-formats.md", package = "neurogeo"
  )

  expect_true(nzchar(manifest_path))
  expect_true(file.exists(manifest_path))
  expect_true(nzchar(formats_path))
  expect_true(file.exists(formats_path))
  expect_identical(
    neurogeo:::.ngeo_conformance_manifest()$corpus_version, "3.5"
  )
  expect_identical(
    neurogeo:::.ngeo_conformance_manifest(version = "2.9")$corpus_version,
    "2.9"
  )
})

test_that("installed format inventory matches implemented 2.9 capabilities", {
  formats_path <- system.file(
    "spec", "supported-formats.md", package = "neurogeo"
  )
  formats <- paste(readLines(formats_path, warn = FALSE), collapse = "\n")

  expect_match(
    formats, "Status: reviewed for neurogeo 4.2.0", fixed = TRUE
  )
  expect_match(formats, "pure-R CIFTI-2 writer", fixed = TRUE)
  expect_match(formats, "NGCS support map schema 2", fixed = TRUE)
  expect_match(formats, "BIDS derivative data + JSON", fixed = TRUE)
  expect_false(grepl(
    "rejects CIFTI writing", formats, fixed = TRUE
  ))
})
