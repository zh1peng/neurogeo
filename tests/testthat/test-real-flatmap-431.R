test_that("4.3.1 flatmap fixtures are governed and download-only", {
  path <- system.file(
    "extdata", "reference-4.3.1", "manifest.csv",
    package = "neurogeo"
  )
  expect_true(nzchar(path))
  manifest <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(nrow(manifest), 8L)
  expect_identical(anyDuplicated(manifest$name), 0L)
  expect_true(all(manifest$redistribution == "download-only"))
  expect_true(all(nzchar(manifest$license_record)))
  expect_true(all(nzchar(manifest$terms_url)))
  expect_match(manifest$sha256, "^[0-9a-f]{64}$")
  expect_true(all(as.numeric(manifest$size) > 0))
  expect_false(any(file.exists(file.path(dirname(path), manifest$file))))
})

test_that("4.3.1 flatmap contract and validation tools are shipped", {
  contract <- system.file(
    "spec", "cortical-flatmap-4.3.1.md",
    package = "neurogeo"
  )
  expect_true(nzchar(contract))
  expect_true(file.exists(contract))

  root <- testthat::test_path("..", "..")
  fetch <- file.path(root, "tools", "fetch-reference-431.R")
  validate <- file.path(root, "tools", "run-flatmap-431-validation.R")
  if (file.exists(fetch) && file.exists(validate)) {
    fetch_text <- paste(readLines(fetch, warn = FALSE), collapse = "\n")
    validation_text <- paste(
      readLines(validate, warn = FALSE),
      collapse = "\n"
    )
    expect_match(fetch_text, "sha256", fixed = TRUE)
    expect_match(fetch_text, "download.file", fixed = TRUE)
    expect_match(
      validation_text,
      "topology_verified_flat_surface_binding",
      fixed = TRUE
    )
    expect_match(
      validation_text,
      "external_neuroimaging_binaries",
      fixed = TRUE
    )
  }
})
