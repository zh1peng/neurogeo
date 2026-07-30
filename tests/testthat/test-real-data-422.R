test_that("4.2.2 external fixture manifest is immutable and download-only", {
  path <- system.file(
    "extdata", "reference-4.2.2", "manifest.csv",
    package = "neurogeo"
  )
  expect_true(nzchar(path))
  manifest <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(nrow(manifest), 6L)
  expect_identical(anyDuplicated(manifest$name), 0L)
  expect_true(all(manifest$redistribution == "download-only"))
  expect_true(all(nzchar(manifest$license_record)))
  expect_true(all(nzchar(manifest$terms_url)))
  expect_match(manifest$source_commit, "^[0-9a-f]{40}$")
  expect_match(manifest$sha256, "^[0-9a-f]{64}$")
  expect_true(all(as.numeric(manifest$size) > 0))
  expect_false(any(file.exists(file.path(dirname(path), manifest$file))))
})

test_that("4.2.2 validation contract and tools are installed or shipped", {
  for (name in c(
    "API-4.2.2.md",
    "migration-4.2.2.md",
    "real-data-validation-4.2.2.md"
  )) {
    path <- system.file("spec", name, package = "neurogeo")
    expect_true(nzchar(path), info = name)
    expect_true(file.exists(path), info = name)
  }

  root <- testthat::test_path("..", "..")
  fetch <- file.path(root, "tools", "fetch-reference-422.R")
  validate <- file.path(
    root, "tools", "run-real-data-validation-422.R"
  )
  if (file.exists(fetch) && file.exists(validate)) {
    fetch_text <- paste(readLines(fetch, warn = FALSE), collapse = "\n")
    validation_text <- paste(
      readLines(validate, warn = FALSE),
      collapse = "\n"
    )
    expect_match(fetch_text, "sha256", fixed = TRUE)
    expect_match(fetch_text, "download.file", fixed = TRUE)
    expect_match(validation_text, "ngeo_file_values", fixed = TRUE)
    expect_match(
      validation_text,
      "external_neuroimaging_binaries",
      fixed = TRUE
    )
  }
})
