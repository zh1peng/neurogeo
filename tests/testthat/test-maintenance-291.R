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

test_that("installed format inventory matches reviewed 6.0 capabilities", {
  formats_path <- system.file(
    "spec", "supported-formats.md", package = "neurogeo"
  )
  formats <- paste(readLines(formats_path, warn = FALSE), collapse = "\n")

  expect_match(
    formats, "Status: reviewed for neurogeo 6.0.0 audit candidate", fixed = TRUE
  )
  expect_match(formats, "pure-R CIFTI-2 writer", fixed = TRUE)
  expect_match(formats, "NGCS support map schema 2", fixed = TRUE)
  expect_match(formats, "BIDS derivative data + JSON", fixed = TRUE)
  expect_false(grepl(
    "rejects CIFTI writing", formats, fixed = TRUE
  ))
})

test_that("4.3 cartography contracts are installed from canonical sources", {
  resources <- c(
    "API-4.3.md",
    "migration-4.3.md",
    "cortical-cartography-4.3.md"
  )
  for (resource in resources) {
    installed <- system.file("spec", resource, package = "neurogeo")
    source <- testthat::test_path(
      "..", "..", "inst", "spec", resource
    )
    expect_true(nzchar(installed), info = resource)
    expect_true(file.exists(installed), info = resource)
    if (file.exists(source)) {
      expect_identical(
        readLines(installed, warn = FALSE),
        readLines(source, warn = FALSE),
        info = resource
      )
    }
  }
  contract <- paste(
    readLines(
      system.file(
        "spec", "cortical-cartography-4.3.md",
        package = "neurogeo"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(contract, "atlas-independent", fixed = TRUE)
  expect_match(contract, "MUST NOT invent a cut", fixed = TRUE)
  expect_match(contract, "is_metric_flattening = FALSE", fixed = TRUE)
  expect_match(contract, "seam-crossing", fixed = TRUE)
})

test_that("4.4 maintenance contracts and source policy are installed", {
  resources <- c(
    "README.md",
    "API-4.4.1.md",
    "migration-4.4.1.md",
    "API-4.4.2.md",
    "migration-4.4.2.md"
  )
  paths <- system.file("spec", resources, package = "neurogeo")
  expect_true(all(nzchar(paths)))
  expect_true(all(file.exists(paths)))
  expect_match(
    paste(readLines(paths[[1L]], warn = FALSE), collapse = "\n"),
    "canonical source",
    fixed = TRUE
  )
})
