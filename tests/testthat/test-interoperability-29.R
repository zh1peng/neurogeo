test_that("CIFTI NamedMap metadata and datatypes round-trip", {
  skip_if_not_installed("cifti")
  scalar <- read_ngeo_cifti(
    golden_path("tiny.dscalar.nii"),
    checksum = FALSE
  )
  metadata <- list(
    list(Description = "effect estimate", Intent = "test"),
    list(Description = "standard error")
  )
  scalar_path <- tempfile(fileext = ".dscalar.nii")
  manifest <- write_ngeo(
    scalar,
    scalar_path,
    datatype = "float64",
    named_map_metadata = metadata
  )
  scalar_restored <- read_ngeo_cifti(
    scalar_path, checksum = FALSE
  )

  expect_equal(scalar_restored$values, scalar$values, tolerance = 1e-12)
  expect_identical(scalar_restored$layers$metadata, metadata)
  expect_identical(
    scalar_restored$history$cifti$datatype,
    "float64"
  )
  expect_identical(manifest$format, "cifti")
  expect_identical(
    manifest$data,
    normalizePath(scalar_path, winslash = "/")
  )

  label <- read_ngeo_cifti(
    golden_path("tiny.dlabel.nii"),
    checksum = FALSE
  )
  label_path <- tempfile(fileext = ".dlabel.nii")
  label_manifest <- write_ngeo(label, label_path)
  label_restored <- read_ngeo_cifti(label_path, checksum = FALSE)
  expect_identical(label_manifest$format, "cifti")
  expect_identical(label_restored$history$cifti$datatype, "int32")
  expect_identical(
    label_restored$base$labels$atlas$table$Label,
    label$base$labels$atlas$table$Label
  )
})

test_that("CIFTI axes and label tables reject unsupported contracts", {
  skip_if_not_installed("cifti")
  series <- read_ngeo_cifti(
    golden_path("tiny.dtseries.nii"),
    checksum = FALSE
  )
  series$layers$time <- c(0, 1, 3)
  expect_error(
    ngeo_validate_cifti_contract(series, "dtseries"),
    class = "ngeo_error_format"
  )
  series$layers$time <- c(0, 1, 2)
  expect_error(
    ngeo_validate_cifti_contract(
      series,
      "dtseries",
      named_map_metadata = rep(list(list(Note = "invalid")), 3)
    ),
    class = "ngeo_error_format"
  )

  label <- read_ngeo_cifti(
    golden_path("tiny.dlabel.nii"),
    checksum = FALSE
  )
  label$base$labels$atlas$table$Red[[1L]] <- 2
  expect_error(
    ngeo_validate_cifti_contract(label, "dlabel"),
    class = "ngeo_error_format"
  )
  expect_error(
    ngeo_validate_cifti_contract(label, "dlabel", datatype = "float32"),
    class = "ngeo_error_format"
  )
})

test_that("BIDS names build and parse canonical entities", {
  skip_if_not_installed("jsonlite")
  fixture <- jsonlite::fromJSON(
    system.file(
      "extdata", "conformance-ngcs29", "bids-cases.json",
      package = "neurogeo"
    ),
    simplifyVector = FALSE
  )
  for (case in fixture$valid) {
    entities <- case$entities
    name <- ngeo_bids_build_name(
      entities, case$suffix, case$extension
    )
    parsed <- ngeo_bids_parse_name(name)
    expect_identical(name, case$expected)
    expect_identical(parsed$entities, entities)
  }
  for (case in fixture$invalid) {
    expect_error(
      ngeo_bids_parse_name(case$name),
      class = "ngeo_error_bids"
    )
  }
})

test_that("BIDS derivative transactions validate collision policies", {
  skip_if_not_installed("cifti")
  skip_if_not_installed("jsonlite")
  x <- read_ngeo_cifti(
    golden_path("tiny.dscalar.nii"),
    checksum = FALSE
  )
  directory <- tempfile("bids-derivative-")
  dir.create(directory)
  entities <- list(
    sub = "01", space = "fsLR", desc = "effect"
  )
  name <- ngeo_bids_build_name(
    entities, "dscalar", ".dscalar.nii"
  )
  path <- file.path(directory, name)
  built_sidecar <- ngeo_bids_sidecar(x, entities = entities)
  expect_silent(ngeo_validate_bids_sidecar(built_sidecar, x))
  first <- write_ngeo_bids_derivative(
    x, path, entities = entities, strict_name = TRUE
  )
  sidecar <- jsonlite::fromJSON(
    first[["sidecar"]], simplifyVector = FALSE
  )

  expect_true(all(file.exists(first)))
  expect_length(attr(first, "sha256"), 2L)
  expect_silent(ngeo_validate_bids_sidecar(sidecar, x, path))
  expect_error(
    write_ngeo_bids_derivative(
      x, path, entities = entities, strict_name = TRUE
    ),
    class = "ngeo_error_io"
  )
  versioned <- write_ngeo_bids_derivative(
    x,
    path,
    entities = entities,
    collision = "version",
    strict_name = TRUE
  )
  parsed <- ngeo_bids_parse_name(versioned[["data"]])
  expect_identical(parsed$entities$run, "1")
  versioned_sidecar <- jsonlite::fromJSON(
    versioned[["sidecar"]], simplifyVector = FALSE
  )
  expect_identical(versioned_sidecar$Entities$run, "1")
})

test_that("failed derivative writes leave no final pair", {
  skip_if_not_installed("cifti")
  x <- ngeo_point(
    cbind(x = 1:3, y = 0),
    values = cbind(signal = 1:3)
  )
  directory <- tempfile("failed-bids-")
  dir.create(directory)
  path <- file.path(
    directory,
    "sub-01_desc-invalid_dscalar.dscalar.nii"
  )
  expect_error(
    write_ngeo_bids_derivative(
      x,
      path,
      entities = list(sub = "01", desc = "invalid"),
      strict_name = TRUE
    )
  )
  expect_false(file.exists(path))
  expect_false(file.exists(sub("\\.dscalar\\.nii$", ".json", path)))
})

test_that("support bundle schema 2 equals schema 1 and monolithic layers", {
  skip_if_not_installed("jsonlite")
  fixture <- diagnostic_fixture()
  schema1 <- tempfile("schema1-")
  write_ngeo_support_map(fixture$soft, schema1)
  from_schema1 <- read_ngeo_support_map(schema1)
  bundle_path <- tempfile("schema2-")
  output <- write_ngeo_support_bundle(
    fixture$soft, bundle_path, chunk_size = 2L
  )
  from_schema2 <- read_ngeo_support_map(bundle_path)

  expect_s3_class(output, "ngeo_support_bundle_output")
  expect_identical(output$chunks, 2L)
  expect_equal(from_schema2$operator, fixture$soft$operator)
  expect_identical(
    ngeo_support_map_hash(from_schema1),
    ngeo_support_map_hash(from_schema2)
  )
  expect_silent(ngeo_validate_support_bundle(bundle_path))

  migrated <- tempfile("migrated-")
  ngeo_migrate_support_map_exchange(
    schema1, migrated, chunk_size = 3L
  )
  expect_identical(
    ngeo_support_map_hash(read_ngeo_support_bundle(migrated)),
    ngeo_support_map_hash(fixture$soft)
  )
})

test_that("support bundle checksums reject mutation", {
  skip_if_not_installed("jsonlite")
  fixture <- diagnostic_fixture()
  path <- tempfile("schema2-corrupt-")
  write_ngeo_support_bundle(fixture$hard, path, chunk_size = 2L)
  chunk <- file.path(path, "operator-00001.mtx")
  write("corrupt", file = chunk, append = TRUE)
  expect_error(
    ngeo_validate_support_bundle(path),
    class = "ngeo_error_io"
  )
})

test_that("language-independent conformance corpus is complete", {
  skip_if_not_installed("jsonlite")
  manifest <- neurogeo:::.ngeo_conformance_manifest(
    system.file(
      "extdata", "conformance-ngcs29", "manifest.json",
      package = "neurogeo"
    )
  )

  expect_identical(manifest$corpus_version, "2.9")
  expect_length(manifest$specifications, 14L)
  expect_length(manifest$specifications, 14L)
})
