test_that("file-backed NIfTI matches loaded values and selections", {
  skip_if_not_installed("RNifti")
  path <- golden_path("tiny.nii.gz")
  loaded <- suppressWarnings(
    read_ngeo_nifti(path, checksum = FALSE)
  )
  backed <- read_ngeo_nifti_filebacked(path, checksum = FALSE)
  selected <- read_ngeo_nifti_filebacked(
    path,
    frames = 2L,
    elements = c(1L, 4L, 8L),
    checksum = FALSE
  )

  expect_s3_class(backed$values, "ngeo_file_values")
  expect_s3_class(backed$values, "ngeo_delayed_values")
  expect_equal(as.matrix(backed$values), loaded$values)
  expect_equal(
    as.matrix(selected$values),
    loaded$values[c(1L, 4L, 8L), 2L, drop = FALSE]
  )
  reordered <- read_ngeo_nifti_filebacked(
    path,
    elements = rev(seq_len(nrow(loaded$values))),
    checksum = FALSE
  )
  expect_equal(
    as.matrix(reordered$values),
    loaded$values[rev(seq_len(nrow(loaded$values))), , drop = FALSE]
  )
  expect_identical(
    selected$history$file_backed$selected_layers, 2L
  )
  expect_true(nzchar(ngeo_file_values_identity(backed$values)))
  manifest <- ngeo_object_manifest(backed$values)
  expect_identical(manifest$object_schema, "ngcs/file-values")
  expect_identical(manifest$specification, "NGCS 6.0")
  expect_true(ngeo_validate_manifest(manifest, backed$values)$valid)
})

test_that("file-backed compressed NIfTI streams row chunks", {
  skip_if_not_installed("RNifti")
  path <- golden_path("tiny.nii.gz")
  backed <- read_ngeo_nifti_filebacked(path, checksum = FALSE)
  chunks <- ngeo_value_chunks(
    backed,
    chunk_size = 3L,
    FUN = function(block, rows) list(block = block, rows = rows)
  )
  reconstructed <- do.call(rbind, lapply(chunks, `[[`, "block"))

  expect_equal(reconstructed, as.matrix(backed$values))
  expect_identical(
    unlist(lapply(chunks, `[[`, "rows"), use.names = FALSE),
    seq_len(nrow(backed$values))
  )

  constrained <- read_ngeo_nifti_filebacked(
    path,
    checksum = FALSE,
    budget = ngeo_resource_budget(
      memory_bytes = 16,
      materialized_elements = 2
    )
  )
  expect_error(
    constrained$values[1:3, 1L, drop = FALSE],
    class = "ngeo_error_resource"
  )
})

test_that("file-backed CIFTI matches loaded layers and brain structures", {
  skip_if_not_installed("cifti")
  path <- golden_path("tiny.dscalar.nii")
  loaded <- read_ngeo_cifti(path, checksum = FALSE)
  metadata <- read_ngeo_cifti(
    path, load_data = FALSE, checksum = FALSE
  )
  backed <- read_ngeo_cifti_filebacked(path, checksum = FALSE)
  selected <- read_ngeo_cifti_filebacked(
    path,
    frames = 2L,
    structures = "CORTEX_LEFT",
    checksum = FALSE
  )

  expect_null(metadata$values)
  expect_equal(as.matrix(backed$values), loaded$values)
  expect_equal(
    as.matrix(selected$values),
    loaded$values[1:2, 2L, drop = FALSE]
  )
  expect_true(all(
    selected$base$elements$structure == "CORTEX_LEFT"
  ))
})

test_that("file-backed MGH and MGZ match backend values", {
  skip_if_not_installed("freesurferformats")
  path <- golden_path("tiny.mgh")
  loaded <- read_ngeo_freesurfer(
    path, base = "volume", checksum = FALSE
  )
  backed <- read_ngeo_mgh_filebacked(path, checksum = FALSE)

  expect_equal(as.matrix(backed$values), loaded$values)

  data <- freesurferformats::read.fs.mgh(path)
  mgz <- tempfile(fileext = ".mgz")
  freesurferformats::write.fs.mgh(
    mgz,
    data,
    vox2ras_matrix = loaded$base$geometry$affine
  )
  compressed <- read_ngeo_mgh_filebacked(
    mgz, checksum = FALSE
  )
  expect_equal(as.matrix(compressed$values), loaded$values)
})

test_that("file mutation invalidates reads and identities", {
  skip_if_not_installed("RNifti")
  path <- tempfile(fileext = ".nii.gz")
  file.copy(golden_path("tiny.nii.gz"), path)
  backed <- read_ngeo_nifti_filebacked(
    path, checksum = FALSE, verify = "checksum"
  )
  connection <- file(path, "ab")
  writeBin(charToRaw("mutation"), connection)
  close(connection)

  expect_error(
    ngeo_validate_file_values(backed$values),
    class = "ngeo_error_file_mutation"
  )
  expect_error(
    backed$values[1L, 1L],
    class = "ngeo_error_file_mutation"
  )
})

test_that("complete file-backed sources copy atomically in chunks", {
  skip_if_not_installed("RNifti")
  path <- golden_path("tiny.nii.gz")
  backed <- read_ngeo_filebacked(path, checksum = FALSE)
  output_path <- tempfile(fileext = ".nii.gz")
  output <- write_ngeo_filebacked(
    backed, output_path, chunk_bytes = 17L
  )
  copied <- read_ngeo_nifti_filebacked(
    output_path, checksum = FALSE
  )

  expect_s3_class(output, "ngeo_atomic_output")
  expect_identical(
    digest::digest(
      path, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    digest::digest(
      output_path, algo = "sha256", file = TRUE, serialize = FALSE
    )
  )
  expect_equal(as.matrix(copied$values), as.matrix(backed$values))

  partial <- read_ngeo_nifti_filebacked(
    path, frames = 1L, checksum = FALSE
  )
  expect_error(
    write_ngeo_filebacked(
      partial, tempfile(fileext = ".nii.gz")
    ),
    class = "ngeo_error_partial_selection"
  )
})

test_that("file-backed identity binds source metadata and map names", {
  first <- tempfile()
  second <- tempfile()
  writeBin(as.raw(c(1, 2)), first)
  writeBin(as.raw(c(9, 8)), second)
  selection <- list(element_index = 0:1, layer_index = 0L)
  binary <- list(
    what = "raw", bytes = 1L, signed = FALSE,
    endian = "little", data_offset = 0, compressed = FALSE
  )
  first_values <- ngeo_file_values(
    first, c(2, 1), "map_a", "test",
    selection, binary, verify = "metadata"
  )
  second_values <- ngeo_file_values(
    second, c(2, 1), "map_a", "test",
    selection, binary, verify = "metadata"
  )
  renamed <- ngeo_file_values(
    first, c(2, 1), "map_b", "test",
    selection, binary, verify = "metadata"
  )

  expect_false(identical(
    ngeo_file_values_identity(first_values),
    ngeo_file_values_identity(second_values)
  ))
  expect_false(identical(
    ngeo_file_values_identity(first_values),
    ngeo_file_values_identity(renamed)
  ))
})
