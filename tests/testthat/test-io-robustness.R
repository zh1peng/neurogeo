write_invalid_binary <- function(extension) {
  path <- tempfile(fileext = extension)
  writeBin(as.raw(c(0, 1, 2, 3, 255, 254)), path)
  path
}

test_that("malformed NIfTI inputs fail at a classed I/O boundary", {
  skip_if_not_installed("RNifti")
  path <- write_invalid_binary(".nii")
  on.exit(unlink(path), add = TRUE)

  expect_error(
    read_ngeo_nifti(path),
    class = "ngeo_error_io"
  )
})

test_that("malformed NIfTI masks are classed independently", {
  skip_if_not_installed("RNifti")
  image <- golden_path("tiny.nii.gz")
  mask <- write_invalid_binary(".nii.gz")
  on.exit(unlink(mask), add = TRUE)

  expect_error(
    suppressWarnings(
      read_ngeo_nifti(image, mask = mask, checksum = FALSE)
    ),
    class = "ngeo_error_io"
  )
})

test_that("malformed NIfTI JSON sidecars fail explicitly", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("jsonlite")
  directory <- tempfile("ngeo-sidecar-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  image <- file.path(directory, "sub-01_T1w.nii.gz")
  sidecar <- file.path(directory, "sub-01_T1w.json")
  expect_true(file.copy(golden_path("tiny.nii.gz"), image))
  writeLines("{ this is not valid JSON", sidecar)

  expect_error(
    suppressWarnings(read_ngeo_nifti(image, checksum = FALSE)),
    class = "ngeo_error_io"
  )
})

test_that("malformed GIFTI inputs fail at a classed I/O boundary", {
  skip_if_not_installed("gifti")
  path <- write_invalid_binary(".surf.gii")
  on.exit(unlink(path), add = TRUE)

  expect_error(
    read_ngeo_gifti(path, checksum = FALSE),
    class = "ngeo_error_io"
  )
})

test_that("malformed CIFTI inputs fail at a classed I/O boundary", {
  skip_if_not_installed("cifti")
  path <- write_invalid_binary(".dscalar.nii")
  on.exit(unlink(path), add = TRUE)

  expect_error(
    read_ngeo_cifti(path, checksum = FALSE),
    class = "ngeo_error_io"
  )
})

test_that("malformed FreeSurfer inputs fail at a classed I/O boundary", {
  skip_if_not_installed("freesurferformats")
  path <- write_invalid_binary(".surface")
  on.exit(unlink(path), add = TRUE)

  expect_error(
    read_ngeo_freesurfer(path, checksum = FALSE),
    class = "ngeo_error_io"
  )
})

test_that("missing paths remain argument errors rather than backend errors", {
  missing <- tempfile(fileext = ".nii")
  expect_error(
    read_ngeo_nifti(missing),
    class = "ngeo_error_argument"
  )
})
