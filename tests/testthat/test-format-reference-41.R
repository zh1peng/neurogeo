example_path <- function(name) {
  ngeo_example_data(name)$path[[1L]]
}

reference_surface <- function() {
  geometry <- read_ngeo_gifti(
    example_path("freesurferformats-cube"),
    checksum = TRUE
  )
  coordinates <- geometry$domain$coordinates
  values <- rowSums(coordinates[[geometry$domain$active_coordinates]])
  ngeo_surface(
    coordinates = coordinates,
    faces = geometry$domain$faces,
    values = cbind(curvature = values),
    maps = data.frame(name = "curvature"),
    measures = ngeo_measure(
      value_type = "continuous",
      spatial_semantics = "intensive",
      units = "a.u."
    ),
    space = geometry$domain$space,
    coordinate_roles = geometry$domain$coordinate_meta$role,
    index_base = "one",
    source_index_base = 0L
  )
}

truncate_reference <- function(path, extension) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  output <- tempfile(fileext = extension)
  writeBin(bytes[seq_len(min(64L, length(bytes)))], output)
  output
}

test_that("4.1 example data has auditable and verified provenance", {
  fixtures <- ngeo_example_data()

  expect_equal(nrow(fixtures), 6L)
  expect_true(all(fixtures$verified))
  expect_true(all(file.exists(fixtures$path)))
  expect_setequal(
    fixtures$license,
    c("GPL-2.0-only", "MIT")
  )
  expect_match(fixtures$source_commit, "^[0-9a-f]{40}$")
  expect_match(fixtures$sha256, "^[0-9a-f]{64}$")
  expect_error(
    ngeo_example_data("unknown-fixture"),
    class = "ngeo_error_argument"
  )
})

test_that("upstream NIfTI preserves affine, indices, values, and provenance", {
  skip_if_not_installed("RNifti")
  path <- example_path("rnifti-example")
  x <- suppressWarnings(read_ngeo_nifti(path, checksum = TRUE))

  expect_s3_class(x, "ngeo_volume")
  expect_identical(x$domain$dim, c(96L, 96L, 60L))
  expect_equal(x$domain$source_voxel_index[1L, ], c(0L, 0L, 0L))
  expect_equal(x$domain$source_voxel_index[nrow(x$values), ], c(95L, 95L, 59L))
  expect_equal(diag(x$domain$affine)[1:3], c(-2.5, 2.5, 2.5))
  expect_equal(range(x$values), c(0, 2503))
  expect_identical(x$provenance$sources[[1L]]$importer, "read_ngeo_nifti")
  expect_match(x$provenance$sources[[1L]]$checksum_md5, "^[0-9a-f]{32}$")

  directory <- tempfile("neurogeo-41-nifti-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  written <- write_ngeo_nifti(
    x,
    file.path(directory, "roundtrip.nii.gz")
  )
  restored <- read_ngeo_nifti(
    written$data,
    maps = x$maps,
    measures = x$measures,
    checksum = FALSE
  )
  expect_equal(restored$domain$affine, x$domain$affine, tolerance = 1e-6)
  expect_equal(restored$domain$source_voxel_index, x$domain$source_voxel_index)
  expect_equal(restored$values, x$values)

  truncated <- truncate_reference(path, ".nii.gz")
  expect_error(
    read_ngeo_nifti(truncated, checksum = FALSE),
    class = "ngeo_error_io"
  )
})

test_that("upstream GIFTI geometry supports metric round-trip", {
  skip_if_not_installed("gifti")
  skip_if_not_installed("freesurferformats")
  path <- example_path("freesurferformats-cube")
  geometry <- read_ngeo_gifti(path, checksum = TRUE)

  expect_s3_class(geometry, "ngeo_surface")
  expect_equal(nrow(geometry$domain$elements), 8L)
  expect_equal(nrow(geometry$domain$faces), 12L)
  expect_identical(geometry$domain$face_source_index_base, 0L)
  expect_identical(
    geometry$provenance$sources[[1L]]$importer,
    "read_ngeo_gifti"
  )

  surface <- reference_surface()
  directory <- tempfile("neurogeo-41-gifti-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  written <- write_ngeo_gifti(
    surface,
    file.path(directory, "cube.surf.gii")
  )
  restored <- read_ngeo_gifti(
    geometry = written$geometry,
    data = written$data,
    measures = surface$measures,
    checksum = FALSE
  )
  expect_equal(restored$domain$faces, surface$domain$faces)
  expect_equal(
    restored$domain$coordinates[[1L]],
    surface$domain$coordinates[[1L]],
    tolerance = 1e-6
  )
  expect_equal(restored$values, surface$values, tolerance = 1e-6)

  truncated <- truncate_reference(path, ".surf.gii")
  expect_error(
    read_ngeo_gifti(truncated, checksum = FALSE),
    class = "ngeo_error_io"
  )
})

test_that("upstream CIFTI dscalar preserves brain-model order and round-trips", {
  skip_if_not_installed("cifti")
  path <- example_path("cifti-curvature-fslr32k")
  x <- read_ngeo_cifti(path, checksum = TRUE)

  expect_s3_class(x, "ngeo_grayordinates")
  expect_equal(dim(x$values), c(59412L, 1L))
  expect_identical(
    unique(x$domain$elements$structure),
    c("CORTEX_LEFT", "CORTEX_RIGHT")
  )
  expect_equal(head(x$domain$components$cortex_left$vertex_index), 0:5)
  expect_identical(x$maps$name, "100307_Curvature")
  expect_identical(x$provenance$sources[[1L]]$importer, "read_ngeo_cifti")

  output <- tempfile(fileext = ".dscalar.nii")
  write_ngeo_cifti(x, output, type = "dscalar")
  restored <- read_ngeo_cifti(output, checksum = FALSE)
  expect_equal(restored$values, x$values, tolerance = 1e-6)
  expect_identical(
    restored$domain$elements$structure,
    x$domain$elements$structure
  )
  expect_identical(
    restored$domain$components$cortex_left$vertex_index,
    x$domain$components$cortex_left$vertex_index
  )

  truncated <- truncate_reference(path, ".dscalar.nii")
  expect_error(
    read_ngeo_cifti(truncated, checksum = FALSE),
    class = "ngeo_error_io"
  )
})

test_that("FreeSurfer reference failures are explicit and writers recover", {
  skip_if_not_installed("freesurferformats")
  surface_path <- example_path("freesurfer-tiny-surface")
  mgh_path <- example_path("freesurfer-tiny-mgh")

  expect_error(
    read_ngeo_freesurfer(surface_path, checksum = FALSE),
    class = "ngeo_error_geometry"
  )
  expect_error(
    read_ngeo_freesurfer(
      mgh_path,
      domain = "volume",
      checksum = FALSE
    ),
    class = "ngeo_error_transform"
  )

  volume <- read_ngeo_freesurfer(
    mgh_path,
    domain = "volume",
    affine = diag(4),
    checksum = TRUE
  )
  expect_identical(volume$domain$dim, c(3L, 3L, 3L))
  expect_equal(range(volume$values), c(1, 9))
  expect_equal(volume$domain$affine, diag(4))
  expect_identical(
    volume$provenance$sources[[1L]]$importer,
    "read_ngeo_freesurfer_volume"
  )

  directory <- tempfile("neurogeo-41-freesurfer-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  volume_written <- write_ngeo_freesurfer(
    volume,
    file.path(directory, "volume.mgh")
  )
  volume_restored <- read_ngeo_freesurfer(
    volume_written$data,
    domain = "volume",
    checksum = FALSE
  )
  expect_equal(volume_restored$values, volume$values, tolerance = 1e-5)
  expect_equal(
    volume_restored$domain$affine,
    volume$domain$affine,
    tolerance = 1e-5
  )

  surface <- reference_surface()
  surface_written <- write_ngeo_freesurfer(
    surface,
    file.path(directory, "cube.surface")
  )
  surface_restored <- read_ngeo_freesurfer(
    geometry = surface_written$geometry,
    data = surface_written$data,
    measures = surface$measures,
    checksum = FALSE
  )
  expect_equal(surface_restored$domain$faces, surface$domain$faces)
  expect_equal(surface_restored$values, surface$values, tolerance = 1e-5)
})
