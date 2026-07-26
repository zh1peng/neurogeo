roundtrip_directory <- function() {
  path <- tempfile("neurogeo-roundtrip-")
  dir.create(path)
  path
}

test_that("NIfTI write/read preserves mask, affine, map order, and values", {
  skip_if_not_installed("RNifti")
  directory <- roundtrip_directory()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  mask <- array(
    c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
    dim = c(2, 2, 2)
  )
  values <- array(seq_len(16), dim = c(2, 2, 2, 2))
  affine <- diag(c(2, 3, 4, 1))
  affine[1:3, 4L] <- c(10, 20, 30)
  x <- ngeo_volume(
    values = values,
    dim = c(2, 2, 2),
    affine = affine,
    mask = mask,
    maps = data.frame(name = c("first", "second")),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive", units = "a.u."),
      ngeo_measure(spatial_semantics = "extensive", units = "count")
    )
  )
  manifest <- write_ngeo_nifti(
    x,
    file.path(directory, "roundtrip.nii.gz")
  )
  restored <- read_ngeo_nifti(
    manifest$data,
    mask = manifest$mask,
    maps = x$maps,
    measures = x$measures,
    checksum = FALSE
  )

  expect_equal(restored$domain$voxel_index, x$domain$voxel_index)
  expect_equal(restored$domain$affine, x$domain$affine, tolerance = 1e-6)
  expect_equal(restored$values, x$values)
  expect_identical(restored$maps$name, x$maps$name)
  expect_identical(
    restored$measures$spatial_semantics,
    x$measures$spatial_semantics
  )
})

test_that("GIFTI write/read preserves geometry, maps, labels, and roles", {
  skip_if_not_installed("gifti")
  skip_if_not_installed("freesurferformats")
  directory <- roundtrip_directory()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  x <- read_ngeo_gifti(
    geometry = golden_path("tetra.surf.gii"),
    data = c(metric = golden_path("tetra.shape.gii")),
    labels = c(atlas = golden_path("tetra.label.gii")),
    checksum = FALSE
  )
  x <- ngeo_set_chart(
    x,
    x$domain$coordinates$tetra.surf[, 1:2, drop = FALSE],
    name = "flat"
  )
  manifest <- write_ngeo_gifti(
    x,
    file.path(directory, "roundtrip.surf.gii")
  )
  restored <- read_ngeo_gifti(
    geometry = manifest$geometry,
    data = manifest$data,
    labels = manifest$labels,
    measures = x$measures,
    coordinate_roles = unname(manifest$coordinate_roles),
    checksum = FALSE
  )

  expect_equal(restored$domain$faces, x$domain$faces)
  expect_equal(
    restored$domain$coordinates[[1L]],
    x$domain$coordinates[[1L]],
    tolerance = 1e-6
  )
  expect_equal(restored$values, x$values, tolerance = 1e-6)
  expect_equal(restored$labels$atlas$values, x$labels$atlas$values)
  expect_identical(
    restored$domain$coordinate_meta$role,
    x$domain$coordinate_meta$role
  )
})

test_that("FreeSurfer surface and MGH writers round-trip without binaries", {
  skip_if_not_installed("freesurferformats")
  directory <- roundtrip_directory()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  surface <- read_ngeo_freesurfer(
    geometry = golden_path("tetra.surface"),
    data = c(metric = golden_path("tetra.curv")),
    labels = golden_path("tetra.annot"),
    checksum = FALSE
  )
  surface_manifest <- write_ngeo_freesurfer(
    surface,
    file.path(directory, "roundtrip.surface")
  )
  restored_surface <- read_ngeo_freesurfer(
    geometry = surface_manifest$geometry,
    data = surface_manifest$data,
    labels = surface_manifest$labels[[1L]],
    measures = surface$measures,
    checksum = FALSE
  )

  volume <- read_ngeo_freesurfer(
    golden_path("tiny.mgh"),
    domain = "volume",
    checksum = FALSE
  )
  volume_manifest <- write_ngeo_freesurfer(
    volume,
    file.path(directory, "roundtrip.mgh")
  )
  restored_volume <- read_ngeo_freesurfer(
    volume_manifest$data,
    domain = "volume",
    maps = volume$maps,
    measures = volume$measures,
    checksum = FALSE
  )

  expect_equal(restored_surface$domain$faces, surface$domain$faces)
  expect_equal(restored_surface$values, surface$values, tolerance = 1e-5)
  expect_equal(
    restored_surface$labels$annot$values,
    surface$labels$annot$values
  )
  expect_equal(restored_volume$values, volume$values, tolerance = 1e-5)
  expect_equal(
    restored_volume$domain$affine,
    volume$domain$affine,
    tolerance = 1e-5
  )
})

test_that("provenance export supports path and full redaction", {
  skip_if_not_installed("jsonlite")
  x <- ngeo_points(matrix(c(0, 0, 1, 1), ncol = 2L, byrow = TRUE))
  x$provenance$sources <- list(list(
    source_id = "C:/private/subject-01/data.nii.gz",
    checksum_md5 = "secret-checksum"
  ))
  path_record <- ngeo_export_provenance(x, redact = "paths")
  full_record <- ngeo_export_provenance(x, redact = "all")
  output <- tempfile(fileext = ".json")
  on.exit(unlink(output), add = TRUE)
  ngeo_export_provenance(
    x,
    output,
    redact = "all"
  )
  text <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_identical(
    path_record$provenance$sources[[1L]]$source_id,
    "data.nii.gz"
  )
  expect_identical(
    full_record$provenance$sources[[1L]]$source_id,
    "<redacted>"
  )
  expect_null(full_record$provenance$sources[[1L]]$checksum_md5)
  expect_false(grepl("subject-01|secret-checksum", text))
})
