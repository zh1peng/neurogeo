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
    layers = data.frame(name = c("first", "second")),
    measures = rbind(
      ngeo_measure(support_behavior = "intensive", unit = "a.u."),
      ngeo_measure(support_behavior = "extensive", unit = "count")
    )
  )
  manifest <- write_ngeo_nifti(
    x,
    file.path(directory, "roundtrip.nii.gz")
  )
  restored <- read_ngeo_nifti(
    manifest$data,
    mask = manifest$mask,
    layers = x$layers,
    measures = x$measures,
    checksum = FALSE
  )

  expect_equal(restored$base$geometry$voxel_index, x$base$geometry$voxel_index)
  expect_equal(restored$base$geometry$affine, x$base$geometry$affine, tolerance = 1e-6)
  expect_equal(restored$values, x$values)
  expect_identical(restored$layers$name, x$layers$name)
  expect_identical(
    restored$measures$support_behavior,
    x$measures$support_behavior
  )
})

test_that("GIFTI write/read preserves geometry, layers, labels, and roles", {
  skip_if_not_installed("gifti")
  skip_if_not_installed("freesurferformats")
  directory <- roundtrip_directory()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  x <- read_ngeo_gifti(
    geometry = golden_path("tetra.surf.gii"),
    data = c(distance_method = golden_path("tetra.shape.gii")),
    labels = c(atlas = golden_path("tetra.label.gii")),
    checksum = FALSE
  )
  x <- ngeo_set_chart(
    x,
    x$base$geometry$coordinates$tetra.surf[, 1:2, drop = FALSE],
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

  expect_equal(restored$base$geometry$faces, x$base$geometry$faces)
  expect_equal(
    restored$base$geometry$coordinates[[1L]],
    x$base$geometry$coordinates[[1L]],
    tolerance = 1e-6
  )
  expect_equal(restored$values, x$values, tolerance = 1e-6)
  expect_equal(restored$base$labels$atlas$values, x$base$labels$atlas$values)
  expect_identical(
    restored$base$geometry$coordinate_meta$role,
    x$base$geometry$coordinate_meta$role
  )
})

test_that("FreeSurfer surface and MGH writers round-trip without binaries", {
  skip_if_not_installed("freesurferformats")
  directory <- roundtrip_directory()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  surface <- read_ngeo_freesurfer(
    geometry = golden_path("tetra.surface"),
    data = c(distance_method = golden_path("tetra.curv")),
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
    base = "volume",
    checksum = FALSE
  )
  volume_manifest <- write_ngeo_freesurfer(
    volume,
    file.path(directory, "roundtrip.mgh")
  )
  restored_volume <- read_ngeo_freesurfer(
    volume_manifest$data,
    base = "volume",
    layers = volume$layers,
    measures = volume$measures,
    checksum = FALSE
  )

  expect_equal(restored_surface$base$geometry$faces, surface$base$geometry$faces)
  expect_equal(restored_surface$values, surface$values, tolerance = 1e-5)
  expect_equal(
    restored_surface$base$labels$annot$values,
    surface$base$labels$annot$values
  )
  expect_equal(restored_volume$values, volume$values, tolerance = 1e-5)
  expect_equal(
    restored_volume$base$geometry$affine,
    volume$base$geometry$affine,
    tolerance = 1e-5
  )
})

test_that("history export supports path and full redaction", {
  skip_if_not_installed("jsonlite")
  x <- ngeo_point(matrix(c(0, 0, 1, 1), ncol = 2L, byrow = TRUE))
  x$history$sources <- list(list(
    source_id = "C:/private/subject-01/data.nii.gz",
    checksum_md5 = "secret-checksum"
  ))
  path_record <- ngeo_export_history(x, redact = "paths")
  full_record <- ngeo_export_history(x, redact = "all")
  output <- tempfile(fileext = ".json")
  on.exit(unlink(output), add = TRUE)
  ngeo_export_history(
    x,
    output,
    redact = "all"
  )
  text <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_identical(
    path_record$history$sources[[1L]]$source_id,
    "data.nii.gz"
  )
  expect_identical(
    full_record$history$sources[[1L]]$source_id,
    "<redacted>"
  )
  expect_null(full_record$history$sources[[1L]]$checksum_md5)
  expect_false(grepl("subject-01|secret-checksum", text))
})
