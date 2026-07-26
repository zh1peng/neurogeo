test_that("NIfTI golden input preserves affine, frames, indices, and sidecar", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("jsonlite")

  expect_warning(
    x <- read_ngeo_nifti(
      golden_path("tiny.nii.gz"),
      checksum = FALSE
    ),
    class = "ngeo_warning_transform_conflict"
  )
  expected_affine <- diag(c(2, 3, 4, 1))
  expected_affine[1:3, 4] <- c(10, 20, 30)
  expected_qform <- diag(4)
  expected_qform[1:3, 4] <- c(10, 20, 30)

  expect_s3_class(x, "ngeo_volume")
  expect_equal(x$domain$dim, c(2L, 2L, 2L))
  expect_equal(x$domain$affine, expected_affine)
  expect_equal(x$domain$header_transforms$qform, expected_qform)
  expect_equal(x$domain$header_transforms$sform, expected_affine)
  expect_identical(x$domain$header_transforms$active, "sform")
  expect_equal(x$domain$source_voxel_index[1, ], c(0L, 0L, 0L))
  expected_values <- matrix(1:16, nrow = 8)
  colnames(expected_values) <- c("frame_1", "frame_2")
  expect_equal(x$values, expected_values)
  expect_equal(x$maps$source_frame, 0:1)
  expect_equal(x$provenance$bids_sidecar$RepetitionTime, 2)
  expect_equal(ngeo_voxel_volume(x), 24)

  suppressWarnings(
    metadata_only <- read_ngeo_nifti(
      golden_path("tiny.nii.gz"),
      load_data = FALSE,
      checksum = FALSE
    )
  )
  expect_null(metadata_only$values)
  expect_equal(nrow(metadata_only$maps), 2L)
  expect_equal(nrow(metadata_only$domain$elements), 8L)
})

test_that("BIDS spatial entities are parsed without dataset orchestration", {
  entities <- neurogeo:::.ngeo_bids_entities(
    "sub-01_hemi-L_space-fsLR_den-32k_res-2_bold.func.gii"
  )

  expect_identical(
    entities,
    list(space = "fsLR", hemi = "L", den = "32k", res = "2")
  )
})

test_that("GIFTI golden geometry, metric, and labels remain aligned", {
  skip_if_not_installed("gifti")

  x <- read_ngeo_gifti(
    geometry = golden_path("tetra.surf.gii"),
    data = c(metric = golden_path("tetra.shape.gii")),
    labels = c(atlas = golden_path("tetra.label.gii")),
    checksum = FALSE
  )

  expect_s3_class(x, "ngeo_surface")
  expect_equal(nrow(x$domain$elements), 4L)
  expect_equal(nrow(x$domain$faces), 4L)
  expect_equal(as.vector(x$values), c(1.5, 2.5, 3.5, 4.5))
  expect_identical(x$domain$face_source_index_base, 0L)
  expect_equal(x$labels$atlas$values, c(0L, 3937500L, 3937500L, 0L))
  expect_silent(ngeo_validate(x, "strict"))
})

test_that("FreeSurfer golden surface, curv, annot, and MGH read without binaries", {
  skip_if_not_installed("freesurferformats")

  surface <- read_ngeo_freesurfer(
    geometry = golden_path("tetra.surface"),
    data = c(metric = golden_path("tetra.curv")),
    labels = golden_path("tetra.annot"),
    checksum = FALSE
  )
  expect_s3_class(surface, "ngeo_surface")
  expect_equal(as.vector(surface$values), c(1.5, 2.5, 3.5, 4.5))
  expect_identical(surface$domain$face_source_index_base, 0L)
  expect_equal(length(surface$labels$annot$values), 4L)

  volume <- read_ngeo_freesurfer(
    golden_path("tiny.mgh"),
    domain = "volume",
    checksum = FALSE
  )
  expect_s3_class(volume, "ngeo_volume")
  expect_equal(volume$domain$dim, c(2L, 2L, 2L))
  expected_volume_values <- matrix(1:16, nrow = 8)
  colnames(expected_volume_values) <- c("frame_1", "frame_2")
  expect_equal(volume$values, expected_volume_values)
  expect_equal(ngeo_voxel_volume(volume), 24)
})

test_that("unified reader dispatches standard file suffixes", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("gifti")
  skip_if_not_installed("cifti")

  nifti <- suppressWarnings(
    read_ngeo(golden_path("tiny.nii.gz"), checksum = FALSE)
  )
  gifti <- read_ngeo(golden_path("tetra.surf.gii"), checksum = FALSE)
  cifti <- read_ngeo(golden_path("tiny.dscalar.nii"), checksum = FALSE)

  expect_s3_class(nifti, "ngeo_volume")
  expect_s3_class(gifti, "ngeo_surface")
  expect_s3_class(cifti, "ngeo_grayordinates")
})

test_that("CIFTI golden dscalar reconstructs surface-volume ordering", {
  skip_if_not_installed("cifti")

  x <- read_ngeo_cifti(
    golden_path("tiny.dscalar.nii"),
    checksum = FALSE
  )

  expect_s3_class(x, "ngeo_grayordinates")
  expect_equal(
    x$values,
    cbind(effect = 1:6, standard_error = seq(0.1, 0.6, by = 0.1)),
    tolerance = 1e-7
  )
  expect_identical(
    x$domain$elements$component_id,
    c(
      "cortex_left", "cortex_left",
      "cortex_right", "cortex_right",
      "thalamus_left", "thalamus_left"
    )
  )
  expect_equal(
    x$domain$components$thalamus_left$voxel_index,
    matrix(c(0, 0, 0, 1, 1, 1), ncol = 3, byrow = TRUE)
  )
  expect_false(ngeo_capabilities(x)[["surface_topology"]])
  expect_true(ngeo_capabilities(x)[["voxel_affine"]])

  if (requireNamespace("gifti", quietly = TRUE)) {
    attached <- read_ngeo_cifti(
      golden_path("tiny.dscalar.nii"),
      surfaces = c(
        left = golden_path("tetra.surf.gii"),
        right = golden_path("tetra.surf.gii")
      ),
      checksum = FALSE
    )
    expect_true(ngeo_capabilities(attached)[["surface_topology"]])
    expect_true(ngeo_capabilities(attached)[["geodesic"]])
  }

  metadata_only <- read_ngeo_cifti(
    golden_path("tiny.dscalar.nii"),
    load_data = FALSE,
    checksum = FALSE
  )
  expect_null(metadata_only$values)
  expect_equal(nrow(metadata_only$maps), 2L)
})

test_that("CIFTI golden dlabel and dtseries preserve map semantics", {
  skip_if_not_installed("cifti")

  labels <- read_ngeo_cifti(
    golden_path("tiny.dlabel.nii"),
    checksum = FALSE
  )
  expect_equal(as.vector(labels$values), c(0, 1, 1, 0, 1, 0))
  expect_identical(labels$measures$spatial_semantics, "categorical")
  expect_identical(labels$measures$default_aggregation, "mode")
  expect_equal(labels$labels$atlas$table$Key, c(0, 1))

  series <- read_ngeo_cifti(
    golden_path("tiny.dtseries.nii"),
    checksum = FALSE
  )
  expected_series <- matrix(seq_len(18), nrow = 6L)
  colnames(expected_series) <- paste0("map_", 1:3)
  expect_equal(series$values, expected_series)
  expect_equal(series$maps$time, c(0, 0.8, 1.6))
  expect_identical(series$maps$time_unit, rep("SECOND", 3L))
})

test_that("pure-R CIFTI integration preserves brain-model order", {
  skip_if_not_installed("cifti")
  path <- system.file(
    "extdata", "curvature.32k_fs_LR.dscalar.nii",
    package = "cifti"
  )
  skip_if(!nzchar(path), "cifti integration fixture is unavailable")

  x <- read_ngeo_cifti(path, checksum = FALSE)
  expect_s3_class(x, "ngeo_grayordinates")
  expect_equal(nrow(x$values), 59412L)
  expect_equal(ncol(x$values), 1L)
  expect_identical(
    unique(x$domain$elements$structure),
    c("CORTEX_LEFT", "CORTEX_RIGHT")
  )
  expect_false(ngeo_capabilities(x)[["surface_topology"]])
  expect_false(ngeo_capabilities(x)[["geodesic"]])

  expect_error(
    read_ngeo_cifti(
      path,
      surfaces = c(left = golden_path("tetra.surf.gii")),
      checksum = FALSE
    ),
    class = "ngeo_error_alignment"
  )
})
