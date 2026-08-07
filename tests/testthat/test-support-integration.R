test_that("GIFTI surface geometry builds an auditable identity map", {
  skip_if_not_installed("gifti")
  surface <- read_ngeo_gifti(
    golden_path("tetra.surf.gii"),
    data = golden_path("tetra.shape.gii"),
    measures = ngeo_measure(support_behavior = "intensive"),
    coordinate_space = ngeo_coordinate_space("golden-gifti", kind = "surface"),
    checksum = FALSE
  )
  map <- ngeo_surface_registration_map(
    surface,
    surface,
    method = "barycentric"
  )
  changed <- aggregate_to(surface, surface, map)

  expect_equal(changed$values, surface$values)
  expect_true(ngeo_support_diagnostics(map)$conservative)
  expect_match(
    map$history$operations[[1L]]$operation,
    "surface_barycentric"
  )
})

test_that("NIfTI values can define an explicit aligned segmentation", {
  skip_if_not_installed("RNifti")
  expect_warning(
    volume <- read_ngeo_nifti(
      golden_path("tiny.nii.gz"),
      measures = rbind(
        ngeo_measure(support_behavior = "intensive"),
        ngeo_measure(support_behavior = "intensive")
      ),
      checksum = FALSE
    ),
    "qform and sform differ"
  )
  values <- volume$values[, 1L]
  labels <- ifelse(
    values <= stats::median(values),
    "low",
    "high"
  )
  map <- ngeo_label_overlap_map(volume, labels)
  changed <- aggregate_to(volume, map$target, map)

  expect_s3_class(changed, "ngeo_parcellation")
  expect_equal(nrow(changed$values), 2L)
  expect_identical(map$coverage, "complete")
  expect_true(ngeo_support_diagnostics(map)$conservative)
})

test_that("CIFTI grayordinate components build a hybrid atlas map", {
  skip_if_not_installed("cifti")
  skip_if_not_installed("gifti")
  grayordinate <- read_ngeo_cifti(
    golden_path("tiny.dscalar.nii"),
    surfaces = c(
      left = golden_path("tetra.surf.gii"),
      right = golden_path("tetra.surf.gii")
    ),
    measures = rbind(
      ngeo_measure(support_behavior = "intensive"),
      ngeo_measure(support_behavior = "intensive")
    ),
    checksum = FALSE
  )
  map <- ngeo_atlas_map(
    grayordinate,
    grayordinate$base$elements$component_id
  )
  changed <- aggregate_to(
    grayordinate,
    map$target,
    map
  )

  expect_s3_class(changed, "ngeo_parcellation")
  expect_equal(
    nrow(changed$values),
    length(unique(grayordinate$base$elements$component_id))
  )
  expect_identical(map$coverage, "complete")
  expect_true(all(is.finite(map$source_support)))
})
