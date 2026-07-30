test_that("one affine transform updates every supported domain geometry", {
  source_space <- ngeo_space("source", kind = "unknown", units = "mm")
  target_space <- ngeo_space("target", kind = "unknown", units = "mm")
  matrix <- diag(4)
  matrix[1:3, 4] <- c(10, 20, 30)
  transform <- ngeo_transform(
    source_space,
    target_space,
    type = "affine",
    method = "known-test-transform",
    parameters = list(matrix = matrix)
  )

  points <- ngeo_points(
    matrix(c(0, 0, 1, 2), ncol = 2, byrow = TRUE),
    space = source_space
  )
  changed_points <- ngeo_apply_transform(points, transform)
  expect_equal(
    changed_points$domain$coordinates,
    sweep(points$domain$coordinates, 2, c(10, 20), "+")
  )

  surface <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
      ncol = 3,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE),
    space = source_space
  )
  changed_surface <- ngeo_apply_transform(surface, transform)
  expect_equal(
    changed_surface$domain$coordinates$active,
    sweep(surface$domain$coordinates$active, 2, c(10, 20, 30), "+")
  )

  volume <- ngeo_volume(
    dim = c(2, 1, 1),
    affine = diag(4),
    space = source_space
  )
  volume$domain$header_transforms <- list(qform = diag(4))
  changed_volume <- ngeo_apply_transform(volume, transform)
  expect_equal(changed_volume$domain$affine, matrix)
  expect_null(changed_volume$domain$header_transforms)

  regions <- ngeo_regions(
    data.frame(region_id = c("A", "B")),
    centroid = matrix(c(0, 0, 1, 2), ncol = 2, byrow = TRUE),
    space = source_space
  )
  changed_regions <- ngeo_apply_transform(regions, transform)
  expect_equal(
    changed_regions$domain$centroid,
    sweep(regions$domain$centroid, 2, c(10, 20), "+")
  )

  grayordinates <- ngeo_grayordinates(
    list(
      list(
        component_id = "surface",
        kind = "surface",
        structure = "CORTEX_LEFT",
        vertex_index = 0:3,
        surface_vertex_count = 4L,
        geometry = surface
      ),
      list(
        component_id = "volume",
        kind = "volume",
        structure = "THALAMUS_LEFT",
        voxel_index = matrix(c(0, 0, 0), nrow = 1),
        affine = diag(4),
        volume_dim = c(2L, 2L, 2L)
      )
    ),
    space = source_space
  )
  changed_grayordinates <- ngeo_apply_transform(grayordinates, transform)
  expect_equal(
    changed_grayordinates$domain$components[[1L]]$geometry$domain$coordinates$active,
    changed_surface$domain$coordinates$active
  )
  expect_equal(
    changed_grayordinates$domain$components[[2L]]$affine,
    matrix
  )
})

test_that("affine application rejects geometry without an applicable representation", {
  source_space <- ngeo_space("source", kind = "unknown")
  target_space <- ngeo_space("target", kind = "unknown")
  transform <- ngeo_transform(
    source_space,
    target_space,
    type = "affine",
    parameters = list(matrix = diag(4))
  )

  surface <- builder_surface()
  surface$domain$space <- source_space
  surface$domain$coordinate_meta$metric_eligible <- FALSE
  expect_error(
    ngeo_apply_transform(surface, transform),
    class = "ngeo_error_capability"
  )

  regions <- ngeo_regions(
    data.frame(region_id = c("A", "B")),
    space = source_space
  )
  expect_error(
    ngeo_apply_transform(regions, transform),
    class = "ngeo_error_capability"
  )
})
