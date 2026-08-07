test_that("one affine transform updates every supported base geometry", {
  source_space <- ngeo_coordinate_space("source", kind = "unknown", unit = "mm")
  target_space <- ngeo_coordinate_space("target", kind = "unknown", unit = "mm")
  matrix <- diag(4)
  matrix[1:3, 4] <- c(10, 20, 30)
  transform <- ngeo_transform(
    source_space,
    target_space,
    type = "affine",
    method = "known-test-transform",
    parameters = list(matrix = matrix)
  )

  point <- ngeo_point(
    matrix(c(0, 0, 1, 2), ncol = 2, byrow = TRUE),
    coordinate_space = source_space
  )
  changed_points <- ngeo_apply_transform(point, transform)
  expect_equal(
    changed_points$base$geometry$coordinates,
    sweep(point$base$geometry$coordinates, 2, c(10, 20), "+")
  )

  surface <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
      ncol = 3,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE),
    coordinate_space = source_space
  )
  changed_surface <- ngeo_apply_transform(surface, transform)
  expect_equal(
    changed_surface$base$geometry$coordinates$active,
    sweep(surface$base$geometry$coordinates$active, 2, c(10, 20, 30), "+")
  )

  volume <- ngeo_volume(
    dim = c(2, 1, 1),
    affine = diag(4),
    coordinate_space = source_space
  )
  volume$base$geometry$header_transforms <- list(qform = diag(4))
  changed_volume <- ngeo_apply_transform(volume, transform)
  expect_equal(changed_volume$base$geometry$affine, matrix)
  expect_null(changed_volume$base$geometry$header_transforms)

  parcellation <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    centroid = matrix(c(0, 0, 1, 2), ncol = 2, byrow = TRUE),
    coordinate_space = source_space
  )
  changed_regions <- ngeo_apply_transform(parcellation, transform)
  expect_equal(
    changed_regions$base$geometry$centroid,
    sweep(parcellation$base$geometry$centroid, 2, c(10, 20), "+")
  )

  grayordinate <- ngeo_grayordinate(
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
    coordinate_space = source_space
  )
  changed_grayordinates <- ngeo_apply_transform(grayordinate, transform)
  expect_equal(
    changed_grayordinates$base$geometry$components[[1L]]$geometry$base$geometry$coordinates$active,
    changed_surface$base$geometry$coordinates$active
  )
  expect_equal(
    changed_grayordinates$base$geometry$components[[2L]]$affine,
    matrix
  )
})

test_that("affine application rejects geometry without an applicable representation", {
  source_space <- ngeo_coordinate_space("source", kind = "unknown")
  target_space <- ngeo_coordinate_space("target", kind = "unknown")
  transform <- ngeo_transform(
    source_space,
    target_space,
    type = "affine",
    parameters = list(matrix = diag(4))
  )

  surface <- builder_surface()
  surface$base$coordinate_space <- source_space
  surface$base$geometry$coordinate_meta$metric_eligible <- FALSE
  expect_error(
    ngeo_apply_transform(surface, transform),
    class = "ngeo_error_capability"
  )

  parcellation <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    coordinate_space = source_space
  )
  expect_error(
    ngeo_apply_transform(parcellation, transform),
    class = "ngeo_error_capability"
  )
})
