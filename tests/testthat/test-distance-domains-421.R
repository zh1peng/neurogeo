test_that("Euclidean distance coordinates are explicit for every base", {
  volume <- ngeo_volume(
    dim = c(2, 1, 1),
    affine = diag(4),
    index_base = "zero"
  )
  expect_equal(
    as.numeric(ngeo_distance(volume, 1, 2, distance_method = "world_euclidean")),
    1
  )

  parcellation <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    centroid = matrix(c(0, 0, 3, 4), ncol = 2, byrow = TRUE)
  )
  expect_equal(
    as.numeric(ngeo_distance(parcellation, 1, 2, distance_method = "region_centroid")),
    5
  )

  surface <- builder_surface()
  grayordinate <- ngeo_grayordinate(list(
    list(
      component_id = "surface",
      kind = "surface",
      structure = "CORTEX_LEFT",
      vertex_index = c(0L, 1L),
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
  ))
  distance <- ngeo_distance(
    grayordinate,
    from = 1,
    to = 2,
    distance_method = "edge_geodesic"
  )
  expect_equal(as.numeric(distance), 1)
})

test_that("distance fails when the declared distance_method lacks coordinates", {
  parcellation <- ngeo_parcellation(data.frame(region_id = c("A", "B")))
  expect_error(
    ngeo_distance(parcellation, 1, 2, distance_method = "region_centroid"),
    class = "ngeo_error_capability"
  )

  surface <- builder_surface()
  grayordinate <- ngeo_grayordinate(list(
    list(
      component_id = "surface",
      kind = "surface",
      structure = "CORTEX_LEFT",
      vertex_index = c(0L, 1L),
      surface_vertex_count = 4L,
      geometry = surface
    )
  ))
  grayordinate$base$geometry$components[[1L]]$geometry <- NULL
  expect_error(
    ngeo_distance(grayordinate, 1, 2, distance_method = "edge_geodesic"),
    class = "ngeo_error_capability"
  )

  point <- ngeo_point(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  expect_error(
    ngeo_distance(point, from = NULL),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_distance(point, from = 1, distance_method = c("euclidean", "hops")),
    class = "ngeo_error_metric"
  )
})
