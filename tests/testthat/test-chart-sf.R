chart_surface <- function() {
  ngeo_surface(
    matrix(
      c(
        0, 0, 0,
        1, 0, 0,
        1, 1, 0,
        0, 1, 0
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE),
    values = cbind(signal = 1:4)
  )
}

test_that("inflated GIFTI names are not mistaken for flat charts", {
  expect_identical(
    neurogeo:::.ngeo_gifti_coordinate_role("subject.L.inflated.surf.gii"),
    "visualization"
  )
  expect_identical(
    neurogeo:::.ngeo_gifti_coordinate_role("subject.L.flat.surf.gii"),
    "chart"
  )
})

test_that("surface charts are auxiliary and non-distance_method", {
  x <- chart_surface()
  original_hash <- base_hash(x)
  original_area <- ngeo_vertex_area(x)
  chart <- matrix(
    c(0, 0, 2, 0, 2, 1, 0, 1),
    ncol = 2L,
    byrow = TRUE
  )
  result <- ngeo_set_chart(
    x,
    chart,
    name = "flat",
    distortion = data.frame(area_scale = c(2, 1, 2, 1)),
    source = "test chart"
  )

  expect_false("flat" %in% names(x$base$geometry$coordinates))
  expect_equal(result$base$geometry$active_coordinates, "active")
  expect_equal(ngeo_vertex_area(result), original_area)
  expect_false(
    result$base$geometry$coordinate_meta$metric_eligible[
      result$base$geometry$coordinate_meta$name == "flat"
    ]
  )
  expect_equal(
    result$base$charts$flat$source_base_hash,
    original_hash
  )
  expect_equal(
    ngeo_chart_distortion(result, "flat")$area_scale,
    c(2, 1, 2, 1)
  )
  expect_silent(ngeo_validate(result, "strict"))
})

test_that("surface charts enforce alignment and unique names", {
  x <- chart_surface()
  expect_error(
    ngeo_set_chart(x, matrix(1:6, ncol = 2L)),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_set_chart(
      x,
      matrix(seq_len(8), ncol = 2L),
      name = "active"
    ),
    class = "ngeo_error_chart"
  )
})

test_that("sf export is explicit and retains NGCS identity", {
  skip_if_not_installed("sf")
  x <- chart_surface()
  x <- ngeo_set_chart(
    x,
    matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1),
      ncol = 2L,
      byrow = TRUE
    ),
    name = "flat"
  )
  vertices <- ngeo_as_sf(x, chart = "flat")
  faces <- ngeo_as_sf(x, feature = "face", chart = "flat")

  expect_s3_class(vertices, "sf")
  expect_equal(nrow(vertices), 4L)
  expect_true("signal" %in% names(vertices))
  expect_equal(attr(vertices, "base_hash"), base_hash(x))
  expect_equal(attr(vertices, "ngeo_chart"), "flat")
  expect_equal(nrow(faces), 2L)
  expect_true(all(sf::st_geometry_type(faces) == "POLYGON"))
  expect_true(is.na(sf::st_crs(vertices)))
})

test_that("sf export refuses implicit surface projection and oversized copies", {
  skip_if_not_installed("sf")
  x <- chart_surface()
  expect_error(
    ngeo_as_sf(x),
    class = "ngeo_error_chart"
  )
  x <- ngeo_set_chart(
    x,
    matrix(seq_len(8), ncol = 2L),
    name = "flat"
  )
  expect_error(
    ngeo_as_sf(x, max_features = 3L),
    class = "ngeo_error_resource"
  )
})

test_that("planar point and region centroids export without claiming a CRS", {
  skip_if_not_installed("sf")
  point <- ngeo_point(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  parcellation <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    centroid = matrix(c(0, 0, 2, 2), ncol = 2, byrow = TRUE)
  )

  expect_s3_class(ngeo_as_sf(point), "sf")
  expect_s3_class(ngeo_as_sf(parcellation), "sf")
})
