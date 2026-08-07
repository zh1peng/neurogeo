test_that("surface fixture satisfies indexing and area invariants", {
  fixture <- read_fixture("surface-tetrahedron.json")
  coordinates <- rows_to_matrix(fixture$coordinates)
  faces <- rows_to_matrix(fixture$faces, "integer")
  values <- unlist(fixture$values, use.names = FALSE)

  x <- ngeo_surface(
    coordinates = coordinates,
    faces = faces,
    values = values,
    coordinate_space = ngeo_coordinate_space("fixture_surface", kind = "surface"),
    index_base = "zero"
  )

  expect_s3_class(x, "ngeo_surface")
  expect_silent(ngeo_validate(x, "strict"))
  expect_identical(base_type(x), "surface")
  expect_equal(nrow(x$base$elements), fixture$expected$n_vertex)
  expect_equal(nrow(x$base$geometry$faces), fixture$expected$n_face)
  expect_identical(x$base$geometry$face_source_index_base, 0L)
  expect_equal(x$base$geometry$faces, faces + 1L)
  expect_equal(as.vector(x$values), values)
  expect_equal(
    ngeo_vertex_area(x),
    unlist(fixture$expected$barycentric_vertex_area),
    tolerance = fixture$tolerance$absolute
  )
  expect_true(all(ngeo_capabilities(x)[c(
    "coordinates_3d", "surface_topology", "surface_area", "geodesic"
  )]))
})

test_that("surface alignment and index errors fail early", {
  coordinates <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), nrow = 1)

  expect_error(
    ngeo_surface(coordinates, faces, values = 1:2),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_surface(coordinates, matrix(c(1, 2, 4), nrow = 1)),
    class = "ngeo_error_index"
  )
  expect_error(
    ngeo_surface(coordinates, matrix(c(1, 1, 2), nrow = 1)),
    class = "ngeo_error_geometry"
  )
})

test_that("non-anatomical coordinates cannot silently define area", {
  coordinates <- matrix(c(0, 0, 1, 0, 0, 1), ncol = 2, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), nrow = 1)
  x <- ngeo_surface(
    coordinates,
    faces,
    coordinate_roles = "chart"
  )

  expect_error(ngeo_vertex_area(x), class = "ngeo_error_metric")
})

