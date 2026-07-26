test_that("points preserve aligned metadata and expose coordinate capabilities", {
  coordinates <- matrix(
    c(0, 0, 0, 1, 2, 3, 4, 5, 6),
    ncol = 3,
    byrow = TRUE
  )
  x <- ngeo_points(
    coordinates,
    values = cbind(a = 1:3, b = 4:6),
    structure = c("LEFT", "LEFT", "RIGHT"),
    uncertainty = c(0, 0.5, 1),
    index_base = "zero"
  )

  expect_s3_class(x, "ngeo_points")
  expect_silent(ngeo_validate(x, "strict"))
  expect_identical(ngeo_domain_type(x), "points")
  expect_equal(ngeo_elements(x)$source_index, 0:2)
  expect_true(ngeo_capabilities(x)[["coordinates_3d"]])
  expect_false(ngeo_capabilities(x)[["adjacency"]])
})

test_that("grayordinates are one ordered domain with degradable capabilities", {
  components <- list(
    list(
      component_id = "left",
      kind = "surface",
      structure = "CORTEX_LEFT",
      vertex_index = c(0L, 2L),
      surface_vertex_count = 4L,
      source_index_base = 0L
    ),
    list(
      component_id = "right",
      kind = "surface",
      structure = "CORTEX_RIGHT",
      vertex_index = c(1L, 3L),
      surface_vertex_count = 4L,
      source_index_base = 0L
    ),
    list(
      component_id = "subcortex",
      kind = "volume",
      structure = "THALAMUS_LEFT",
      voxel_index = matrix(c(0, 0, 0, 1, 1, 1), ncol = 3, byrow = TRUE),
      affine = diag(c(2, 2, 2, 1)),
      source_index_base = 0L
    )
  )
  x <- ngeo_grayordinates(components, values = 1:6)

  expect_s3_class(x, "ngeo_grayordinates")
  expect_silent(ngeo_validate(x, "strict"))
  expect_equal(nrow(ngeo_elements(x)), 6L)
  expect_identical(
    ngeo_elements(x)$component_id,
    c("left", "left", "right", "right", "subcortex", "subcortex")
  )
  capabilities <- ngeo_capabilities(x)
  expect_false(capabilities[["surface_topology"]])
  expect_false(capabilities[["geodesic"]])
  expect_true(capabilities[["voxel_affine"]])
})

test_that("attached grayordinate surfaces enable surface capabilities", {
  coordinates <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ),
    ncol = 3,
    byrow = TRUE
  )
  geometry <- ngeo_surface(
    coordinates,
    matrix(c(1, 2, 3, 1, 2, 4), ncol = 3, byrow = TRUE)
  )
  component <- list(
    component_id = "left",
    kind = "surface",
    structure = "CORTEX_LEFT",
    vertex_index = c(0L, 2L),
    surface_vertex_count = 4L,
    source_index_base = 0L,
    geometry = geometry
  )
  x <- ngeo_grayordinates(list(component), values = c(1, 2))

  expect_true(ngeo_capabilities(x)[["surface_topology"]])
  expect_true(ngeo_capabilities(x)[["surface_area"]])
  expect_true(ngeo_capabilities(x)[["geodesic"]])
})

test_that("regions support standalone and membership-backed representations", {
  points <- ngeo_points(matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE))
  regions <- data.frame(
    region_id = c("A", "B"),
    name = c("anterior", "posterior")
  )
  x <- ngeo_regions(
    regions,
    values = c(10, 20),
    membership = c("A", "A", "B"),
    base_domain = points,
    centroid = matrix(c(0.5, 0.5, 2, 2), ncol = 2, byrow = TRUE),
    support_size = c(2, 1),
    adjacency = matrix(c(0, 1, 1, 0), nrow = 2)
  )

  expect_s3_class(x, "ngeo_regions")
  expect_silent(ngeo_validate(x, "strict"))
  expect_identical(x$domain$base_domain_hash, ngeo_domain_hash(points))
  expect_true(ngeo_capabilities(x)[["partition"]])
  expect_true(ngeo_capabilities(x)[["adjacency"]])
  expect_true(ngeo_capabilities(x)[["coordinates_2d"]])
})

test_that("measurement and transform objects encode explicit semantics", {
  measure <- ngeo_measure(
    spatial_semantics = "intensive",
    units = "mm"
  )
  transform <- ngeo_transform(
    ngeo_space("native", kind = "surface"),
    ngeo_space("template", kind = "surface"),
    type = "spherical_registration",
    method = "fixture"
  )

  expect_identical(measure$default_aggregation, "support_weighted_mean")
  expect_s3_class(transform, "ngeo_transform")
  expect_identical(transform$direction, "source_to_target")
})

test_that("known affine transforms compose, invert, and preserve alignment", {
  source <- ngeo_space("source", kind = "unknown")
  middle <- ngeo_space("middle", kind = "unknown")
  target <- ngeo_space("target", kind = "unknown")
  translate <- diag(4)
  translate[1:3, 4L] <- c(10, -2, 3)
  scale <- diag(c(2, 2, 2, 1))
  first <- ngeo_transform(
    source,
    middle,
    type = "affine",
    method = "known translation",
    parameters = list(matrix = translate)
  )
  second <- ngeo_transform(
    middle,
    target,
    type = "affine",
    method = "known scale",
    parameters = list(matrix = scale)
  )
  composed <- ngeo_compose_transform(first, second)
  inverse <- ngeo_invert_transform(composed)
  x <- ngeo_points(
    matrix(c(0, 0, 0, 1, 2, 3), ncol = 3L, byrow = TRUE),
    values = cbind(signal = c(4, 5)),
    space = source
  )

  transformed <- ngeo_apply_transform(x, composed)
  recovered <- ngeo_apply_transform(transformed, inverse)

  expect_equal(
    transformed$domain$coordinates,
    .ngeo_affine_coordinates(x$domain$coordinates, scale %*% translate)
  )
  expect_equal(recovered$domain$coordinates, x$domain$coordinates)
  expect_identical(transformed$values, x$values)
  expect_identical(
    transformed$domain$elements$element_id,
    x$domain$elements$element_id
  )
  expect_false(identical(ngeo_domain_hash(transformed), ngeo_domain_hash(x)))
})

test_that("known transforms reject mismatched spaces and unknown types", {
  source <- ngeo_space("source", kind = "unknown")
  target <- ngeo_space("target", kind = "unknown")
  affine <- ngeo_transform(
    source,
    target,
    type = "affine",
    parameters = list(matrix = diag(4))
  )
  x <- ngeo_points(matrix(c(0, 0, 1, 1), ncol = 2L, byrow = TRUE))

  expect_error(
    ngeo_apply_transform(x, affine),
    class = "ngeo_error_space_mismatch"
  )
  recorded <- ngeo_transform(
    source,
    target,
    type = "spherical_registration"
  )
  expect_error(
    ngeo_validate_transform(recorded),
    class = "ngeo_error_transform_type"
  )
})

test_that("sparse values remain sparse and aligned", {
  skip_if_not_installed("Matrix")
  values <- Matrix::Matrix(matrix(1:6, nrow = 3), sparse = TRUE)
  x <- ngeo_points(
    matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE),
    values = values
  )

  expect_s4_class(ngeo_values(x), "dgCMatrix")
  expect_equal(dim(ngeo_values(x)), c(3L, 2L))
})
