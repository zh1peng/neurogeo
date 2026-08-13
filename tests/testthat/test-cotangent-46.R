test_that("6.1 promotes cotangent basis but not a generic feature facade", {
  x <- ngeo_point(cbind(x = 1:5, y = 0))
  spatial_weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  expect_error(
    ngeo_spatial_basis(x, spatial_weights, operator = "cotangent", n_modes = 2L),
    class = "ngeo_error_argument"
  )
  surface <- builder_surface()
  basis <- ngeo_spatial_basis(surface, operator = "cotangent", n_modes = 2L)
  expect_s3_class(basis, "ngeo_spatial_basis")
  expect_identical(basis$operator, "cotangent")
  expect_false("ngeo_spatial_features" %in% getNamespaceExports("neurogeo"))
})
