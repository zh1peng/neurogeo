test_that("optional 4.6 promotion gates remain closed", {
  x <- ngeo_point(cbind(x = 1:5, y = 0))
  spatial_weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  expect_error(
    ngeo_spatial_basis(x, spatial_weights, operator = "cotangent", n_modes = 2L),
    class = "ngeo_error_capability"
  )
  expect_false("ngeo_spatial_features" %in% getNamespaceExports("neurogeo"))
})
