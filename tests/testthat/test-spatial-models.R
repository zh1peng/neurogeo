test_that("OLS and SLX recover simulated coefficients", {
  fixture <- model_grid()
  ols <- ngeo_spatial_lm(
    fixture$x,
    response = "response",
    predictors = "predictor",
    spatial_weights = fixture$spatial_weights
  )
  slx <- ngeo_spatial_lm(
    fixture$x,
    response = "response",
    predictors = "predictor",
    spatial_weights = fixture$spatial_weights,
    model = "slx"
  )

  expect_s3_class(ols, "ngeo_spatial_lm")
  expect_s3_class(slx, "ngeo_spatial_lm")
  expect_equal(
    stats::setNames(slx$coefficients$estimate, slx$coefficients$term),
    c(`(Intercept)` = 2, predictor = 3, lag_predictor = 1.5),
    tolerance = 0.05
  )
  expect_lt(slx$sigma, ols$sigma)
  expect_true(is.finite(slx$residual_moran))
})

test_that("model weight subsetting uses warning-free sparse coercion", {
  fixture <- model_grid()
  expect_no_warning(
    fit <- ngeo_spatial_lm(
      fixture$x,
      response = "response",
      predictors = "predictor",
      spatial_weights = fixture$spatial_weights,
      model = "slx"
    )
  )
  expect_s3_class(fit, "ngeo_spatial_lm")
})

test_that("spatial models reject mismatched spatial_weights and categorical layers", {
  fixture <- model_grid()
  mismatch <- fixture$spatial_weights
  mismatch$base_hash <- "other"
  expect_error(
    ngeo_spatial_lm(
      fixture$x,
      "response",
      "predictor",
      spatial_weights = mismatch
    ),
    class = "ngeo_error_base_mismatch"
  )
  categorical <- fixture$x
  categorical$measures$support_behavior[[2L]] <- "categorical"
  expect_error(
    ngeo_spatial_lm(categorical, "response", "predictor"),
    class = "ngeo_error_measure"
  )
})

test_that("explicit-bandwidth kernel regression recovers a linear field", {
  coordinates <- cbind(x = 0:9, y = 0)
  predictor <- coordinates[, 1L]
  x <- ngeo_point(
    coordinates,
    values = cbind(
      response = 1 + 2 * predictor,
      predictor = predictor
    )
  )
  result <- ngeo_kernel_regression(
    x,
    response = "response",
    predictors = "predictor",
    bandwidth = 3.1,
    kernel = "bisquare",
    distance_method = "euclidean",
    singular = "error"
  )

  expect_s3_class(result, "ngeo_kernel_regression")
  expect_equal(result$fitted, 1 + 2 * predictor, tolerance = 1e-10)
  expect_equal(result[["(Intercept)"]], rep(1, 10), tolerance = 1e-10)
  expect_equal(result$predictor, rep(2, 10), tolerance = 1e-10)
  expect_identical(attr(result, "distance_method"), "euclidean")
})

test_that("singleton Moran spectral randomizations are reproducible", {
  fixture <- model_grid()
  first <- ngeo_moran_null(
    fixture$x,
    fixture$spatial_weights,
    layer = "response",
    nsim = 8,
    seed = 99,
    zero_policy = TRUE
  )
  second <- ngeo_moran_null(
    fixture$x,
    fixture$spatial_weights,
    layer = "response",
    nsim = 8,
    seed = 99,
    zero_policy = TRUE
  )
  expect_s3_class(first, "ngeo_null")
  expect_identical(first$simulations, second$simulations)
  expect_identical(first$method, "moran_spectral_randomization_singleton")
  expect_identical(first$status, "stable")
  expect_true(first$preserves_spatial_autocorrelation)
})

test_that("simulation streams are reproducible across worker counts", {
  skip_on_cran()
  fixture <- model_grid()
  serial <- ngeo_moran_null(
    fixture$x,
    fixture$spatial_weights,
    layer = "response",
    nsim = 3,
    seed = 123,
    zero_policy = TRUE,
    workers = 1,
    experimental = TRUE
  )
  parallel <- ngeo_moran_null(
    fixture$x,
    fixture$spatial_weights,
    layer = "response",
    nsim = 3,
    seed = 123,
    zero_policy = TRUE,
    workers = 2,
    experimental = TRUE
  )

  expect_identical(parallel$simulations, serial$simulations)
})

test_that("surface spins require and respect spherical registration geometry", {
  skip_if_not_installed("dbscan")
  sphere <- rbind(
    c(1, 1, 1),
    c(1, -1, -1),
    c(-1, 1, -1),
    c(-1, -1, 1)
  ) / sqrt(3)
  x <- ngeo_surface(
    coordinates = list(anatomical = sphere * 10, sphere = sphere),
    faces = matrix(
      c(1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4),
      ncol = 3L,
      byrow = TRUE
    ),
    values = cbind(signal = 1:4),
    coordinate_roles = c("anatomical", "registration")
  )
  first <- ngeo_spin_null(
    x,
    nsim = 6,
    seed = 7,
    coordinates = "sphere",
    experimental = TRUE
  )
  second <- ngeo_spin_null(
    x,
    nsim = 6,
    seed = 7,
    coordinates = "sphere",
    experimental = TRUE
  )

  expect_s3_class(first, "ngeo_null")
  expect_identical(first$simulations, second$simulations)
  expect_true(all(first$mappings >= 1L & first$mappings <= 4L))
  expect_identical(first$base_hash, base_hash(x))
  expect_identical(first$status, "experimental_uncalibrated")
  expect_true(all(first$mapping_diagnostics$cross_stratum == 0L))
})
