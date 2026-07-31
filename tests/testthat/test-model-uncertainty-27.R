model_uncertainty_fixture <- function() {
  fixture <- model_grid()
  covariance <- ngeo_support_covariance(
    fixture$x,
    variance = rep(0.0025, nrow(fixture$x$domain$elements))
  )
  list(
    x = fixture$x,
    weights = fixture$weights,
    covariance = covariance
  )
}

test_that("uncertain variograms correct measurement error reproducibly", {
  fixture <- model_uncertainty_fixture()
  one <- ngeo_variogram_uncertainty(
    fixture$x,
    "response",
    fixture$covariance,
    breaks = 6L,
    model = "exponential",
    nsim = 20L,
    seed = 2701,
    workers = 1L
  )
  two <- ngeo_variogram_uncertainty(
    fixture$x,
    "response",
    fixture$covariance,
    breaks = 6L,
    model = "exponential",
    nsim = 20L,
    seed = 2701,
    workers = 2L
  )

  expect_s3_class(one, "ngeo_variogram_uncertainty")
  expect_equal(one$parameter_simulations, two$parameter_simulations)
  expect_true(all(one$empirical$semivariance <=
                    one$empirical$raw_semivariance + 1e-12))
  expect_true(all(is.finite(one$parameter_interval)))
})

test_that("kriging uncertainty matches the direct linear covariance", {
  fixture <- model_uncertainty_fixture()
  variogram <- structure(
    list(
      model = "exponential",
      parameters = c(nugget = 0.05, partial_sill = 1, range = 3)
    ),
    class = "ngeo_variogram_fit"
  )
  result <- ngeo_kriging_uncertainty(
    fixture$x,
    "response",
    variogram,
    targets = 1:5,
    neighbors = 8L,
    value_covariance = fixture$covariance,
    support_variance = rep(0.01, 5)
  )
  linear <- attr(
    ngeo_kriging(
      fixture$x, "response", variogram,
      targets = 1:5, neighbors = 8L
    ),
    "linear_weights"
  )
  reference <- rowSums(
    as.matrix(linear)^2 * rep(0.0025, ncol(linear))
  )

  expect_s3_class(result, "ngeo_kriging_uncertainty")
  expect_equal(result$measurement_variance, reference, tolerance = 1e-12)
  expect_equal(
    result$total_variance,
    result$process_variance + result$measurement_variance +
      result$parameter_variance + result$support_variance
  )
})

test_that("GWR reports local coefficient covariance and sensitivity", {
  fixture <- model_uncertainty_fixture()
  result <- ngeo_gwr_uncertainty(
    fixture$x,
    "response",
    "predictor",
    bandwidth = 2.5,
    value_covariance = fixture$covariance,
    bandwidths = c(2, 3),
    kernel = "gaussian",
    targets = 1:6,
    singular = "error"
  )

  expect_s3_class(result, "ngeo_gwr_uncertainty")
  expect_equal(nrow(result$coefficients), 12L)
  expect_true(all(result$coefficients$standard_error >= 0))
  expect_true(all(result$coefficients$lower <=
                    result$coefficients$estimate))
  expect_true(all(result$sensitivity$minimum <=
                    result$sensitivity$maximum))
  expect_match(result$assumptions, "not confidence intervals")
})

test_that("SAR measurement simulations are worker-count invariant", {
  fixture <- model_uncertainty_fixture()
  one <- ngeo_spatial_regression_uncertainty(
    fixture$x,
    "response",
    "predictor",
    fixture$weights,
    model = "sar",
    value_covariance = fixture$covariance,
    nsim = 20L,
    seed = 2702,
    workers = 1L
  )
  two <- ngeo_spatial_regression_uncertainty(
    fixture$x,
    "response",
    "predictor",
    fixture$weights,
    model = "sar",
    value_covariance = fixture$covariance,
    nsim = 20L,
    seed = 2702,
    workers = 2L
  )

  expect_s3_class(one, "ngeo_spatial_regression_uncertainty")
  expect_equal(one$coefficient_simulations, two$coefficient_simulations)
  expect_true(all(one$coefficient_summary$standard_error >= 0))
  expect_equal(one$successful_simulations, 20L)
})

test_that("proper CAR posterior matches the direct Gaussian reference", {
  fixture <- model_uncertainty_fixture()
  result <- ngeo_car_uncertainty(
    fixture$x,
    "response",
    fixture$weights,
    fixture$covariance,
    type = "proper",
    rho = 0.8,
    precision = 2
  )
  weight <- fixture$weights$matrix
  weight <- (weight + Matrix::t(weight)) / 2
  q <- as.matrix(
    Matrix::Diagonal(x = Matrix::rowSums(abs(weight))) - 0.8 * weight
  )
  observation_precision <- diag(1 / 0.0025, nrow(q))
  posterior <- solve(observation_precision + 2 * q)
  reference <- as.numeric(
    posterior %*% observation_precision %*%
      fixture$x$values[, "response"]
  )

  expect_s3_class(result, "ngeo_car_uncertainty")
  expect_equal(result$map$estimate, reference, tolerance = 1e-10)
  expect_equal(result$posterior_covariance, posterior, tolerance = 1e-10)
  expect_true(all(result$map$standard_error > 0))
})

test_that("CAR uncertainty fails before oversized covariance materialization", {
  fixture <- model_uncertainty_fixture()
  previous <- getOption("neurogeo.max_exact_logdet")
  on.exit(
    options(neurogeo.max_exact_logdet = previous),
    add = TRUE
  )
  options(neurogeo.max_exact_logdet = 10L)

  expect_error(
    ngeo_car_uncertainty(
      fixture$x,
      "response",
      fixture$weights,
      fixture$covariance,
      precision = 1
    ),
    class = "ngeo_error_resource"
  )
})

test_that("support ensembles use total variance and calibration is explicit", {
  fixture <- model_uncertainty_fixture()
  first <- ngeo_spatial_regression(
    fixture$x, "response", "predictor", model = "ols"
  )
  changed <- fixture$x
  changed$values[, "response"] <- changed$values[, "response"] + 0.1
  second <- ngeo_spatial_regression(
    changed, "response", "predictor", model = "ols"
  )
  ensemble <- ngeo_support_model_ensemble(
    list(first = first, second = second)
  )
  calibration <- ngeo_model_calibration(
    truth = c(1, 2, 3),
    estimate = c(1.1, 1.9, 3),
    lower = c(0.5, 1.5, 2.5),
    upper = c(1.5, 2.5, 3.5),
    residual_moran = c(0.1, -0.1)
  )

  expect_s3_class(ensemble, "ngeo_support_model_ensemble")
  expect_equal(
    ensemble$summary$total_variance,
    ensemble$summary$within_support_variance +
      ensemble$summary$between_support_variance
  )
  expect_match(ensemble$claim, "not parcellation-invariant")
  expect_s3_class(calibration, "ngeo_model_calibration")
  expect_equal(calibration$coverage, 1)
  expect_equal(calibration$mean_residual_moran, 0)
})

test_that("model uncertainty rejects covariance from another domain", {
  fixture <- model_uncertainty_fixture()
  other <- ngeo_points(
    cbind(x = 1:4, y = 0),
    values = cbind(response = 1:4)
  )
  wrong <- ngeo_support_covariance(other, variance = rep(1, 4))
  expect_error(
    ngeo_car_uncertainty(
      fixture$x,
      "response",
      fixture$weights,
      wrong,
      precision = 1
    ),
    class = "ngeo_error_domain_mismatch"
  )
})
