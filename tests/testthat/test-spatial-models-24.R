test_that("bounded WLS variogram fitting recovers a direct curve", {
  distance <- seq(0.5, 4, length.out = 8)
  empirical <- data.frame(
    bin = seq_along(distance),
    distance = distance,
    semivariance = neurogeo:::.ngeo_variogram_curve(
      distance, "exponential", 0.2, 1.5, 2
    ),
    n_pairs = rep(100L, length(distance))
  )
  class(empirical) <- c("ngeo_variogram", "data.frame")
  fit <- ngeo_fit_variogram(
    empirical,
    model = "exponential",
    start = c(nugget = 0.1, partial_sill = 1, range = 1)
  )

  expect_s3_class(fit, "ngeo_variogram_fit")
  expect_equal(
    fit$parameters,
    c(nugget = 0.2, partial_sill = 1.5, range = 2),
    tolerance = 0.02
  )
})

test_that("local ordinary kriging is bounded and reports variance", {
  coordinates <- cbind(x = 0:7, y = 0)
  x <- ngeo_points(
    coordinates,
    values = cbind(signal = sin(coordinates[, 1L] / 2)),
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  fit <- structure(
    list(
      model = "exponential",
      parameters = c(nugget = 0.05, partial_sill = 1, range = 3)
    ),
    class = "ngeo_variogram_fit"
  )
  result <- ngeo_kriging(
    x, "signal", fit,
    targets = matrix(
      c(2.5, 0, 0, 4.5, 0, 0),
      ncol = 3L,
      byrow = TRUE
    ),
    neighbors = 4L
  )

  expect_s3_class(result, "ngeo_kriging")
  expect_true(all(is.finite(result$prediction)))
  expect_true(all(result$variance >= 0))
  expect_identical(result$neighbors, c(4L, 4L))
})

test_that("GWR bandwidth selection and local fits recover a linear field", {
  coordinates <- cbind(x = 0:11, y = 0)
  predictor <- coordinates[, 1L]
  x <- ngeo_points(
    coordinates,
    values = cbind(
      response = 1 + 2 * predictor,
      predictor = predictor
    )
  )
  bandwidth <- ngeo_gwr_bandwidth(
    x, "response", "predictor",
    candidates = c(2.1, 4.1, 8.1),
    kernel = "bisquare"
  )
  fit <- ngeo_gwr(
    x, "response", "predictor", bandwidth,
    kernel = "bisquare", singular = "error"
  )

  expect_s3_class(bandwidth, "ngeo_gwr_bandwidth")
  expect_true(is.finite(bandwidth$bandwidth))
  expect_s3_class(fit, "ngeo_gwr")
  expect_equal(fit$predictor, rep(2, 12), tolerance = 1e-8)
  expect_true(all(is.finite(fit$condition_number)))
})

test_that("SAR and SEM likelihoods recover small direct simulations", {
  fixture <- model_grid()
  weight <- as.matrix(fixture$weights$matrix)
  predictor <- fixture$x$values[, "predictor"]
  design_mean <- 1 + 2 * predictor
  set.seed(2401)
  sar_response <- as.numeric(
    solve(diag(nrow(weight)) - 0.35 * weight,
          design_mean + stats::rnorm(nrow(weight), sd = 0.03))
  )
  fixture$x$values[, "response"] <- sar_response
  sar <- ngeo_spatial_regression(
    fixture$x, "response", "predictor",
    fixture$weights, model = "sar"
  )

  set.seed(2402)
  sem_response <- design_mean + as.numeric(
    solve(diag(nrow(weight)) - 0.4 * weight,
          stats::rnorm(nrow(weight), sd = 0.05))
  )
  fixture$x$values[, "response"] <- sem_response
  sem <- ngeo_spatial_regression(
    fixture$x, "response", "predictor",
    fixture$weights, model = "sem"
  )

  expect_equal(sar$spatial_parameter, 0.35, tolerance = 0.12)
  expect_equal(sem$spatial_parameter, 0.4, tolerance = 0.45)
  expect_identical(sar$log_determinant_method, "exact_dense")
  expect_true(all(is.finite(c(sar$logLik, sem$logLik))))
})

test_that("CAR smoothing records constraints and reduces graph roughness", {
  fixture <- model_grid()
  result <- ngeo_car(
    fixture$x,
    "response",
    fixture$weights,
    type = "intrinsic",
    precision = 1
  )
  weight <- fixture$weights$matrix
  observed_roughness <- sum(
    (fixture$x$values[, "response"] -
       as.numeric(weight %*% fixture$x$values[, "response"]))^2
  )
  fitted_roughness <- sum(
    (result$fitted - as.numeric(weight %*% result$fitted))^2
  )

  expect_s3_class(result, "ngeo_car")
  expect_identical(result$constraint, "sum-to-zero spatial effect")
  expect_lt(fitted_roughness, observed_roughness)
})

test_that("exact CAR fails before an oversized dense solve", {
  fixture <- model_grid()
  previous <- getOption("neurogeo.max_exact_logdet")
  on.exit(
    options(neurogeo.max_exact_logdet = previous),
    add = TRUE
  )
  options(neurogeo.max_exact_logdet = 10L)

  expect_error(
    ngeo_car(
      fixture$x,
      "response",
      fixture$weights,
      precision = 1
    ),
    class = "ngeo_error_resource"
  )
})

test_that("support models retain every map identity and scoped claim", {
  fixture <- inference_fixture()
  result <- ngeo_support_model(
    fixture$source,
    fixture$maps,
    fixture$targets,
    response = "outcome",
    predictors = "predictor",
    model = "ols"
  )

  expect_s3_class(result, "ngeo_support_model")
  expect_equal(length(result$fits), 2L)
  expect_equal(length(result$support_map_hashes), 2L)
  expect_match(result$claim, "not invariant")
})

test_that("2.4 models reject unbounded or singular requests", {
  fixture <- model_grid()
  expect_error(
    ngeo_spatial_regression(
      fixture$x, "response", "predictor", model = "sar"
    ),
    class = "ngeo_error_weights"
  )
  expect_error(
    ngeo_gwr_bandwidth(
      fixture$x, "response", "predictor",
      candidates = 0
    ),
    class = "ngeo_error_argument"
  )
})

test_that("kriging executes metric distances and covariance variance algebra", {
  coordinates <- rbind(
    c(0, 0, 0),
    c(10, 0, 0),
    c(10, 1, 0),
    c(0.1, 0, 0.1)
  )
  surface <- ngeo_surface(
    coordinates,
    rbind(c(1, 2, 3), c(2, 3, 4)),
    values = cbind(signal = 1:4),
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  fit <- structure(
    list(
      model = "exponential",
      parameters = c(nugget = 0.1, partial_sill = 1, range = 5)
    ),
    class = "ngeo_variogram_fit"
  )
  euclidean <- ngeo_kriging(
    surface, "signal", fit, targets = 1:4,
    neighbors = 4, metric = "euclidean"
  )
  geodesic <- ngeo_kriging(
    surface, "signal", fit, targets = 1:4,
    neighbors = 4, metric = "edge_geodesic"
  )
  expect_false(isTRUE(all.equal(
    euclidean$prediction, geodesic$prediction
  )))

  points <- ngeo_points(
    cbind(c(0, 1, 3), 0),
    values = cbind(signal = c(1, 2, 4)),
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  target <- matrix(c(1.5, 0, 0), nrow = 1)
  result <- ngeo_kriging(
    points, "signal", fit, targets = target,
    neighbors = 3, metric = "euclidean"
  )
  observed <- neurogeo:::.ngeo_element_coordinates(points)
  covariance <- neurogeo:::.ngeo_variogram_covariance(
    neurogeo:::.ngeo_euclidean_matrix(observed, observed),
    fit,
    diagonal = TRUE
  )
  cross <- neurogeo:::.ngeo_variogram_covariance(
    neurogeo:::.ngeo_euclidean_matrix(observed, target),
    fit
  )[, 1]
  system <- rbind(
    cbind(covariance, 1),
    c(rep(1, 3), 0)
  )
  solution <- solve(system, c(cross, 1))
  reference <- 1.1 - sum(solution[1:3] * cross) - solution[[4]]
  expect_equal(result$variance, reference, tolerance = 1e-12)
  expect_error(
    ngeo_kriging(
      points, "signal", fit, targets = target,
      metric = "edge_geodesic"
    ),
    class = "ngeo_error_metric"
  )
})

test_that("GWR bandwidth selection executes the declared metric", {
  coordinates <- rbind(
    c(0, 0, 0),
    c(10, 0, 0),
    c(10, 1, 0),
    c(0.1, 0, 0.1),
    c(5, 0.2, 0.2),
    c(5, 0.8, 0.2)
  )
  surface <- ngeo_surface(
    coordinates,
    rbind(
      c(1, 2, 5), c(2, 5, 6),
      c(2, 3, 6), c(3, 4, 6)
    ),
    values = cbind(
      response = c(1, 2, 3, 5, 2.5, 3.5),
      predictor = seq_len(6)
    )
  )
  euclidean <- ngeo_gwr_bandwidth(
    surface, "response", "predictor",
    candidates = c(2, 6, 12), metric = "euclidean"
  )
  geodesic <- ngeo_gwr_bandwidth(
    surface, "response", "predictor",
    candidates = c(2, 6, 12), metric = "edge_geodesic"
  )
  expect_false(isTRUE(all.equal(
    euclidean$candidates$rmse,
    geodesic$candidates$rmse
  )))
})

test_that("SAR likelihood respects the spectral parameter interval", {
  coordinates <- rbind(
    c(0, 0), c(1, 0), c(0.5, 0.8),
    c(10, 0), c(11, 0), c(10.5, 0.8)
  )
  response <- c(
    0.5872355, 0.6278771, 0.6477944,
    -1.1849773, -1.2526826, -0.9547022
  )
  x <- ngeo_points(
    coordinates,
    values = cbind(response = response)
  )
  weights <- ngeo_weights(
    x,
    method = "distance_band",
    threshold = 1.1,
    style = "B"
  )
  fit <- ngeo_spatial_regression(
    x, "response", weights = weights, model = "sar"
  )

  expect_equal(fit$parameter_interval[[2]], 0.5, tolerance = 1e-7)
  expect_lt(fit$spatial_parameter, fit$parameter_interval[[2]])
  expect_gt(fit$spatial_parameter, fit$parameter_interval[[1]])
})
