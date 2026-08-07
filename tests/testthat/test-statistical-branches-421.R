test_that("kernel regression validates bandwidth, support, and local rank", {
  coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
  predictor <- coordinates[, 1] + coordinates[, 2]
  x <- ngeo_point(
    coordinates,
    values = cbind(
      response = 1 + 2 * predictor,
      predictor = predictor,
      duplicate = predictor
    )
  )

  expect_error(
    ngeo_kernel_regression(x, "response", "predictor", bandwidth = 0),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_kernel_regression(
      x, "response", "predictor", bandwidth = 1, cutoff = Inf
    ),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_kernel_regression(
      x, "response", "predictor", bandwidth = 1, support = "base"
    ),
    class = "ngeo_error_support"
  )

  previous <- options(neurogeo.max_kernel_targets = 2L)
  on.exit(options(previous), add = TRUE)
  expect_error(
    ngeo_kernel_regression(x, "response", bandwidth = 2),
    class = "ngeo_error_resource"
  )
  options(neurogeo.max_kernel_targets = 2000L)

  sparse <- ngeo_kernel_regression(
    x,
    "response",
    "predictor",
    bandwidth = 0.2,
    kernel = "bisquare",
    targets = 1:2,
    singular = "na"
  )
  expect_true(all(is.na(sparse$fitted)))
  expect_error(
    ngeo_kernel_regression(
      x,
      "response",
      "predictor",
      bandwidth = 0.2,
      kernel = "bisquare",
      targets = 1,
      singular = "error"
    ),
    class = "ngeo_error_model"
  )
  expect_error(
    ngeo_kernel_regression(
      x,
      "response",
      c("predictor", "duplicate"),
      bandwidth = 4,
      targets = 1,
      singular = "error"
    ),
    class = "ngeo_error_model"
  )

  intercept_only <- ngeo_kernel_regression(
    x,
    "response",
    bandwidth = 2,
    targets = c(1, 5, 9)
  )
  expect_equal(nrow(intercept_only), 3L)
  expect_true(all(is.finite(intercept_only$fitted)))

  missing <- x
  missing$values[2, "response"] <- NA_real_
  expect_error(
    ngeo_kernel_regression(
      missing, "response", "predictor", bandwidth = 2
    ),
    class = "ngeo_error_missing"
  )
  omitted <- ngeo_kernel_regression(
    missing,
    "response",
    "predictor",
    bandwidth = 2,
    targets = c(1, 3),
    na_action = "omit"
  )
  expect_equal(nrow(omitted), 2L)

  missing_target <- missing
  missing_target$values[1, "predictor"] <- NA_real_
  expect_error(
    ngeo_kernel_regression(
      missing_target,
      "response",
      "predictor",
      bandwidth = 2,
      targets = 1,
      na_action = "omit"
    ),
    class = "ngeo_error_missing"
  )
})

test_that("kernel regression can use explicit base support", {
  values <- array(seq_len(8), c(2, 2, 2))
  volume <- ngeo_volume(
    values = values,
    dim = c(2, 2, 2),
    affine = diag(4),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  result <- ngeo_kernel_regression(
    volume,
    response = 1,
    bandwidth = 2,
    targets = 1:2,
    support = "base"
  )
  expect_equal(nrow(result), 2L)
  expect_identical(attr(result, "support"), "base")
})

test_that("variogram rejects undefined estimands and invalid pair bins", {
  coordinates <- matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE)
  geometry <- ngeo_point(coordinates)
  expect_error(ngeo_variogram(geometry), class = "ngeo_error_values")

  categorical <- ngeo_point(
    coordinates,
    values = cbind(label = 1:4),
    measures = ngeo_measure(support_behavior = "categorical")
  )
  expect_error(
    ngeo_variogram(categorical),
    class = "ngeo_error_measure"
  )

  x <- ngeo_point(
    coordinates,
    values = cbind(a = c(1, 2, 4, 8), b = 1:4)
  )
  expect_error(ngeo_variogram(x, map = c(1, 2)), class = "ngeo_error_argument")
  expect_error(ngeo_variogram(x, max_distance = 0), class = "ngeo_error_argument")
  expect_error(ngeo_variogram(x, breaks = 0), class = "ngeo_error_argument")
  expect_error(
    ngeo_variogram(x, breaks = c(0, 2, 1, 4)),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_variogram(x, max_distance = 0.1),
    class = "ngeo_error_statistic"
  )

  previous <- options(neurogeo.max_variogram_pairs = 2L)
  on.exit(options(previous), add = TRUE)
  expect_error(ngeo_variogram(x), class = "ngeo_error_dense_distance")
  options(neurogeo.max_variogram_pairs = 1e6)

  missing <- x
  missing$values[, "a"] <- c(1, NA, Inf, NA)
  expect_error(
    ngeo_variogram(missing, "a"),
    class = "ngeo_error_missing"
  )
  expect_error(
    ngeo_variogram(missing, "a", na_action = "omit"),
    class = "ngeo_error_statistic"
  )

  regular <- ngeo_variogram(x, "a", breaks = 3L)
  expect_s3_class(regular, "ngeo_variogram")
  expect_gt(nrow(regular), 0L)

  colocated <- ngeo_point(
    matrix(0, nrow = 3, ncol = 2),
    values = cbind(signal = 1:3)
  )
  expect_error(
    ngeo_variogram(colocated, breaks = 2L),
    class = "ngeo_error_statistic"
  )
})
