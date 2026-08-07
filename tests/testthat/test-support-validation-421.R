expect_support_map_error <- function(x, class = "ngeo_error_support_map") {
  expect_error(ngeo_validate_support_map(x), class = class)
}

test_that("support-map validation audits every stored invariant", {
  fixture <- diagnostic_fixture()
  map <- fixture$soft

  expect_error(
    ngeo_validate_support_map(list()),
    class = "ngeo_error_support_map"
  )
  expect_error(
    ngeo_validate_support_map(map, tolerance = -1),
    class = "ngeo_error_argument"
  )

  changed <- map
  changed$source_element_id <- changed$source_element_id[-1]
  expect_support_map_error(changed)

  changed <- map
  changed$source_base_hash <- ""
  expect_support_map_error(changed)

  changed <- map
  changed$direction <- "source_by_target"
  expect_support_map_error(changed)

  changed <- map
  changed$operator@x[[1L]] <- -1
  expect_support_map_error(changed)

  changed <- map
  changed$type <- "unknown"
  expect_support_map_error(changed)

  changed <- map
  changed$coverage <- "unknown"
  expect_support_map_error(changed)

  changed <- fixture$hard
  changed$operator@x[[1L]] <- 0.5
  expect_support_map_error(changed)

  changed <- map
  changed$operator@x[] <- 0.75
  expect_support_map_error(changed)

  changed <- map
  changed$coverage <- "complete"
  changed$operator[, 1L] <- 0
  expect_support_map_error(changed)

  changed <- map
  changed$source_support <- c(1, 1)
  expect_support_map_error(changed, "ngeo_error_support")

  changed <- map
  changed$target_support <- -c(1, 1)
  expect_support_map_error(changed, "ngeo_error_support")

  changed <- map
  changed$target_support <- changed$target_support + 1
  expect_support_map_error(changed, "ngeo_error_support")

  changed <- map
  changed$weight_variance <- methods::as(
    Matrix::Matrix(matrix(-1, nrow = 2, ncol = 4), sparse = TRUE),
    "dgCMatrix"
  )
  expect_support_map_error(changed, "ngeo_error_uncertainty")
})

test_that("support covariance rejects malformed representations", {
  source <- diagnostic_fixture()$source
  expect_error(
    ngeo_support_covariance(source, variance = rep(1, 4), tolerance = -1),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_support_covariance(source),
    class = "ngeo_error_uncertainty"
  )
  expect_error(
    ngeo_support_covariance(
      source,
      covariance = diag(4),
      factor = matrix(1, 4, 1)
    ),
    class = "ngeo_error_uncertainty"
  )
  expect_error(
    ngeo_support_covariance(source, variance = c(1, 2)),
    class = "ngeo_error_uncertainty"
  )
  expect_error(
    ngeo_support_covariance(source, covariance = diag(3)),
    class = "ngeo_error_alignment"
  )

  asymmetric <- diag(4)
  asymmetric[1, 2] <- 0.5
  expect_error(
    ngeo_support_covariance(source, covariance = asymmetric),
    class = "ngeo_error_uncertainty"
  )
  negative_diagonal <- diag(c(-1, 1, 1, 1))
  expect_error(
    ngeo_support_covariance(source, covariance = negative_diagonal),
    class = "ngeo_error_uncertainty"
  )
  expect_error(
    ngeo_support_covariance(source, factor = matrix(1, 3, 1)),
    class = "ngeo_error_uncertainty"
  )

  diagonal <- ngeo_support_covariance(source, variance = rep(0.1, 4))
  changed <- diagonal
  changed$element_id[[1L]] <- changed$element_id[[2L]]
  expect_error(
    ngeo_validate_support_covariance(changed),
    class = "ngeo_error_uncertainty"
  )
  changed <- diagonal
  changed$representation <- "matrix"
  expect_error(
    ngeo_validate_support_covariance(changed),
    class = "ngeo_error_uncertainty"
  )
  changed <- diagonal
  changed$representation <- "low_rank"
  expect_error(
    ngeo_validate_support_covariance(changed),
    class = "ngeo_error_uncertainty"
  )
})

test_that("matrix and operator uncertainty paths remain reproducible", {
  fixture <- diagnostic_fixture()
  matrix_covariance <- ngeo_support_covariance(
    fixture$source,
    covariance = diag(rep(0.02, 4))
  )
  monte_carlo <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$soft,
    matrix_covariance,
    layers = "outcome",
    method = "monte_carlo",
    output = "covariance",
    nsim = 20,
    seed = 421
  )
  expect_identical(monte_carlo$seed, 421L)
  expect_equal(dim(monte_carlo$covariance[[1L]]), c(2L, 2L))

  zero_covariance <- ngeo_support_covariance(
    fixture$source,
    covariance = matrix(0, 4, 4)
  )
  zero_result <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$soft,
    zero_covariance,
    layers = "outcome",
    method = "monte_carlo",
    nsim = 4,
    seed = 7
  )
  expect_equal(
    zero_result$variance,
    matrix(0, nrow = 2, ncol = 1),
    ignore_attr = TRUE
  )

  uncertain_layer <- fixture$soft
  uncertain_layer$weight_variance <- methods::as(
    uncertain_layer$operator * 0.01,
    "dgCMatrix"
  )
  ngeo_validate_support_map(uncertain_layer)
  analytic <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    uncertain_layer,
    matrix_covariance,
    layers = "outcome"
  )
  expect_true(all(analytic$variance > 0))

  extensive <- fixture$source
  extensive$measures$support_behavior[[1L]] <- "extensive"
  extensive_result <- ngeo_support_uncertainty(
    extensive,
    fixture$target,
    uncertain_layer,
    matrix_covariance,
    layers = "outcome"
  )
  expect_true(all(extensive_result$variance > 0))
})

test_that("uncertainty obeys configured dense covariance limits", {
  fixture <- diagnostic_fixture()
  covariance <- ngeo_support_covariance(
    fixture$source,
    covariance = diag(rep(0.1, 4))
  )

  previous <- options(neurogeo.max_covariance_draw_dimension = 2L)
  on.exit(options(previous), add = TRUE)
  expect_error(
    ngeo_support_uncertainty(
      fixture$source,
      fixture$target,
      fixture$soft,
      covariance,
      layers = "outcome",
      method = "monte_carlo",
      nsim = 3
    ),
    class = "ngeo_error_resource"
  )

  options(
    neurogeo.max_covariance_draw_dimension = 5000L,
    neurogeo.max_full_covariance_targets = 1L
  )
  expect_error(
    ngeo_support_uncertainty(
      fixture$source,
      fixture$target,
      fixture$soft,
      covariance,
      layers = "outcome",
      output = "covariance"
    ),
    class = "ngeo_error_resource"
  )
})

test_that("support sensitivity validates ensembles and propagates value variance", {
  fixture <- diagnostic_fixture()
  layers <- list(hard = fixture$hard, soft = fixture$soft)
  result <- ngeo_support_sensitivity(
    fixture$source,
    fixture$target,
    layers,
    layers = "outcome",
    reference = 2L,
    value_variance = rep(0.1, 4)
  )
  expect_identical(result$reference, 2L)
  expect_length(result$variance, 2L)
  expect_true(all(is.finite(result$distribution$mean_within_variance)))
  expect_identical(result$ensemble_kind, "alternative_maps")

  expect_error(
    ngeo_support_sensitivity(
      fixture$source,
      fixture$target,
      list(fixture$hard)
    ),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_support_sensitivity(
      fixture$source,
      list(fixture$target),
      layers
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_support_sensitivity(
      fixture$source,
      fixture$target,
      layers,
      reference = 3L
    ),
    class = "ngeo_error_argument"
  )

  mismatched <- fixture$soft
  mismatched$target_base_hash <- "different"
  expect_error(
    ngeo_support_sensitivity(
      fixture$source,
      fixture$target,
      list(fixture$hard, mismatched)
    ),
    class = "ngeo_error_base_mismatch"
  )
})
