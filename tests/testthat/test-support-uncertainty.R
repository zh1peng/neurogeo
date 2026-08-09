test_that("base-bound diagonal covariance propagates analytically", {
  fixture <- diagnostic_fixture()
  covariance <- ngeo_support_covariance(
    fixture$source,
    variance = rep(1, 4)
  )
  result <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$hard,
    covariance,
    layers = "outcome",
    method = "analytic",
    output = "covariance"
  )

  expect_s3_class(covariance, "ngeo_support_covariance")
  expect_s3_class(result, "ngeo_support_uncertainty")
  expect_equal(result$variance[, 1L], rep(5 / 9, 2L))
  expect_equal(
    Matrix::diag(result$covariance[[1L]]),
    result$variance[, 1L]
  )
  expect_equal(
    as.matrix(result$covariance[[1L]])[1L, 2L],
    0
  )
})

test_that("low-rank covariance retains shared target dependence", {
  fixture <- diagnostic_fixture()
  covariance <- ngeo_support_covariance(
    fixture$source,
    variance = rep(0.25, 4),
    factor = matrix(rep(0.5, 4), ncol = 1L)
  )
  result <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$hard,
    covariance,
    layers = "outcome",
    output = "covariance"
  )

  expect_identical(covariance$representation, "low_rank")
  expect_gt(as.matrix(result$covariance[[1L]])[1L, 2L], 0)
  expect_equal(
    result$variance[, 1L],
    Matrix::diag(result$covariance[[1L]])
  )
})

test_that("analytic and Monte Carlo normalized covariance agree", {
  fixture <- diagnostic_fixture()
  covariance <- ngeo_support_covariance(
    fixture$source,
    variance = rep(1, 4)
  )
  analytic <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$soft,
    covariance,
    layers = "outcome"
  )
  monte_carlo <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$soft,
    covariance,
    layers = "outcome",
    method = "monte_carlo",
    nsim = 3000,
    seed = 22
  )

  expect_equal(
    monte_carlo$variance,
    analytic$variance,
    tolerance = 0.08
  )
  expect_identical(monte_carlo$seed, 22L)
})

test_that("operator ensembles enforce common ordered domains", {
  fixture <- diagnostic_fixture()
  ensemble <- ngeo_registration_ensemble(
    list(hard = fixture$hard, soft = fixture$soft),
    spatial_weights = c(0.25, 0.75)
  )

  expect_s3_class(ensemble, "ngeo_support_ensemble")
  expect_identical(ensemble$kind, "registration")
  expect_equal(ensemble$spatial_weights, c(0.25, 0.75))
  expect_silent(ngeo_validate_support_ensemble(ensemble))

  changed <- fixture$soft
  changed$target_element_id <- rev(changed$target_element_id)
  expect_error(
    ngeo_segmentation_ensemble(list(fixture$hard, changed)),
    class = "ngeo_error_base_mismatch"
  )
})

test_that("Monte Carlo uncertainty cycles a declared operator ensemble", {
  fixture <- diagnostic_fixture()
  covariance <- ngeo_support_covariance(
    fixture$source,
    variance = rep(0.01, 4)
  )
  ensemble <- ngeo_segmentation_ensemble(
    list(fixture$hard, fixture$soft)
  )
  result <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$hard,
    covariance,
    layers = "outcome",
    method = "monte_carlo",
    operator_ensemble = ensemble,
    nsim = 100,
    seed = 7
  )

  expect_identical(
    result$operator_ensemble_hash,
    ensemble$ensemble_hash
  )
  expect_true(all(result$variance > 0))
  expect_match(result$assumptions, "segmentation ensemble")
})

test_that("support conditioning is exact for identity and bounded at scale", {
  surface <- builder_surface()
  identity <- ngeo_surface_nearest_map(surface, surface)
  condition <- ngeo_support_condition(identity)

  expect_s3_class(condition, "ngeo_support_condition")
  expect_equal(condition$stable_rank, 4, tolerance = 1e-8)
  expect_equal(condition$numerical_rank, 4L)
  expect_equal(condition$condition_number, 1)
  expect_false(any(condition$source$isolate))
})

test_that("enhanced diagnostics summarize structures and conditioning", {
  fixture <- diagnostic_fixture()
  diagnostics <- ngeo_support_diagnostics(
    fixture$soft,
    source_structure = c("left", "left", "right", "right")
  )

  expect_equal(
    diagnostics$coverage_by_structure$structure,
    c("left", "right")
  )
  expect_equal(
    diagnostics$coverage_by_structure$support_coverage_fraction,
    c(1, 1)
  )
  expect_s3_class(
    diagnostics$conditioning,
    "ngeo_support_condition"
  )
  expect_true(all(c(
    "entropy_q95",
    "mean_effective_target_count",
    "operator_variance_total"
  ) %in% diagnostics$summary$distance_method))
})

test_that("ensemble sensitivity decomposes between-operator variance", {
  fixture <- diagnostic_fixture()
  ensemble <- ngeo_support_ensemble(
    list(fixture$hard, fixture$soft),
    kind = "operator"
  )
  result <- ngeo_support_sensitivity(
    fixture$source,
    fixture$target,
    ensemble
  )

  expect_identical(result$ensemble_hash, ensemble$ensemble_hash)
  expect_true(any(result$distribution$between_operator_variance > 0))
  expect_true(all(result$distribution$total_variance >= 0))
})

test_that("non-uniform ensembles use declared mixture weights", {
  fixture <- diagnostic_fixture()
  ensemble <- ngeo_support_ensemble(
    list(hard = fixture$hard, soft = fixture$soft),
    spatial_weights = c(0.9, 0.1)
  )
  sensitivity <- ngeo_support_sensitivity(
    fixture$source,
    fixture$target,
    ensemble,
    layers = "outcome"
  )
  alternatives <- do.call(cbind, lapply(
    sensitivity$values,
    function(value) value[, 1L]
  ))
  expected_mean <- as.numeric(alternatives %*% c(0.9, 0.1))
  expected_variance <- rowSums(
    sweep(
      sweep(alternatives, 1L, expected_mean, "-")^2,
      2L,
      c(0.9, 0.1),
      "*"
    )
  )

  expect_equal(sensitivity$distribution$ensemble_mean, expected_mean)
  expect_equal(
    sensitivity$distribution$between_operator_variance,
    expected_variance
  )
  expect_equal(
    sensitivity$distribution$lower,
    apply(alternatives, 1L, min)
  )
  expect_equal(
    sensitivity$distribution$upper,
    apply(alternatives, 1L, max)
  )

  covariance <- ngeo_support_covariance(
    fixture$source,
    variance = rep(0, 4L)
  )
  first <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$hard,
    covariance,
    layers = "outcome",
    method = "monte_carlo",
    operator_ensemble = ensemble,
    nsim = 2000L,
    seed = 1701
  )
  second <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$hard,
    covariance,
    layers = "outcome",
    method = "monte_carlo",
    operator_ensemble = ensemble,
    nsim = 2000L,
    seed = 1701
  )
  expected_frequency <- 0.1
  observed_frequency <- mean(first$operator_draw == 2L)
  binomial_sd <- sqrt(expected_frequency * (1 - expected_frequency) / 2000)

  expect_identical(first$operator_draw, second$operator_draw)
  expect_lte(abs(observed_frequency - expected_frequency), 4 * binomial_sd)
  expect_equal(first$estimate[, 1L], expected_mean, tolerance = 1e-12)
  expect_match(first$assumptions, "declared weights", fixed = TRUE)
})

test_that("covariance validation rejects base and PSD violations", {
  fixture <- diagnostic_fixture()
  invalid <- diag(4)
  invalid[1L, 2L] <- 2
  invalid[2L, 1L] <- 2
  expect_error(
    ngeo_support_covariance(
      fixture$source,
      covariance = invalid
    ),
    class = "ngeo_error_uncertainty"
  )

  covariance <- ngeo_support_covariance(
    fixture$source,
    variance = rep(1, 4)
  )
  other <- fixture$source
  other$base$elements$element_id[[1L]] <- "changed"
  expect_error(
    neurogeo::ngeo_support_uncertainty(
      other,
      fixture$target,
      fixture$hard,
      covariance,
      layers = "outcome"
    ),
    class = "ngeo_error_base_mismatch"
  )
})

test_that("random normalized covariance matches direct Jacobians", {
  set.seed(2202)
  for (iteration in seq_len(15L)) {
    n_source <- sample(8:20, 1L)
    n_target <- sample(2:5, 1L)
    source <- ngeo_point(
      cbind(seq_len(n_source), 0),
      values = stats::rnorm(n_source),
      measures = ngeo_measure(support_behavior = "intensive"),
      coordinate_space = ngeo_coordinate_space("covariance-property")
    )
    probability <- matrix(
      stats::rexp(n_source * n_target),
      nrow = n_source
    )
    probability <- probability / rowSums(probability)
    colnames(probability) <- paste0("R", seq_len(n_target))
    support <- stats::runif(n_source, 0.5, 2)
    map <- ngeo_probabilistic_atlas_map(
      source,
      probability,
      source_support = support
    )
    variance <- stats::runif(n_source, 0.01, 0.5)
    covariance <- ngeo_support_covariance(
      source,
      variance = variance
    )
    result <- ngeo_support_uncertainty(
      source,
      map$target,
      map,
      covariance
    )
    denominator <- as.numeric(map$operator %*% support)
    jacobian <- Matrix::Diagonal(x = 1 / denominator) %*%
      map$operator %*% Matrix::Diagonal(x = support)
    expected <- as.numeric((jacobian^2) %*% variance)

    expect_equal(result$variance[, 1L], expected, tolerance = 1e-10)
  }
})

test_that("ensemble integrity hashes detect mutation", {
  fixture <- diagnostic_fixture()
  ensemble <- ngeo_support_ensemble(
    list(fixture$hard, fixture$soft)
  )
  ensemble$spatial_weights <- c(0.9, 0.1)

  expect_error(
    ngeo_validate_support_ensemble(ensemble),
    class = "ngeo_error_uncertainty"
  )
})
