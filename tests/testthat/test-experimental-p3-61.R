test_that("brain point process computes finite-domain pair enrichment", {
  x <- ngeo_point(
    cbind(x = 0:5, y = 0),
    coordinate_space = ngeo_coordinate_space(
      "event-line", kind = "unknown", unit = "mm"
    )
  )
  result <- ngeo_brain_point_process(
    x,
    events = list(A = c(1L, 2L, 5L), B = c(3L, 6L)),
    radii = c(0, 1, 3),
    pairs = data.frame(x = c("A", "A"), y = c("A", "B")),
    simulations = 19L,
    seed = 613L
  )

  expect_s3_class(result, "ngeo_brain_point_process")
  expect_identical(result$history$status, "stable")
  expect_equal(nrow(result$estimates), 6L)
  for (pair in unique(paste(
    result$estimates$type_x, result$estimates$type_y
  ))) {
    rows <- paste(result$estimates$type_x, result$estimates$type_y) == pair
    expect_true(all(result$estimates$pair_fraction[rows] >= 0))
    expect_true(all(result$estimates$pair_fraction[rows] <= 1))
  }
  finite <- is.finite(result$estimates$relative_pair_enrichment)
  expect_true(all(result$estimates$p_clustering[finite] > 0))
  expect_identical(result$history$edge_correction,
                   "finite-domain exposure-weighted CSR normalization")
  expect_identical(result$history$familywise_envelope, FALSE)
  contract <- ngeo_inference_contract(result)
  expect_identical(contract$identifiers$analysis_hash, result$analysis_hash)
  expect_identical(contract$identifiers$result_hash, result$result_hash)
})

test_that("brain point process simulations are reproducible", {
  x <- ngeo_point(cbind(x = 0:4, y = 0))
  first <- ngeo_brain_point_process(
    x, c(1L, 3L, 5L), radii = c(1, 2), simulations = 9L, seed = 9L,
    retain_simulations = TRUE
  )
  second <- ngeo_brain_point_process(
    x, c(1L, 3L, 5L), radii = c(1, 2), simulations = 9L, seed = 9L,
    retain_simulations = TRUE
  )
  expect_equal(first$simulations$estimates, second$simulations$estimates)
  expect_equal(first$estimates, second$estimates)
})

test_that("point process provides global envelopes and weighted simple draws", {
  x <- ngeo_point(cbind(x = 0:5, y = 0))
  result <- ngeo_brain_point_process(
    x, events = c(1L, 3L, 5L), radii = 0:3,
    exposure = c(1, 2, 3, 4, 5, 6), occupancy = "simple",
    simulations = 39L, seed = 6202L, envelope = "global",
    retain_simulations = TRUE
  )

  expect_true(result$history$familywise_envelope)
  expect_match(result$history$simple_sampling, "without replacement")
  expect_match(result$history$csr_normalization, "Monte Carlo")
  finite <- is.finite(result$estimates$relative_pair_enrichment)
  expect_true(all(is.finite(
    result$estimates$global_envelope_lower[finite]
  )))
  expect_true(all(result$estimates$p_global_clustering[finite] > 0))
  simulation_mean <- colMeans(result$simulations$estimates, na.rm = TRUE)
  expect_equal(
    simulation_mean[is.finite(simulation_mean)],
    rep(1, sum(is.finite(simulation_mean))), tolerance = 1e-12
  )
})

p3_time_fixture <- function() {
  values <- matrix(
    c(
      8, 6, 1, 0, 0, 0,
      2, 8, 6, 1, 0, 0,
      0, 2, 8, 6, 1, 0,
      0, 0, 2, 8, 6, 1,
      0, 0, 0, 2, 8, 6
    ),
    nrow = 5L,
    byrow = TRUE
  )
  # Columns are time maps; transpose the moving-pulse rows above.
  values <- t(values)
  x <- ngeo_point(
    cbind(x = 0:5, y = 0),
    values = values,
    measures = ngeo_measure(support_behavior = "intensive")
  )
  x <- ngeo_set_time_axis(
    x,
    ngeo_time_axis(c(0, 1, 2.5, 4, 7), unit = "day")
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  list(x = x, weights = weights)
}

test_that("nonseparable hotspots expose state, change, and propagation", {
  fixture <- p3_time_fixture()
  result <- ngeo_nonseparable_hotspots(
    fixture$x,
    fixture$weights,
    spatial_bandwidth = 0.6,
    temporal_bandwidth = 0.8,
    interaction = 1.5,
    spatial_cutoff = 2L,
    temporal_cutoff = 3,
    z_threshold = 0.7,
    retain_matrix = TRUE
  )

  expect_s3_class(result, "ngeo_nonseparable_hotspots")
  expect_identical(result$history$status, "stable")
  expect_identical(dim(result$z), c(6L, 5L))
  expect_equal(nrow(result$local), 30L)
  expect_equal(nrow(result$state), 6L)
  expect_true(all(result$state$hot_class %in% c(
    "none", "historical", "new", "persistent", "consecutive", "recurrent"
  )))
  expect_true(nrow(result$propagation) > 0L)
  expect_true(all(result$propagation$lag > 0))
  expect_identical(result$history$nonseparable, TRUE)
  expect_identical(result$history$causal_propagation_claimed, FALSE)
  expect_match(result$history$inference, "exploratory")
  contract <- ngeo_inference_contract(result)
  expect_identical(contract$identifiers$axis_hash, result$axis_hash)
  expect_identical(contract$identifiers$weights_hash, result$weights_hash)
})

test_that("nonseparable hotspots reject a separable zero interaction", {
  fixture <- p3_time_fixture()
  expect_error(
    ngeo_nonseparable_hotspots(
      fixture$x, fixture$weights, interaction = 0
    ),
    class = "ngeo_error_argument"
  )
})

test_that("hotspots provide joint null maxT and group feature interface", {
  fixture <- p3_time_fixture()
  first <- ngeo_nonseparable_hotspots(
    fixture$x, fixture$weights, spatial_cutoff = 2L,
    temporal_cutoff = 3, permutations = 19L, seed = 6203L
  )
  second_x <- fixture$x
  second_x$values <- second_x$values + matrix(
    seq_len(length(second_x$values)) / 100,
    nrow = nrow(second_x$values)
  )
  second <- ngeo_nonseparable_hotspots(
    second_x, fixture$weights, spatial_cutoff = 2L,
    temporal_cutoff = 3, permutations = 19L, seed = 6204L
  )

  expect_true(first$history$familywise_error_control)
  expect_true(all(first$local$p_adjusted >= first$local$p_value))
  expect_true(all(first$local$time_unit == "day"))
  features <- ngeo_hotspot_group_features(
    list(subject_1 = first, subject_2 = second),
    summaries = c("mean_z", "trend")
  )
  expect_s3_class(features, "ngeo_subject_features")
  expect_identical(rownames(features$values), c("subject_1", "subject_2"))
  expect_equal(ncol(features$values), 12L)
  expect_identical(features$diagnostics$time_unit, "day")
})

test_that("finite-domain pair enrichment has exact CSR normalization", {
  x <- ngeo_point(cbind(x = 0:3, y = 0))
  result <- ngeo_brain_point_process(
    x, events = list(A = 1L, B = 2L), radii = 0:3,
    pairs = data.frame(x = "A", y = "B"),
    simulations = 0L
  )
  probability <- rep(1 / 4, 4)
  distance <- abs(outer(0:3, 0:3, "-"))
  expected_reference <- vapply(0:3, function(radius) {
    sum(outer(probability, probability) * (distance <= radius))
  }, numeric(1))
  expect_equal(result$estimates$csr_pair_probability, expected_reference)
  expect_equal(result$estimates$pair_fraction, c(0, 1, 1, 1))
  expect_equal(
    result$estimates$relative_pair_enrichment,
    result$estimates$pair_fraction / expected_reference
  )
})

test_that("undefined single-event self-K does not acquire a p-value", {
  x <- ngeo_point(cbind(x = 0:3, y = 0))
  result <- ngeo_brain_point_process(
    x, events = 1L, radii = 1, simulations = 9L, seed = 1L
  )
  expect_true(is.na(result$estimates$relative_pair_enrichment))
  expect_true(is.na(result$estimates$p_clustering))
})

test_that("hotspot computation obeys materialization budgets", {
  fixture <- p3_time_fixture()
  expect_error(
    ngeo_nonseparable_hotspots(
      fixture$x, fixture$weights,
      spatial_cutoff = 2L, temporal_cutoff = 3,
      budget = ngeo_resource_budget(materialized_elements = 2)
    ),
    class = "ngeo_error_resource"
  )
})

test_that("point process normalizes extreme exposure and canonicalizes metrics", {
  x <- ngeo_point(cbind(x = 0:2, y = 0))
  result <- ngeo_brain_point_process(
    x, list(A = 1L, B = 2L), radii = 1,
    pairs = data.frame(x = "A", y = "B"),
    exposure = rep(1e308, 3L), distance_method = "euc"
  )
  expect_identical(result$history$distance_method, "euclidean")
  expect_true(all(is.finite(result$estimates$csr_pair_probability)))
  expect_error(
    ngeo_brain_point_process(x, 1L, radii = 1, max_elements = 3e9),
    class = "ngeo_error_index"
  )
})

test_that("hotspots enforce temporal semantics and effective interaction", {
  fixture <- p3_time_fixture()
  interval <- ngeo_set_time_axis(
    fixture$x,
    ngeo_time_axis(
      time = c(0.5, 2, 4.5, 8, 12),
      interval_start = c(0, 1, 3, 6, 10),
      interval_end = c(1, 3, 6, 10, 14),
      unit = "day"
    ),
    temporal_semantics = "interval_total"
  )
  expect_error(
    ngeo_nonseparable_hotspots(interval, fixture$weights),
    class = "ngeo_error_temporal_measure"
  )
  expect_error(
    ngeo_nonseparable_hotspots(
      fixture$x, fixture$weights, temporal_cutoff = 0.1
    ),
    class = "ngeo_error_support"
  )
})

test_that("hotspot scores are invariant to finite positive value scaling", {
  fixture <- p3_time_fixture()
  first <- ngeo_nonseparable_hotspots(
    fixture$x, fixture$weights, spatial_cutoff = 2L,
    temporal_cutoff = 3, retain_matrix = TRUE
  )
  scaled <- fixture$x
  scaled$values <- scaled$values * 1e-100
  second <- ngeo_nonseparable_hotspots(
    scaled, fixture$weights, spatial_cutoff = 2L,
    temporal_cutoff = 3, retain_matrix = TRUE
  )
  expect_equal(first$z, second$z, tolerance = 1e-12)

  shifted <- fixture$x
  shifted$values <- shifted$values * 2 + 1e16
  third <- ngeo_nonseparable_hotspots(
    shifted, fixture$weights, spatial_cutoff = 2L,
    temporal_cutoff = 3, retain_matrix = TRUE
  )
  expect_equal(first$z, third$z, tolerance = 1e-12)
})

test_that("hotspot Gi-star denominator includes truncated zero weights", {
  fixture <- p3_time_fixture()
  result <- ngeo_nonseparable_hotspots(
    fixture$x, fixture$weights,
    spatial_bandwidth = 1, temporal_bandwidth = 1, interaction = 1,
    spatial_cutoff = 1L, temporal_cutoff = 1, retain_matrix = TRUE
  )
  values <- as.matrix(fixture$x$values)
  standardized <- as.numeric(scale(
    as.numeric(values), center = TRUE,
    scale = sqrt(mean((as.numeric(values) - mean(values))^2))
  ))
  standardized <- matrix(standardized, nrow = nrow(values))
  spatial <- c(0, 1)
  temporal <- c(0, 1)
  weight <- exp(
    -outer(spatial, rep(1, 2)) - outer(rep(1, 2), temporal) -
      outer(spatial, temporal)
  )
  n <- length(values)
  denominator <- sqrt(
    (n * sum(weight^2) - sum(weight)^2) / (n - 1)
  )
  expected <- sum(weight * standardized[1:2, 1:2]) / denominator
  expect_equal(result$z[1, 1], expected, tolerance = 1e-12)
})
