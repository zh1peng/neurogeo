time_points_fixture <- function(
    values = matrix(
      c(
        1, 2, 4,
        2, 4, 8,
        4, 8, 16,
        8, 16, 32
      ),
      nrow = 3
    ),
    time = c(0, 1, 3, 6),
    semantics = "instantaneous") {
  x <- ngeo_points(
    matrix(c(0, 0, 1, 0, 2, 0), ncol = 2L, byrow = TRUE),
    values = values
  )
  ngeo_set_time_axis(
    x,
    ngeo_time_axis(time = time, unit = "second"),
    temporal_semantics = semantics
  )
}

test_that("time axes are explicit, deterministic, and mutation-safe", {
  regular <- ngeo_time_axis(start = 2, step = 0.5, n = 4)
  irregular <- ngeo_time_axis(
    time = c(0, 1, 4, 10), unit = "hour"
  )
  interval <- ngeo_time_axis(
    time = c(0.5, 2),
    unit = "day",
    interval_start = c(0, 1),
    interval_end = c(1, 3)
  )

  expect_s3_class(regular, "ngeo_time_axis")
  expect_identical(regular$time, c(2, 2.5, 3, 3.5))
  expect_true(regular$regular)
  expect_equal(regular$step, 0.5)
  expect_false(irregular$regular)
  expect_true(all(interval$duration == c(1, 2)))
  expect_identical(ngeo_time_axis_hash(regular), regular$axis_hash)
  expect_invisible(ngeo_validate_time_axis(interval))

  expect_error(
    ngeo_time_axis(time = c(0, 1, 1)),
    class = "ngeo_error_time_axis"
  )
  expect_error(
    ngeo_time_axis(
      time = c(0.5, 1.25),
      interval_start = c(0, 0.75),
      interval_end = c(1, 1.5)
    ),
    class = "ngeo_error_temporal_support"
  )
  overlap <- ngeo_time_axis(
    time = c(0.5, 1.25),
    interval_start = c(0, 0.75),
    interval_end = c(1, 1.5),
    allow_overlap = TRUE
  )
  expect_s3_class(overlap, "ngeo_time_axis")

  changed <- regular
  changed$time[[2L]] <- 2.75
  expect_error(
    ngeo_validate_time_axis(changed),
    class = "ngeo_error_time_axis_mutation"
  )
  changed <- regular
  changed$regular <- FALSE
  expect_error(
    ngeo_validate_time_axis(changed),
    class = "ngeo_error_time_axis"
  )
})

test_that("time binding and slicing preserve one spatial domain", {
  x <- time_points_fixture()
  axis <- ngeo_get_time_axis(x)
  domain_hash <- ngeo_domain_hash(x)

  expect_identical(x$maps$time, axis$time)
  expect_identical(
    x$measures$temporal_semantics,
    rep("instantaneous", 4L)
  )
  expect_identical(nrow(x$domain$elements), 3L)

  sliced <- ngeo_time_slice(x, index = c(1L, 3L))
  ranged <- ngeo_time_slice(x, range = c(1, 3))
  expect_identical(ngeo_domain_hash(sliced), domain_hash)
  expect_identical(dim(sliced$values), c(3L, 2L))
  expect_identical(ngeo_get_time_axis(sliced)$time, c(0, 3))
  expect_identical(ngeo_get_time_axis(ranged)$time, c(1, 3))
  expect_equal(sliced$values, x$values[, c(1L, 3L), drop = FALSE])

  expect_error(
    ngeo_time_slice(x, index = c(2L, 1L)),
    class = "ngeo_error_index"
  )
  expect_error(
    ngeo_set_time_axis(
      x, ngeo_time_axis(time = 1:3)
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_set_time_axis(
      x,
      axis,
      temporal_semantics = c("instantaneous", "instantaneous")
    ),
    class = "ngeo_error_temporal_measure"
  )

  changed <- x
  changed$maps$time[[1L]] <- -1
  expect_error(
    ngeo_get_time_axis(changed),
    class = "ngeo_error_time_axis_mutation"
  )
})

test_that("temporal semantics require matching support", {
  x <- ngeo_points(
    matrix(c(0, 0, 1, 0), ncol = 2L, byrow = TRUE),
    values = matrix(1:4, nrow = 2L)
  )
  instant <- ngeo_time_axis(time = c(0, 1))
  interval <- ngeo_time_axis(
    time = c(0.5, 2),
    interval_start = c(0, 1),
    interval_end = c(1, 3)
  )

  expect_error(
    ngeo_set_time_axis(x, instant, "rate"),
    class = "ngeo_error_temporal_support"
  )
  expect_error(
    ngeo_set_time_axis(x, interval, "instantaneous"),
    class = "ngeo_error_temporal_support"
  )
  expect_s3_class(
    ngeo_set_time_axis(x, interval, "interval_mean"),
    "ngeo_points"
  )
})

test_that("temporal weights are sparse, exact, and axis-bound", {
  axis <- ngeo_time_axis(time = c(0, 1, 3, 6))
  adjacent <- ngeo_temporal_weights(axis, style = "B")
  lag_two <- ngeo_temporal_weights(
    axis, method = "lag", lag = 2, style = "B"
  )
  distance <- ngeo_temporal_weights(
    axis, method = "distance", threshold = 2, style = "B"
  )
  directed <- ngeo_temporal_weights(
    axis, directed = TRUE, style = "B"
  )

  expect_s3_class(adjacent, "ngeo_temporal_weights")
  expect_equal(
    as.matrix(adjacent$matrix),
    matrix(
      c(
        0, 1, 0, 0,
        1, 0, 1, 0,
        0, 1, 0, 1,
        0, 0, 1, 0
      ),
      nrow = 4L, byrow = TRUE
    )
  )
  expect_equal(
    which(lag_two$matrix[1L, ] != 0), 3L
  )
  expect_equal(
    as.matrix(distance$matrix),
    matrix(
      c(
        0, 1, 0, 0,
        1, 0, 1, 0,
        0, 1, 0, 0,
        0, 0, 0, 0
      ),
      nrow = 4L, byrow = TRUE
    )
  )
  expect_equal(ngeo_temporal_neighbors(axis)[[2L]], c(1L, 3L))
  expect_equal(which(directed$matrix[1L, ] != 0), integer())

  changed <- adjacent
  changed$raw_matrix@x[[1L]] <- 2
  expect_error(
    ngeo_validate_temporal_weights(changed),
    class = "ngeo_error_temporal_weights"
  )
})

test_that("matrix-free spatiotemporal lag matches Kronecker references", {
  x <- time_points_fixture()
  spatial <- ngeo_weights(
    x, method = "distance_band", threshold = 1.1, style = "B"
  )
  temporal <- ngeo_temporal_weights(
    ngeo_get_time_axis(x), style = "B"
  )

  for (combination in c("sum", "product")) {
    weights <- ngeo_spatiotemporal_weights(
      spatial, temporal, combination = combination,
      spatial_scale = 0.25
    )
    expect_silent(ngeo_validate_spatiotemporal_weights(weights))
    reference <- ngeo_materialize_spatiotemporal_weights(weights)
    observed <- ngeo_spatiotemporal_lag(x, weights)
    expected <- matrix(
      as.numeric(reference %*% as.numeric(x$values)),
      nrow = nrow(x$values)
    )

    expect_equal(unname(observed), expected, tolerance = 1e-12)
    expect_false(weights$matrix_materialized)
    expect_false("matrix" %in% names(weights))
    expect_identical(weights$domain_hash, ngeo_domain_hash(x))
    expect_identical(
      weights$axis_hash, ngeo_get_time_axis(x)$axis_hash
    )
  }

  weights <- ngeo_spatiotemporal_weights(spatial, temporal)
  expect_error(
    ngeo_materialize_spatiotemporal_weights(
      weights, max_observations = 5
    ),
    class = "ngeo_error_resource"
  )
  expect_error(
    ngeo_spatiotemporal_lag(
      x, weights,
      budget = ngeo_resource_budget(materialized_elements = 5)
    ),
    class = "ngeo_error_resource"
  )
})

test_that("temporal and spatiotemporal Moran agree with references", {
  x <- time_points_fixture()
  temporal <- ngeo_temporal_weights(
    ngeo_get_time_axis(x), style = "B"
  )
  spatial <- ngeo_weights(
    x, method = "distance_band", threshold = 1.1, style = "B"
  )
  weights <- ngeo_spatiotemporal_weights(
    spatial, temporal, spatial_scale = 0.4
  )

  temporal_result <- ngeo_temporal_moran(x, temporal)
  expect_equal(
    temporal_result$moran_i[[1L]],
    neurogeo:::.ngeo_moran_value(
      as.numeric(x$values[1L, ]), temporal$matrix
    ),
    tolerance = 1e-12
  )

  result <- ngeo_spatiotemporal_moran(
    x, weights, permutations = 19, seed = 33
  )
  reference <- ngeo_materialize_spatiotemporal_weights(weights)
  expect_s3_class(result, "ngeo_spatiotemporal_moran")
  expect_equal(
    result$estimate,
    neurogeo:::.ngeo_moran_value(
      as.numeric(x$values), reference
    ),
    tolerance = 1e-12
  )
  expect_identical(result$permutations, 19L)
  expect_false(result$matrix_materialized)
  expect_equal(
    result$simulated,
    ngeo_spatiotemporal_moran(
      x, weights, permutations = 19, seed = 33
    )$simulated
  )
})

test_that("temporal variograms have exact bounded pair accounting", {
  x <- time_points_fixture()
  temporal <- ngeo_temporal_variogram(x, breaks = 3)
  joint <- ngeo_spatiotemporal_variogram(
    x,
    spatial_distance = as.matrix(stats::dist(
      matrix(c(0, 0, 1, 0, 2, 0), ncol = 2L, byrow = TRUE),
      diag = TRUE, upper = TRUE
    )),
    spatial_breaks = 2,
    temporal_breaks = 2
  )

  expect_s3_class(temporal, "ngeo_temporal_variogram")
  expect_identical(attr(temporal, "pair_count"), 18)
  expect_equal(sum(temporal$n_pairs), 18)
  expect_s3_class(joint, "ngeo_spatiotemporal_variogram")
  expect_identical(attr(joint, "pair_count"), 66)
  expect_equal(sum(joint$n_pairs), 66)

  expect_error(
    ngeo_temporal_variogram(x, max_pairs = 17),
    class = "ngeo_error_resource"
  )
  expect_error(
    ngeo_spatiotemporal_variogram(x, max_pairs = 65),
    class = "ngeo_error_resource"
  )
})

test_that("longitudinal helpers obey temporal measurement support", {
  time <- c(0, 1, 3, 6)
  values <- rbind(
    2 + 3 * time,
    5 - 2 * time,
    rep(4, length(time))
  )
  instant <- time_points_fixture(values = values, time = time)
  trend <- ngeo_temporal_trend(instant)
  change <- ngeo_longitudinal_change(
    instant, from = 1, to = 4, scale = "rate"
  )
  mean_map <- ngeo_temporal_contrast(instant, operation = "mean")

  expect_equal(trend$values[, "intercept"], c(2, 5, 4))
  expect_equal(trend$values[, "slope"], c(3, -2, 0))
  expect_equal(as.numeric(change$values), c(3, -2, 0))
  expect_equal(as.numeric(mean_map$values), rowMeans(values))
  expect_identical(ngeo_domain_hash(trend), ngeo_domain_hash(instant))
  expect_identical(nrow(trend$domain$elements), nrow(values))

  interval_axis <- ngeo_time_axis(
    time = c(0.5, 2),
    interval_start = c(0, 1),
    interval_end = c(1, 3)
  )
  base <- ngeo_points(
    matrix(c(0, 0, 1, 0), ncol = 2L, byrow = TRUE),
    values = matrix(c(2, 4, 6, 8), nrow = 2L)
  )
  rate <- ngeo_set_time_axis(base, interval_axis, "rate")
  total <- ngeo_set_time_axis(base, interval_axis, "interval_total")

  expect_equal(
    as.numeric(ngeo_temporal_contrast(
      rate, operation = "integral"
    )$values),
    c(14, 20)
  )
  expect_equal(
    as.numeric(ngeo_temporal_contrast(
      total, operation = "sum"
    )$values),
    c(8, 12)
  )
  expect_error(
    ngeo_temporal_contrast(total, operation = "mean"),
    class = "ngeo_error_temporal_support"
  )
  expect_error(
    ngeo_temporal_contrast(instant, operation = "integral"),
    class = "ngeo_error_temporal_support"
  )
})

test_that("3.3 schemas cover temporal objects", {
  axis <- ngeo_time_axis(time = c(0, 1, 2))
  x <- time_points_fixture(
    values = matrix(1:9, nrow = 3L),
    time = c(0, 1, 2)
  )
  temporal <- ngeo_temporal_weights(axis)
  spatial <- ngeo_weights(
    x, method = "distance_band", threshold = 1.1
  )
  joint <- ngeo_spatiotemporal_weights(spatial, temporal)

  expect_true(all(vapply(
    list(axis, temporal, joint),
    function(object) {
      ngeo_validate(object)
      TRUE
    },
    logical(1)
  )))
  expect_identical(
    ngeo_object_manifest(axis)$object_schema,
    "ngcs/time-axis"
  )
})

test_that("temporal arithmetic enforces and transforms measurement units", {
  coordinates <- matrix(c(0, 0), ncol = 2)
  incompatible <- ngeo_points(
    coordinates,
    values = matrix(c(-2, -1), nrow = 1),
    maps = data.frame(name = c("t1", "t2")),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive", units = "mm"),
      ngeo_measure(spatial_semantics = "intensive", units = "second")
    )
  )
  axis <- ngeo_time_axis(time = c(0, 1), unit = "second")
  expect_error(
    ngeo_set_time_axis(incompatible, axis),
    class = "ngeo_error_temporal_measure"
  )

  compatible <- incompatible
  compatible$measures$units <- "mm"
  compatible <- ngeo_set_time_axis(compatible, axis)
  percent <- ngeo_longitudinal_change(
    compatible, scale = "percent"
  )
  rate <- ngeo_longitudinal_change(compatible, scale = "rate")
  trend <- ngeo_temporal_trend(compatible)

  expect_equal(as.numeric(percent$values), -50)
  expect_identical(percent$measures$units, "percent")
  expect_identical(rate$measures$units, "mm/second")
  expect_identical(trend$measures$units, c("mm", "mm/second"))
})
