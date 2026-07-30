statistics_grid <- function() {
  coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
  values <- cbind(signal = c(1, 2, 3, 2, 4, 7, 3, 7, 9))
  x <- ngeo_points(
    coordinates,
    values = values,
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  weights <- ngeo_weights(
    x,
    method = "distance_band",
    threshold = 1.01,
    style = "W"
  )
  list(x = x, weights = weights)
}

test_that("global Moran and Geary match spdep reference values", {
  skip_if_not_installed("spdep")
  fixture <- statistics_grid()
  values <- as.numeric(fixture$x$values[, 1L])
  listw <- as_spdep_listw(fixture$weights)
  n <- length(values)
  s0 <- spdep::Szero(listw)

  moran <- ngeo_moran(fixture$x, fixture$weights)
  geary <- ngeo_geary(fixture$x, fixture$weights)
  reference_moran <- spdep::moran(
    values,
    listw,
    n = n,
    S0 = s0,
    zero.policy = TRUE
  )
  reference_geary <- spdep::geary(
    values,
    listw,
    n = n,
    n1 = n - 1L,
    S0 = s0,
    zero.policy = TRUE
  )

  expect_equal(moran$estimate, unname(reference_moran$I), tolerance = 1e-12)
  expect_equal(geary$estimate, unname(reference_geary$C), tolerance = 1e-12)
  expect_equal(moran$expectation, -1 / (n - 1L))
  expect_equal(geary$expectation, 1)
})

test_that("local Moran statistics match spdep and identify quadrants", {
  skip_if_not_installed("spdep")
  fixture <- statistics_grid()
  values <- as.numeric(fixture$x$values[, 1L])
  listw <- as_spdep_listw(fixture$weights)

  result <- ngeo_local_moran(fixture$x, fixture$weights)
  reference <- spdep::localmoran(
    values,
    listw,
    zero.policy = TRUE,
    mlvar = TRUE
  )

  expect_s3_class(result, "ngeo_lisa")
  expect_equal(
    result$local_i,
    unname(reference[, "Ii"]),
    tolerance = 1e-12
  )
  expect_equal(
    result$expectation,
    unname(reference[, "E.Ii"]),
    tolerance = 1e-12
  )
  expect_true(all(result$cluster %in% c(
    "high-high", "high-low", "low-high", "low-low"
  )))
})

test_that("permutation inference is reproducible and preserves caller RNG", {
  fixture <- statistics_grid()
  set.seed(42)
  state <- .Random.seed
  first <- ngeo_moran(
    fixture$x,
    fixture$weights,
    permutations = 99,
    seed = 2026
  )
  expect_identical(.Random.seed, state)
  second <- ngeo_moran(
    fixture$x,
    fixture$weights,
    permutations = 99,
    seed = 2026
  )
  local_first <- ngeo_local_moran(
    fixture$x,
    fixture$weights,
    permutations = 49,
    seed = 7
  )
  local_second <- ngeo_local_moran(
    fixture$x,
    fixture$weights,
    permutations = 49,
    seed = 7
  )

  expect_identical(first$simulated, second$simulated)
  expect_identical(first$p.value, second$p.value)
  expect_identical(local_first$p.value, local_second$p.value)
  expect_identical(attr(local_first, "null_model"), "conditional")
  expect_true("significant" %in% names(local_first))
  expect_true(first$p.value >= 0 && first$p.value <= 1)
})

test_that("Local Moran declares conditional and total randomization nulls", {
  fixture <- statistics_grid()
  conditional <- ngeo_local_moran(
    fixture$x, fixture$weights,
    permutations = 49, seed = 11,
    null_model = "conditional"
  )
  total <- ngeo_local_moran(
    fixture$x, fixture$weights,
    permutations = 49, seed = 11,
    null_model = "total"
  )

  expect_identical(attr(conditional, "null_model"), "conditional")
  expect_identical(attr(total, "null_model"), "total")
  expect_false(isTRUE(all.equal(
    conditional$expectation, total$expectation
  )))
})

test_that("variogram boundaries must cover every retained pair", {
  x <- ngeo_points(
    cbind(0:3, 0),
    values = cbind(signal = c(1, 2, 4, 8))
  )
  expect_error(
    ngeo_variogram(x, "signal", breaks = c(0, 1.1)),
    class = "ngeo_error_argument"
  )
  result <- ngeo_variogram(x, "signal", breaks = c(0, 3))
  expect_identical(attr(result, "pair_count"), sum(result$n_pairs))
})

test_that("permutation control unifies tails and multiple testing", {
  fixture <- statistics_grid()
  control <- ngeo_permutation_control(
    permutations = 49,
    seed = 2026,
    alternative = "greater",
    adjust = "BH"
  )
  first <- ngeo_local_moran(
    fixture$x,
    fixture$weights,
    control = control
  )
  second <- ngeo_local_moran(
    fixture$x,
    fixture$weights,
    control = control
  )

  expect_s3_class(control, "ngeo_permutation_control")
  expect_identical(first$p.value, second$p.value)
  expect_equal(first$p.adjusted, stats::p.adjust(first$p.value, "BH"))
  expect_identical(attr(first, "alternative"), "greater")
  expect_identical(attr(first, "adjustment"), "BH")
  expect_error(
    ngeo_permutation_control(adjust = "not-a-method"),
    class = "ngeo_error_argument"
  )
})

test_that("Getis-Ord Gi and Gi-star match spdep reference z-scores", {
  skip_if_not_installed("spdep")
  fixture <- statistics_grid()
  values <- as.numeric(fixture$x$values[, 1L])
  nb <- as_spdep_nb(fixture$weights)
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  star_listw <- spdep::nb2listw(
    spdep::include.self(nb),
    style = "W",
    zero.policy = TRUE
  )

  gi <- ngeo_getis_ord(
    fixture$x,
    fixture$weights,
    star = FALSE,
    adjust = "BH"
  )
  gi_star <- ngeo_getis_ord(
    fixture$x,
    fixture$weights,
    star = TRUE
  )

  expect_s3_class(gi, "ngeo_getis")
  expect_equal(
    gi$z_score,
    as.numeric(spdep::localG(values, listw)),
    tolerance = 1e-12
  )
  expect_equal(
    gi_star$z_score,
    as.numeric(spdep::localG(values, star_listw)),
    tolerance = 1e-12
  )
  expect_equal(gi$p.adjusted, stats::p.adjust(gi$p.value, "BH"))
})

test_that("exact-order correlogram matches spdep graph lags", {
  skip_if_not_installed("spdep")
  fixture <- statistics_grid()
  values <- as.numeric(fixture$x$values[, 1L])
  lag_nb <- suppressWarnings(
    spdep::nblag(as_spdep_nb(fixture$weights), maxlag = 3)
  )
  reference <- vapply(lag_nb, function(nb) {
    listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    spdep::moran(
      values,
      listw,
      n = length(values),
      S0 = spdep::Szero(listw),
      zero.policy = TRUE
    )[["I"]]
  }, numeric(1))

  result <- ngeo_correlogram(
    fixture$x,
    fixture$weights,
    lags = 1:3
  )

  expect_s3_class(result, "ngeo_correlogram")
  expect_equal(result$moran_i, unname(reference), tolerance = 1e-12)
  expect_true(all(result$n_edges > 0))
})

test_that("statistics enforce domain, missing, and measurement invariants", {
  fixture <- statistics_grid()
  shifted <- statistics_grid()$x
  shifted$domain$coordinates[1L, 1L] <- 0.1
  expect_error(
    ngeo_moran(shifted, fixture$weights),
    class = "ngeo_error_domain_mismatch"
  )

  missing <- fixture$x
  missing$values[1L, 1L] <- NA_real_
  expect_error(
    ngeo_moran(missing, fixture$weights),
    class = "ngeo_error_missing"
  )
  expect_s3_class(
    ngeo_moran(
      missing,
      fixture$weights,
      na_action = "omit",
      zero_policy = TRUE
    ),
    "ngeo_global_stat"
  )

  categorical <- fixture$x
  categorical$measures$spatial_semantics <- "categorical"
  expect_error(
    ngeo_moran(categorical, fixture$weights),
    class = "ngeo_error_measure"
  )
})

test_that("empirical variogram bins pair semivariances explicitly", {
  x <- ngeo_points(
    cbind(x = 0:3, y = 0),
    values = cbind(signal = 0:3),
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  result <- ngeo_variogram(
    x,
    breaks = c(0, 1.1, 2.1, 3.1),
    metric = "euclidean"
  )

  expect_s3_class(result, "ngeo_variogram")
  expect_equal(result$n_pairs, c(3L, 2L, 1L))
  expect_equal(result$distance, c(1, 2, 3))
  expect_equal(result$semivariance, c(0.5, 2, 4.5))
  expect_equal(attr(result, "pair_count"), 6)
})

test_that("spatial diagnostic plots render", {
  fixture <- statistics_grid()
  moran <- ngeo_moran(fixture$x, fixture$weights)
  lisa <- ngeo_local_moran(fixture$x, fixture$weights)
  getis <- ngeo_getis_ord(fixture$x, fixture$weights)
  correlogram <- ngeo_correlogram(
    fixture$x,
    fixture$weights,
    lags = 1:2
  )
  variogram <- ngeo_variogram(fixture$x, breaks = 4)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)

  expect_silent(plot(moran))
  expect_silent(plot(lisa))
  expect_silent(plot(getis))
  expect_silent(plot(correlogram))
  expect_silent(plot(variogram))
})
