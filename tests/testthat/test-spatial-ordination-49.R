ordination_fixture_49 <- function(n = 25L) {
  coordinates <- cbind(seq_len(n), sin(seq_len(n) / 3))
  layer_a <- scale(coordinates[, 1])[, 1]
  layer_b <- 0.7 * layer_a + 0.3 * scale(coordinates[, 2])[, 1]
  x <- ngeo_points(
    coordinates,
    values = cbind(layer_a = layer_a, layer_b = layer_b)
  )
  weights <- ngeo_weights(
    x, method = "knn", k = 2L, style = "W", symmetry = "union"
  )
  list(x = x, weights = weights)
}

test_that("experimental spatial ordination delegates to MULTISPATI", {
  skip_if_not_installed("adespatial")
  skip_if_not_installed("ade4")
  skip_if_not_installed("spdep")
  fixture <- ordination_fixture_49()

  fit <- ngeo_spatial_ordination(
    fixture$x, c("layer_a", "layer_b"), fixture$weights, axes = 2L
  )

  expect_s3_class(fit, "ngeo_spatial_ordination")
  expect_identical(fit$backend, "adespatial::multispati")
  expect_false(fit$population_inference)
  expect_identical(fit$regime, "reference_map")
  expect_equal(nrow(fit$training_scores), nrow(fixture$x$domain$elements))
  expect_equal(nrow(fit$loadings), 2L)
  expect_identical(fit$domain_hash, ngeo_domain_hash(fixture$x))
  expect_match(fit$ordination_hash, "^[0-9a-f]{64}$")
  expect_output(print(fit), "population inference: FALSE")

  test <- fixture$x
  frozen <- ngeo_spatial_ordination(
    fixture$x, c("layer_a", "layer_b"), fixture$weights,
    axes = 1L, regime = "frozen_training", newdata = list(test = test),
    independent_training = TRUE
  )
  expect_true(frozen$independent_training)
  expect_equal(frozen$projected$test, frozen$training_scores, tolerance = 1e-10)

  expect_error(
    ngeo_spatial_ordination(
      fixture$x, 1:2, fixture$weights, regime = "frozen_training",
      newdata = list(test = test)
    ),
    class = "ngeo_error_inference_leakage"
  )
})

test_that("ordination rejects stale weights and categorical layers", {
  skip_if_not_installed("adespatial")
  skip_if_not_installed("ade4")
  skip_if_not_installed("spdep")
  fixture <- ordination_fixture_49()
  other <- ordination_fixture_49(20L)
  expect_error(
    ngeo_spatial_ordination(fixture$x, 1:2, other$weights),
    class = "ngeo_error_domain_mismatch"
  )

  categorical <- fixture$x
  categorical$measures$spatial_semantics[[1L]] <- "categorical"
  expect_error(
    ngeo_spatial_ordination(categorical, 1:2, fixture$weights),
    class = "ngeo_error_measure"
  )
})

test_that("experimental ordination boundaries fail before inference", {
  skip_if_not_installed("adespatial")
  skip_if_not_installed("ade4")
  skip_if_not_installed("spdep")
  fixture <- ordination_fixture_49()
  empty <- ngeo_points(cbind(x = 1:4, y = 0))
  expect_error(
    ngeo_spatial_ordination(empty, 1:2, fixture$weights),
    class = "ngeo_error_values"
  )
  expect_error(
    ngeo_spatial_ordination(fixture$x, 1L, fixture$weights),
    class = "ngeo_error_argument"
  )
  missing <- fixture$x
  missing$values[[1L]] <- NA_real_
  expect_error(
    ngeo_spatial_ordination(missing, 1:2, fixture$weights),
    class = "ngeo_error_missing"
  )
  constant <- fixture$x
  constant$values[, 1L] <- 1
  expect_error(
    ngeo_spatial_ordination(constant, 1:2, fixture$weights),
    class = "ngeo_error_measure"
  )
  expect_error(
    ngeo_spatial_ordination(fixture$x, 1:2, "not-weights"),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_spatial_ordination(
      fixture$x, 1:2, fixture$weights, axes = 0L
    ),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_spatial_ordination(
      fixture$x, 1:2, fixture$weights, newdata = list(test = fixture$x)
    ),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_spatial_ordination(
      fixture$x, 1:2, fixture$weights, regime = "frozen_training",
      newdata = list(), independent_training = TRUE
    ),
    class = "ngeo_error_argument"
  )

  model <- list(
    domain_hash = ngeo_domain_hash(fixture$x),
    layer_names = c("one", "two", "three"), center = c(0, 0, 0),
    scale = c(1, 1, 1), loadings = matrix(1, 3, 1)
  )
  expect_error(
    neurogeo:::.ngeo_ordination_projection(
      ordination_fixture_49(20L)$x, 1:2, model
    ),
    class = "ngeo_error_domain_mismatch"
  )
  expect_error(
    neurogeo:::.ngeo_ordination_projection(fixture$x, 1:2, model),
    class = "ngeo_error_alignment"
  )
})

test_that("sampled cross-variograms are deterministic and auditable", {
  fixture <- ordination_fixture_49(30L)
  one <- neurogeo:::.ngeo_cross_variograms(
    fixture$x, 1:2, pair_sample = 100L, breaks = 5L, seed = 49L
  )
  two <- neurogeo:::.ngeo_cross_variograms(
    fixture$x, 1:2, pair_sample = 100L, breaks = 5L, seed = 49L
  )

  expect_equal(one$table, two$table, tolerance = 0)
  expect_identical(one$pair_ranks, two$pair_ranks)
  expect_identical(one$sampling$design, "uniform_without_replacement")
  expect_identical(one$sampling$seed, 49L)
  expect_identical(one$sampling$requested_pairs, 100L)
  expect_identical(one$metric, "euclidean")
  expect_match(one$sampling$hash, "^[0-9a-f]{64}$")

  expect_error(
    neurogeo:::.ngeo_cross_variograms(
      fixture$x, 1:2, pair_sample = 1000L, breaks = 5L, seed = 49L
    ),
    class = "ngeo_error_resource"
  )
})

test_that("cross-variogram sampling rejects ambiguous inputs", {
  fixture <- ordination_fixture_49(12L)
  expect_error(
    neurogeo:::.ngeo_cross_variograms(
      fixture$x, 1:2, pair_sample = 10L, breaks = 0L, seed = 1L
    ),
    class = "ngeo_error_argument"
  )
  expect_error(
    neurogeo:::.ngeo_cross_variograms(
      fixture$x, 1:2, pair_sample = 10L,
      breaks = c(0.5, 0.25, 10), seed = 1L
    ),
    class = "ngeo_error_argument"
  )
  expect_error(
    neurogeo:::.ngeo_cross_variograms(
      ngeo_points(cbind(x = 1:4, y = 0)), 1:2,
      pair_sample = 2L, seed = 1L
    ),
    class = "ngeo_error_values"
  )
  expect_error(
    neurogeo:::.ngeo_cross_variograms(
      fixture$x, 1L, pair_sample = 10L, seed = 1L
    ),
    class = "ngeo_error_argument"
  )
  invalid <- fixture$x
  invalid$values[[1L]] <- Inf
  expect_error(
    neurogeo:::.ngeo_cross_variograms(
      invalid, 1:2, pair_sample = 10L, seed = 1L
    ),
    class = "ngeo_error_missing"
  )
  categorical <- fixture$x
  categorical$measures$spatial_semantics[[1L]] <- "categorical"
  expect_error(
    neurogeo:::.ngeo_cross_variograms(
      categorical, 1:2, pair_sample = 10L, seed = 1L
    ),
    class = "ngeo_error_measure"
  )
})

test_that("complete pair cross-variograms match gstat", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sf")
  fixture <- ordination_fixture_49(16L)
  total <- 16L * 15L / 2L
  observed <- neurogeo:::.ngeo_cross_variograms(
    fixture$x, 1:2, pair_sample = total, breaks = 4L, seed = 9L
  )

  coordinates <- neurogeo:::.ngeo_element_coordinates(fixture$x)
  data <- data.frame(
    L1 = fixture$x$values[, 1], L2 = fixture$x$values[, 2],
    x = coordinates[, 1], y = coordinates[, 2]
  )
  spatial <- sf::st_as_sf(data, coords = c("x", "y"))
  model <- gstat::gstat(id = "L1", formula = L1 ~ 1, data = spatial)
  model <- gstat::gstat(model, id = "L2", formula = L2 ~ 1, data = spatial)
  reference <- gstat::variogram(model, boundaries = observed$boundaries)

  normalize <- function(value) {
    value <- as.data.frame(value)[, c("id", "np", "dist", "gamma")]
    value$id <- as.character(value$id)
    value <- value[order(value$id, value$dist), , drop = FALSE]
    rownames(value) <- NULL
    value
  }
  expect_equal(normalize(observed$table), normalize(reference), tolerance = 1e-12)
})

test_that("experimental LMC returns PSD sill diagnostics", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sf")
  fixture <- ordination_fixture_49(35L)
  fit <- ngeo_coregionalization(
    fixture$x, 1:2, pair_sample = 300L, breaks = 6L,
    model = "Exp", range = 8, seed = 7L
  )

  expect_s3_class(fit, "ngeo_coregionalization")
  expect_identical(fit$status, "experimental")
  expect_true(all(fit$psd_diagnostics$min_eigenvalue >= -1e-8))
  expect_true(all(fit$psd_diagnostics$positive_semidefinite))
  expect_identical(fit$assumptions$stationarity, "second_order")
  expect_true(fit$assumptions$isotropy)
  expect_identical(fit$empirical$sampling$seed, 7L)
  expect_false(fit$capabilities$co_kriging)
  expect_match(fit$model_hash, "^[0-9a-f]{64}$")
  expect_output(print(fit), "co-kriging: FALSE")
})

test_that("experimental LMC enforces its bounded domain and model", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sf")
  surface <- ngeo_surface(
    rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0)),
    matrix(c(1, 2, 3), nrow = 1L),
    values = cbind(a = c(1, 2, 3), b = c(3, 2, 1)),
    index_base = "one"
  )
  expect_error(
    ngeo_coregionalization(
      surface, 1:2, pair_sample = 3L, range = 1
    ),
    class = "ngeo_error_capability"
  )
  fixture <- ordination_fixture_49(12L)
  expect_error(
    ngeo_coregionalization(
      fixture$x, 1:2, pair_sample = 20L, range = 0
    ),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_coregionalization(
      fixture$x, 1:2, pair_sample = 20L, range = 2, nugget = -1
    ),
    class = "ngeo_error_argument"
  )
})

test_that("experimental MGWR uses fixed term bandwidths without p maps", {
  skip_if_not_installed("GWmodel")
  skip_if_not_installed("sf")
  set.seed(49)
  n <- 30L
  coordinates <- cbind(runif(n), runif(n))
  p1 <- rnorm(n)
  p2 <- rnorm(n)
  x <- ngeo_points(
    coordinates,
    values = cbind(response = 1 + 2 * p1 - p2 + rnorm(n, sd = 0.2), p1, p2)
  )
  fit <- ngeo_mgwr(
    x, "response", c("p1", "p2"), bandwidths = c(0.8, 0.6, 0.7),
    max_iterations = 5L
  )

  expect_s3_class(fit, "ngeo_mgwr")
  expect_identical(fit$status, "experimental_not_promoted")
  expect_equal(unname(fit$bandwidths), c(0.8, 0.6, 0.7))
  expect_equal(nrow(fit$local), n)
  expect_true(all(c("effective_n", "condition_number") %in% names(fit$local)))
  expect_false(any(grepl("p.value|TV|statistic", names(fit$local))))
  expect_false(fit$inference$nominal_local_p_values)
  expect_match(fit$model_hash, "^[0-9a-f]{64}$")
  expect_output(print(fit), "nominal local p-values: FALSE")

  old <- options(neurogeo.max_mgwr_elements = 20L)
  on.exit(options(old), add = TRUE)
  expect_error(
    ngeo_mgwr(x, "response", c("p1", "p2"), c(0.8, 0.6, 0.7)),
    class = "ngeo_error_resource"
  )
})

test_that("experimental MGWR validates model dimensions and values", {
  skip_if_not_installed("GWmodel")
  skip_if_not_installed("sf")
  x <- ngeo_points(
    cbind(x = 1:8, y = sin(1:8)),
    values = cbind(response = 1:8, predictor = rep(c(0, 1), 4))
  )
  expect_error(
    ngeo_mgwr(x, "response", integer(), 1),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_mgwr(x, "response", "predictor", 1),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_mgwr(
      x, "response", "predictor", c(1, 1), max_iterations = 0L
    ),
    class = "ngeo_error_argument"
  )
  missing <- x
  missing$values[[1L]] <- NA_real_
  expect_error(
    ngeo_mgwr(missing, "response", "predictor", c(1, 1)),
    class = "ngeo_error_missing"
  )
  tiny <- ngeo_points(
    cbind(x = 1:4, y = 0),
    values = cbind(response = 1:4, predictor = c(0, 1, 0, 1))
  )
  expect_error(
    ngeo_mgwr(tiny, "response", "predictor", c(1, 1)),
    class = "ngeo_error_model"
  )
})
