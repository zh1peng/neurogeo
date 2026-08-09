weighted_support_fixture <- function(source_support) {
  source <- ngeo_point(
    cbind(x = 0:3, y = 0),
    values = cbind(
      outcome = c(0, 3, 6, 30),
      predictor = 0:3
    ),
    measures = rbind(
      ngeo_measure(support_behavior = "intensive"),
      ngeo_measure(support_behavior = "intensive")
    )
  )
  target <- ngeo_parcellation(
    data.frame(region_id = paste0("r", 1:4)),
    support_size = rep(NA_real_, 4),
    coordinate_space = source$base$coordinate_space
  )
  support_map <- ngeo_support_map(
    source,
    target,
    Matrix::Diagonal(4),
    source_support = source_support
  )
  list(source = source, target = target, support_map = support_map)
}

test_that("ambiguous layer names never select the first match silently", {
  x <- ngeo_point(
    cbind(x = 0:2, y = 0),
    values = cbind(first = 1:3, second = 4:6),
    layers = data.frame(
      layer_id = c("first-id", "second-id"),
      name = c("signal", "signal")
    )
  )

  expect_error(
    values(x, layers = "signal"),
    "first-id, second-id",
    class = "ngeo_error_layer_ambiguous"
  )
  expect_equal(
    as.numeric(values(x, layers = "second-id")),
    4:6
  )
})

test_that("support effects use declared unequal support weights", {
  values_matrix <- cbind(
    outcome = c(0, 3, 6, 30),
    predictor = 0:3
  )
  support <- c(100, 100, 100, 1)
  frame <- data.frame(
    outcome = values_matrix[, "outcome"],
    predictor = values_matrix[, "predictor"]
  )
  reference <- summary(stats::lm(
    outcome ~ predictor,
    data = frame,
    weights = support
  ))$coefficients["predictor", ]

  observed <- expect_silent(
    neurogeo:::.ngeo_fit_support_effect(values_matrix, support)
  )
  expect_equal(
    unname(observed[c("estimate", "standard_error", "statistic", "p_value")]),
    unname(reference[c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]),
    tolerance = 1e-12
  )
})

test_that("all support slope facades retain support-weighted estimates", {
  support <- c(100, 100, 100, 1)
  fixture <- weighted_support_fixture(support)
  reference <- coef(stats::lm(
    c(0, 3, 6, 30) ~ I(0:3),
    weights = support
  ))[[2L]]
  maps <- list(weighted = fixture$support_map)
  targets <- list(fixture$target)

  atlas <- ngeo_atlas_robust_effect(
    fixture$source, maps, targets, "outcome", "predictor"
  )
  support_test <- ngeo_support_test(
    fixture$source, maps, targets, "outcome", "predictor",
    statistic = "slope", nsim = 3, seed = 1
  )
  common <- ngeo_common_support_test(
    fixture$source, maps, targets, "outcome", "predictor",
    statistic = "slope", nsim = 3, seed = 1
  )
  alternative <- weighted_support_fixture(rev(support))$support_map
  boundary <- ngeo_boundary_test(
    fixture$source,
    ngeo_support_ensemble(
      list(fixture$support_map, alternative),
      kind = "segmentation"
    ),
    fixture$target,
    "outcome",
    "predictor",
    nsim = 3,
    seed = 1
  )

  expect_equal(atlas$estimates$estimate, reference, tolerance = 1e-12)
  expect_equal(support_test$estimates$statistic, reference, tolerance = 1e-12)
  expect_equal(common$estimates$statistic, reference, tolerance = 1e-12)
  expect_equal(boundary$effects$estimate[[1L]], reference, tolerance = 1e-12)
})

test_that("failure-safe overwrite restores the previous output", {
  path <- tempfile(fileext = ".txt")
  writeLines("old", path)
  calls <- 0L
  operations <- list(
    rename = function(from, to) {
      calls <<- calls + 1L
      if (calls == 2L) {
        return(FALSE)
      }
      file.rename(from, to)
    },
    copy = file.copy,
    unlink = unlink
  )

  expect_error(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("new", target),
      overwrite = TRUE,
      .operations = operations
    ),
    class = "ngeo_error_io"
  )
  expect_identical(readLines(path), "old")
  expect_false(file.exists(neurogeo:::.ngeo_atomic_backup_path(path)))
})

test_that("failure-safe overwrite recovers an interrupted backup", {
  path <- tempfile(fileext = ".txt")
  backup <- neurogeo:::.ngeo_atomic_backup_path(path)
  writeLines("old", backup)

  expect_error(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("new", target)
    ),
    class = "ngeo_error_overwrite"
  )
  expect_identical(readLines(path), "old")
  expect_false(file.exists(backup))
})

test_that("failure-safe overwrite preserves output when backup rename fails", {
  path <- tempfile(fileext = ".txt")
  writeLines("old", path)
  operations <- list(
    rename = function(...) FALSE,
    copy = file.copy,
    unlink = unlink
  )

  expect_error(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("new", target),
      overwrite = TRUE,
      .operations = operations
    ),
    class = "ngeo_error_io"
  )
  expect_identical(readLines(path), "old")
  expect_false(file.exists(neurogeo:::.ngeo_atomic_backup_path(path)))
})

test_that("failure-safe overwrite copies backup when rollback rename fails", {
  path <- tempfile(fileext = ".txt")
  writeLines("old", path)
  calls <- 0L
  operations <- list(
    rename = function(from, to) {
      calls <<- calls + 1L
      if (calls >= 2L) {
        return(FALSE)
      }
      file.rename(from, to)
    },
    copy = file.copy,
    unlink = unlink
  )

  expect_error(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("new", target),
      overwrite = TRUE,
      .operations = operations
    ),
    class = "ngeo_error_io"
  )
  expect_identical(readLines(path), "old")
  expect_false(file.exists(neurogeo:::.ngeo_atomic_backup_path(path)))
})

test_that("failed rollback leaves a recoverable backup", {
  path <- tempfile(fileext = ".txt")
  backup <- neurogeo:::.ngeo_atomic_backup_path(path)
  writeLines("old", path)
  calls <- 0L
  operations <- list(
    rename = function(from, to) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(file.rename(from, to))
      }
      FALSE
    },
    copy = function(...) FALSE,
    unlink = unlink
  )

  expect_error(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("new", target),
      overwrite = TRUE,
      .operations = operations
    ),
    class = "ngeo_error_io_recovery"
  )
  expect_false(file.exists(path))
  expect_identical(readLines(backup), "old")
  expect_error(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("unused", target)
    ),
    class = "ngeo_error_overwrite"
  )
  expect_identical(readLines(path), "old")
  expect_false(file.exists(backup))
})

test_that("cleanup failure keeps both bytes and is healed next time", {
  path <- tempfile(fileext = ".txt")
  backup <- neurogeo:::.ngeo_atomic_backup_path(path)
  writeLines("old", path)
  operations <- list(
    rename = file.rename,
    copy = file.copy,
    unlink = function(target, ...) {
      if (identical(normalizePath(target, mustWork = FALSE),
                    normalizePath(backup, mustWork = FALSE))) {
        return(1L)
      }
      unlink(target, ...)
    }
  )

  expect_warning(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("new", target),
      overwrite = TRUE,
      .operations = operations
    ),
    class = "ngeo_warning_io_cleanup"
  )
  expect_identical(readLines(path), "new")
  expect_identical(readLines(backup), "old")
  expect_error(
    neurogeo:::.ngeo_atomic_write(
      path,
      function(target) writeLines("unused", target)
    ),
    class = "ngeo_error_overwrite"
  )
  expect_identical(readLines(path), "new")
  expect_false(file.exists(backup))
})

test_that("Haar rotations satisfy pre-registered finite-sample moments", {
  set.seed(6002)
  n <- 6000L
  rotations <- replicate(n, neurogeo:::.ngeo_rotation_matrix())
  element_mean <- apply(rotations, c(1L, 2L), mean)
  element_second <- apply(rotations^2, c(1L, 2L), mean)
  simultaneous_mean_tolerance <- 6 * sqrt(1 / (3 * n))

  expect_lt(max(abs(element_mean)), simultaneous_mean_tolerance)
  expect_lt(max(abs(element_second - 1 / 3)), 0.03)
  for (i in seq_len(20L)) {
    rotation <- rotations[, , i]
    expect_equal(crossprod(rotation), diag(3), tolerance = 1e-12)
    expect_equal(det(rotation), 1, tolerance = 1e-12)
  }
})

test_that("spin strata prevent cross-component mapping and report collisions", {
  skip_if_not_installed("dbscan")
  tetrahedron <- rbind(
    c(1, 1, 1), c(1, -1, -1),
    c(-1, 1, -1), c(-1, -1, 1)
  ) / sqrt(3)
  sphere <- rbind(tetrahedron, tetrahedron)
  anatomical <- rbind(tetrahedron, tetrahedron + c(10, 0, 0))
  faces <- rbind(
    c(1, 2, 3), c(1, 2, 4), c(1, 3, 4), c(2, 3, 4),
    c(5, 6, 7), c(5, 6, 8), c(5, 7, 8), c(6, 7, 8)
  )
  x <- ngeo_surface(
    list(anatomical = anatomical, sphere = sphere),
    faces,
    values = cbind(signal = seq_len(8)),
    coordinate_roles = c("anatomical", "registration")
  )

  expect_error(
    ngeo_spin_null(
      x, coordinates = "sphere", nsim = 2, seed = 2,
      experimental = TRUE
    ),
    class = "ngeo_error_strata_required"
  )
  observed <- ngeo_spin_null(
    x, coordinates = "sphere", strata = rep(c("left", "right"), each = 4),
    nsim = 8, seed = 2, experimental = TRUE
  )
  expect_true(all(observed$mapping_diagnostics$cross_stratum == 0L))
  expect_equal(
    observed$mapping_diagnostics$collisions,
    8L - observed$mapping_diagnostics$unique_targets
  )
  expect_identical(observed$mapping_policy, "nearest_with_replacement")
})

test_that("eigen-sign surrogate discloses failed irregular-graph invariants", {
  x <- ngeo_point(
    cbind(x = c(0, 1, 2, 5, 9), y = 0),
    values = cbind(signal = c(-2, 0, 1, 4, 10))
  )
  spatial_weights <- ngeo_spatial_weights(
    x, method = "knn", k = 2, symmetry = "union", style = "W"
  )
  observed <- ngeo_moran_null(
    x, spatial_weights, "signal", nsim = 12, seed = 6003,
    zero_policy = TRUE, experimental = TRUE
  )
  source <- x$values[, "signal"]
  simulated_moran <- apply(
    observed$simulations,
    2L,
    neurogeo:::.ngeo_moran_value,
    matrix = spatial_weights$matrix
  )

  expect_true(any(abs(colMeans(observed$simulations) - mean(source)) > 1e-8))
  expect_true(any(abs(
    apply(observed$simulations, 2L, stats::var) - stats::var(source)
  ) > 1e-8))
  expect_true(any(abs(simulated_moran - observed$observed_moran) > 1e-8))
  expect_false(observed$preserves_spatial_autocorrelation)
})

test_that("metric roles, components, and zero distances fail safely", {
  sphere <- rbind(
    c(1, 1, 1), c(1, -1, -1),
    c(-1, 1, -1), c(-1, -1, 1)
  ) / sqrt(3)
  registration_active <- ngeo_surface(
    list(sphere = sphere, anatomical = sphere * 10),
    rbind(c(1, 2, 3), c(1, 2, 4), c(1, 3, 4), c(2, 3, 4)),
    coordinate_roles = c("registration", "anatomical"),
    active_coordinates = "sphere"
  )
  expect_error(
    ngeo_distance(registration_active, 1, 2, "euclidean"),
    class = "ngeo_error_metric"
  )

  disconnected <- ngeo_surface(
    rbind(sphere, sphere + c(10, 0, 0)),
    rbind(c(1, 2, 3), c(1, 2, 4), c(5, 6, 7), c(5, 6, 8))
  )
  graph_knn <- ngeo_spatial_weights(
    disconnected, method = "knn", k = 1, style = "B"
  )
  expect_identical(graph_knn$distance_method, "edge_geodesic")
  expect_equal(sum(graph_knn$matrix[1:4, 5:8]), 0)
  expect_equal(sum(graph_knn$matrix[5:8, 1:4]), 0)

  duplicate <- ngeo_point(
    rbind(c(0, 0), c(0, 0), c(1, 0))
  )
  expect_error(
    ngeo_spatial_weights(
      duplicate, method = "inverse_distance", k = 1, style = "none"
    ),
    class = "ngeo_error_metric"
  )
})

test_that("kriging rejects unsafe covariance and variance results", {
  point <- ngeo_point(
    rbind(c(0, 0), c(0, 0), c(1, 0)),
    values = cbind(signal = c(1, 2, 3)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  fit <- structure(
    list(
      model = "gaussian",
      parameters = c(nugget = 0, partial_sill = 1, range = 2)
    ),
    class = "ngeo_variogram_fit"
  )
  expect_error(
    ngeo_kriging(
      point, "signal", fit, targets = matrix(c(0.5, 0, 0), nrow = 1),
      neighbors = 3, distance_method = "euclidean"
    ),
    class = "ngeo_error_covariance_condition"
  )
  expect_equal(
    neurogeo:::.ngeo_kriging_variance_gate(-1e-12, 1),
    0
  )
  expect_error(
    neurogeo:::.ngeo_kriging_variance_gate(-1e-3, 1),
    class = "ngeo_error_variance"
  )
})
