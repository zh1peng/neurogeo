coupling_fixture <- function(values, subjects, layers, regions = FALSE,
                             support = NULL) {
  values <- as.matrix(values)
  maps <- data.frame(
    map_id = paste0("map_", seq_len(ncol(values))),
    name = paste(subjects, layers, sep = "_"),
    subject_id = subjects,
    feature = layers,
    stringsAsFactors = FALSE
  )
  measures <- do.call(rbind, lapply(layers, function(layer) {
    ngeo_measure(
      spatial_semantics = "intensive",
      units = if (layer == "x") "mm" else "ratio"
    )
  }))
  n <- nrow(values)
  if (!regions) {
    return(ngeo_points(
      cbind(x = seq_len(n), y = 0), values = values,
      maps = maps, measures = measures
    ))
  }
  adjacency <- Matrix::sparseMatrix(
    i = c(seq_len(n - 1L), 2:n),
    j = c(2:n, seq_len(n - 1L)),
    x = 1,
    dims = c(n, n)
  )
  ngeo_regions(
    data.frame(region_id = seq_len(n)),
    values = values,
    support_size = if (is.null(support)) rep.int(1, n) else support,
    adjacency = adjacency,
    maps = maps,
    measures = measures
  )
}

coupling_weights <- function(x, style = "W") {
  if (inherits(x, "ngeo_regions")) {
    ngeo_weights(x, method = "region_contiguity", style = style)
  } else {
    ngeo_weights(
      x, method = "distance_band", threshold = 1.01, style = style
    )
  }
}

test_that("same-location coupling is support weighted and bounded", {
  signal <- c(-2, -1, 0, 1, 2)
  x <- coupling_fixture(
    cbind(signal, signal, signal, -signal),
    rep(c("s1", "s2"), each = 2L),
    rep(c("x", "y"), 2L)
  )
  index <- ngeo_validate_layers(x, complete = "error")
  result <- ngeo_layer_coupling(
    x, index, estimands = "same_location"
  )
  expect_s3_class(result, "ngeo_subject_features")
  observed <- result$values[, result$endpoints$estimand == "same_location"]
  expect_equal(unname(observed), c(1, -1), tolerance = 1e-12)
  expect_true(all(c(
    "centering", "support_weighting", "standardization", "direction",
    "bounds", "units", "null_target"
  ) %in% names(result$endpoints)))

  support <- c(1, 2, 7, 3, 1)
  first <- c(0, 0, 1, 2, 4)
  second <- c(4, 1, 1, 0, 0)
  regional <- coupling_fixture(
    cbind(first, second), "s1", c("x", "y"),
    regions = TRUE, support = support
  )
  expected <- sum(support * (first - weighted.mean(first, support)) *
    (second - weighted.mean(second, support))) /
    sqrt(sum(support * (first - weighted.mean(first, support))^2) *
      sum(support * (second - weighted.mean(second, support))^2))
  weighted <- ngeo_layer_coupling(
    regional,
    ngeo_validate_layers(regional, complete = "error"),
    estimands = "same_location"
  )
  expect_equal(as.numeric(weighted$values), expected, tolerance = 1e-12)
  expect_equal(
    expected,
    stats::cor(rep(first, support), rep(second, support)),
    tolerance = 1e-12
  )
})

test_that("missing layers remain missing and invalid measures fail", {
  x <- coupling_fixture(
    cbind(1:5, 5:1, c(2, 3, 5, 7, 11)),
    c("s1", "s1", "s2"), c("x", "y", "x")
  )
  index <- ngeo_validate_layers(x, complete = "report")
  result <- ngeo_layer_coupling(x, index, estimands = "same_location")
  expect_true(is.finite(result$values["s1", 1L]))
  expect_true(is.na(result$values["s2", 1L]))

  constant <- coupling_fixture(
    cbind(rep(1, 5), 1:5), "s1", c("x", "y")
  )
  expect_error(
    ngeo_layer_coupling(
      constant, ngeo_validate_layers(constant),
      estimands = "same_location"
    ),
    class = "ngeo_error_measure"
  )
  invalid <- x
  invalid$measures$spatial_semantics[[1L]] <- "extensive"
  expect_error(
    ngeo_layer_coupling(invalid, index, estimands = "same_location"),
    class = "ngeo_error_measure"
  )
  inconsistent <- x
  inconsistent$measures$units[[3L]] <- "other"
  loose_index <- ngeo_validate_layers(
    inconsistent, complete = "report", require_consistent_measures = FALSE
  )
  expect_error(
    ngeo_layer_coupling(
      inconsistent, loose_index, estimands = "same_location"
    ),
    class = "ngeo_error_layer_measure"
  )
})

test_that("spectral coupling separates energy from normalized coupling", {
  placeholder <- coupling_fixture(
    matrix(seq_len(48), nrow = 6L),
    rep(c("s1", "s2", "s3", "s4"), each = 2L),
    rep(c("x", "y"), 4L)
  )
  weights <- coupling_weights(placeholder, "B")
  basis <- ngeo_spatial_basis(
    placeholder, weights, support = "identity", n_modes = 5L
  )
  mode <- basis$components[[1L]]$vectors[, 1L]
  mode_2 <- basis$components[[1L]]$vectors[, 2L]
  placeholder$values <- cbind(
    mode, mode,
    2 * mode, 3 * mode,
    mode, -mode,
    mode, mode_2
  )
  index <- ngeo_validate_layers(placeholder, complete = "error")
  result <- ngeo_layer_coupling(
    placeholder, index, basis = basis,
    bands = list(low = 1:2, high = 3:5),
    estimands = "spectral_coupling", energy_floor = 1e-12
  )
  coupling <- result$values[
    , result$endpoints$estimand == "spectral_coupling" &
      result$endpoints$band == "low", drop = FALSE
  ]
  expect_equal(
    unname(coupling[, 1L]), c(1, 1, -1, 0), tolerance = 1e-10
  )
  energy_x <- result$values[
    , result$endpoints$estimand == "band_energy_x" &
      result$endpoints$band == "low", drop = FALSE
  ]
  expect_equal(
    unname(energy_x[, 1L]), c(1, 4, 1, 1), tolerance = 1e-10
  )
  energy_y <- result$values[
    , result$endpoints$estimand == "band_energy_y" &
      result$endpoints$band == "low", drop = FALSE
  ]
  expect_equal(
    unname(energy_y[, 1L]), c(1, 9, 1, 1), tolerance = 1e-10
  )
  high_coupling <- result$values[
    , result$endpoints$estimand == "spectral_coupling" &
      result$endpoints$band == "high", drop = FALSE
  ]
  expect_true(all(is.na(high_coupling)))
  expect_true(all(result$diagnostics$low_energy$low_energy))

  flipped <- basis
  flipped$components[[1L]]$vectors[, 1L] <-
    -flipped$components[[1L]]$vectors[, 1L]
  sign_result <- ngeo_layer_coupling(
    placeholder, index, basis = flipped,
    bands = list(low = 1:2, high = 3:5),
    estimands = "spectral_coupling", energy_floor = 1e-12
  )
  expect_equal(sign_result$values, result$values, tolerance = 1e-10)
})

test_that("complete degenerate bands are rotation invariant", {
  n <- 8L
  theta <- 2 * pi * (0:(n - 1L)) / n
  values <- cbind(cos(theta), sin(theta))
  x <- coupling_fixture(values, "reference", c("x", "y"))
  adjacency <- Matrix::sparseMatrix(
    i = c(seq_len(n), seq_len(n)),
    j = c(2:n, 1L, n, seq_len(n - 1L)),
    x = 1, dims = c(n, n)
  )
  weights <- ngeo_weights(x, method = "distance_band", threshold = 1.01,
                          style = "B")
  weights$raw_matrix <- adjacency
  weights$matrix <- adjacency
  basis <- ngeo_spatial_basis(x, weights, support = "identity", n_modes = 7L)
  cluster <- which(basis$components[[1L]]$degenerate_cluster ==
    basis$components[[1L]]$degenerate_cluster[[1L]])
  expect_equal(length(cluster), 2L)
  bands <- list(first_pair = cluster,
                remainder = setdiff(1:7, cluster))
  observed <- ngeo_layer_coupling(
    x, ngeo_validate_layers(x), basis = basis, bands = bands,
    estimands = "spectral_coupling"
  )
  rotated <- basis
  angle <- 0.37
  rotation <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2L)
  rotated$components[[1L]]$vectors[, cluster] <-
    rotated$components[[1L]]$vectors[, cluster, drop = FALSE] %*% rotation
  repeated <- ngeo_layer_coupling(
    x, ngeo_validate_layers(x), basis = rotated, bands = bands,
    estimands = "spectral_coupling"
  )
  expect_equal(repeated$values, observed$values, tolerance = 1e-10)
})

test_that("directional lag and classic cross-Moran retain direction", {
  skip_if_not_installed("spdep")
  n <- 7L
  x_values <- c(-2, 0, 1, 4, 3, -1, 2)
  y_values <- c(3, -1, 2, 0, 5, 4, -2)
  x <- coupling_fixture(cbind(x_values, y_values), "s1", c("x", "y"))
  weights <- coupling_weights(x, "W")
  result <- ngeo_layer_coupling(
    x, ngeo_validate_layers(x), weights = weights,
    estimands = c("directional_lag", "classic_cross_moran"),
    lag_direction = "both"
  )
  moran_rows <- which(result$endpoints$estimand == "classic_cross_moran")
  expect_setequal(result$endpoints$direction[moran_rows],
                  c("x_to_y", "y_to_x"))
  expect_false(isTRUE(all.equal(
    result$values[1L, moran_rows[[1L]]],
    result$values[1L, moran_rows[[2L]]]
  )))
  listw <- spdep::mat2listw(as.matrix(weights$matrix), style = "W")
  reference_xy <- spdep::moran_bv(
    x_values, y_values, listw, nsim = 2L, scale = TRUE
  )$t0
  observed_xy <- result$values[
    1L, moran_rows[result$endpoints$direction[moran_rows] == "x_to_y"]
  ]
  expect_equal(unname(observed_xy), unname(reference_xy), tolerance = 1e-12)

  binary <- coupling_weights(x, "B")
  binary_result <- ngeo_layer_coupling(
    x, ngeo_validate_layers(x), weights = binary,
    estimands = "classic_cross_moran", lag_direction = "x_to_y"
  )
  zx <- as.numeric(scale(x_values))
  zy <- as.numeric(scale(y_values))
  expected_binary <- n / sum(binary$matrix) *
    sum(zx * as.numeric(binary$matrix %*% zy)) / sum(zx^2)
  expect_equal(
    as.numeric(binary_result$values), expected_binary, tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(
    as.numeric(binary_result$values), as.numeric(observed_xy)
  )))
  lag_rows <- which(result$endpoints$estimand == "directional_lag")
  expect_true(all(result$endpoints$standardization[lag_rows] ==
    "support_weighted_lag_norm"))
  expect_true(all(result$endpoints$standardization[moran_rows] ==
    "sample_z_n_over_S0"))
})

test_that("isolates and ambiguous pair families are rejected", {
  x <- coupling_fixture(cbind(1:5, c(2, 4, 1, 5, 3)), "s1", c("x", "y"))
  weights <- coupling_weights(x, "W")
  weights$raw_matrix[5L, ] <- 0
  weights$raw_matrix[, 5L] <- 0
  weights$matrix <- neurogeo:::.ngeo_row_standardize(weights$raw_matrix)
  expect_error(
    ngeo_layer_coupling(
      x, ngeo_validate_layers(x), weights = weights,
      estimands = "directional_lag"
    ),
    class = "ngeo_error_zero_policy"
  )

  three <- coupling_fixture(
    cbind(1:5, 5:1, c(2, 3, 5, 7, 11)),
    "s1", c("x", "y", "z")
  )
  expect_error(
    ngeo_layer_coupling(
      three, ngeo_validate_layers(three), estimands = "same_location"
    ),
    class = "ngeo_error_layer_pairs"
  )
})

test_that("reference-map nulls retain regime and transformation provenance", {
  x <- coupling_fixture(
    cbind(c(-2, -1, 0, 1, 2), c(2, 0, -1, 1, -2)),
    "reference", c("x", "y")
  )
  mappings <- cbind(c(2, 3, 4, 5, 1), c(5, 4, 3, 2, 1))
  group <- structure(list(
    method = "declared_permutation_group",
    mappings = mappings,
    domain_hash = ngeo_domain_hash(x),
    nsim = ncol(mappings)
  ), class = "ngeo_null")
  null <- list(
    randomized_stack = "y",
    fixed_stack = "x",
    group = group,
    shared_transformation = TRUE,
    preserved_properties = c("within_randomized_stack_covariance")
  )
  result <- ngeo_layer_coupling(
    x, ngeo_validate_layers(x), estimands = "same_location", null = null
  )
  expect_identical(result$null$inference_unit, "spatial_map")
  expect_identical(result$null$population_inference, FALSE)
  expect_identical(result$null$randomized_stack, "y")
  expect_identical(result$null$fixed_stack, "x")
  expect_equal(nrow(result$null$simulated), 2L)
  expect_true(nzchar(result$null$null_hash))
  expect_true(all(result$null$p_value >= 0 & result$null$p_value <= 1))

  invalid <- null
  invalid$randomized_stack <- c("x", "y")
  invalid$fixed_stack <- character()
  expect_error(
    ngeo_layer_coupling(
      x, ngeo_validate_layers(x), estimands = "same_location",
      null = invalid
    ),
    class = "ngeo_error_null_target"
  )
})
