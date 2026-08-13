p2_chain_fixture <- function() {
  coordinates <- cbind(x = 0:7, y = 0)
  signal_x <- c(-2, -1, 0, 2, 3, 1, -1, -2)
  signal_y <- c(-1, -2, 1, 3, 2, 0, -2, -1)
  values <- cbind(s1_x = signal_x, s1_y = signal_y)
  layers <- data.frame(
    layer_id = colnames(values),
    name = colnames(values),
    measure_id = c("mx", "my"),
    subject_id = "s1",
    feature = c("x", "y"),
    stringsAsFactors = FALSE
  )
  measures <- rbind(
    ngeo_measure(measure_id = "mx", support_behavior = "intensive"),
    ngeo_measure(measure_id = "my", support_behavior = "intensive")
  )
  x <- ngeo_point(
    coordinates, values = values, layers = layers, measures = measures
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  list(
    x = x,
    weights = weights,
    index = ngeo_validate_layers(x, complete = "error")
  )
}

test_that("graph wavelets localize cross-layer coupling by scale", {
  fixture <- p2_chain_fixture()
  basis <- ngeo_spatial_basis(
    fixture$x, fixture$weights, support = "identity", n_modes = 7L
  )
  result <- ngeo_wavelet_coupling(
    fixture$x, fixture$index, basis, scales = c(0.5, 2)
  )

  expect_s3_class(result, "ngeo_wavelet_coupling")
  expect_identical(result$history$status, "stable")
  expect_equal(nrow(result$scale_summary), 2L)
  expect_equal(sort(unique(result$values$scale)), c(0.5, 2))
  expect_true(all(result$scale_summary$coherence >= -1 - 1e-12))
  expect_true(all(result$scale_summary$coherence <= 1 + 1e-12))
  expect_identical(result$scales$physical_calibration, c(FALSE, FALSE))
  expect_true(all(result$scale_summary$retained_energy_fraction_x <= 1))
  expect_match(result$history$scale_contract, "not claimed")

  shifted <- fixture$x
  shifted$values <- shifted$values * 2 + 1e16
  shifted_result <- ngeo_wavelet_coupling(
    shifted, fixture$index, basis, scales = c(0.5, 2)
  )
  expect_equal(shifted_result$values$wavelet_x, 2 * result$values$wavelet_x,
               tolerance = 1e-12)
  expect_equal(shifted_result$scale_summary$coherence,
               result$scale_summary$coherence, tolerance = 1e-12)

  extreme <- fixture$x
  extreme$values[, ] <- rep(c(-1e308, 1e308), length.out = length(extreme$values))
  expect_error(
    ngeo_wavelet_coupling(extreme, fixture$index, basis, scales = 1),
    class = "ngeo_error_measure"
  )
})

test_that("wavelet marginal energies remain defined for a constant partner", {
  fixture <- p2_chain_fixture()
  basis <- ngeo_spatial_basis(
    fixture$x, fixture$weights, support = "identity", n_modes = 7L
  )
  fixture$x$values[, "s1_y"] <- 1
  first <- ngeo_wavelet_coupling(
    fixture$x, fixture$index, basis, scales = 1
  )$scale_summary
  expect_gt(first$energy_x, 0)
  expect_equal(first$energy_y, 0)
  expect_equal(first$cross_energy, 0)
  expect_true(is.na(first$coherence))

  fixture$x$values[, "s1_x"] <- 1
  fixture$x$values[, "s1_y"] <- seq_len(nrow(fixture$x$values))
  second <- ngeo_wavelet_coupling(
    fixture$x, fixture$index, basis, scales = 1
  )$scale_summary
  expect_equal(second$energy_x, 0)
  expect_gt(second$energy_y, 0)
  expect_equal(second$cross_energy, 0)
  expect_true(is.na(second$coherence))

  fixture$x$values[, "s1_y"] <- 1
  third <- ngeo_wavelet_coupling(
    fixture$x, fixture$index, basis, scales = 1
  )$scale_summary
  expect_equal(third$energy_x, 0)
  expect_equal(third$energy_y, 0)
  expect_equal(third$cross_energy, 0)
  expect_true(is.na(third$coherence))
})

test_that("cotangent basis uses surface area mass and supports wavelets", {
  surface <- ngeo_surface(
    matrix(c(
      0, 0, 0,
      1, 0, 0,
      1, 1, 0,
      0, 1, 0
    ), ncol = 3L, byrow = TRUE),
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE),
    values = cbind(s1_x = c(-1, 0, 2, 1), s1_y = c(0, -1, 1, 2)),
    layers = data.frame(
      layer_id = c("s1_x", "s1_y"),
      name = c("s1_x", "s1_y"),
      measure_id = c("mx", "my"),
      subject_id = "s1",
      feature = c("x", "y"),
      stringsAsFactors = FALSE
    ),
    measures = rbind(
      ngeo_measure(measure_id = "mx", support_behavior = "intensive"),
      ngeo_measure(measure_id = "my", support_behavior = "intensive")
    )
  )
  basis <- ngeo_spatial_basis(
    surface, operator = "cotangent", n_modes = 3L
  )
  wavelet <- ngeo_wavelet_coupling(
    surface, ngeo_validate_layers(surface), basis, scales = 1
  )

  expect_s3_class(basis, "ngeo_spatial_basis")
  expect_equal(basis$diagnostics$surface_area, 1, tolerance = 1e-12)
  expect_equal(basis$diagnostics$mass_total, 1, tolerance = 1e-12)
  expect_true(all(basis$components[[1L]]$eigenvalues >= -1e-10))
  expect_lt(basis$diagnostics$max_orthogonality_error, 1e-8)
  expect_equal(nrow(wavelet$scale_summary), 1L)
})

test_that("cotangent basis rejects degenerate surface faces", {
  surface <- ngeo_surface(
    matrix(c(0, 0, 0, 1, 0, 0, 2, 0, 0), ncol = 3L, byrow = TRUE),
    matrix(c(1, 2, 3), ncol = 3L),
    values = cbind(signal = 1:3)
  )
  expect_error(
    ngeo_spatial_basis(surface, operator = "cotangent", n_modes = 2L),
    class = "ngeo_error_geometry"
  )
})

test_that("contiguous regionalization returns connected minimum-size regions", {
  fixture <- p2_chain_fixture()
  partition <- ngeo_contiguous_regionalization(
    fixture$x,
    fixture$weights,
    layers = c("s1_x", "s1_y"),
    n_regions = 3L,
    min_elements = 2L
  )

  expect_s3_class(partition, "ngeo_partition")
  expect_equal(nrow(partition$parcellation), 3L)
  expect_true(all(partition$regionalization$region$elements >= 2L))
  for (region in partition$parcellation$region_id) {
    rows <- which(partition$membership == region)
    component <- ngeo_components(
      fixture$weights$raw_matrix[rows, rows, drop = FALSE]
    )
    expect_equal(length(unique(component)), 1L)
  }
  expect_match(partition$regionalization$inference, "training data")
})

test_that("brain landscape reports patches, boundaries, and layer overlap", {
  fixture <- p2_chain_fixture()
  result <- ngeo_brain_landscape(
    fixture$x,
    fixture$weights,
    layers = c("s1_x", "s1_y"),
    threshold = 0.65,
    threshold_type = "quantile",
    boundary_quantile = 0.75
  )

  expect_s3_class(result, "ngeo_brain_landscape")
  expect_identical(result$history$status, "stable")
  expect_equal(nrow(result$layer_summary), 2L)
  expect_equal(nrow(result$cross_layer_overlap), 1L)
  expect_true(all(result$nodes$gradient >= 0))
  expect_true(all(result$cross_layer_overlap$active_jaccard >= 0))
  expect_true(all(result$cross_layer_overlap$active_jaccard <= 1))
  expect_identical(result$history$physical_length_claimed, FALSE)
  expect_match(result$history$boundary_metric, "not_physical_length")
  contract <- ngeo_inference_contract(result)
  expect_identical(contract$identifiers$base_hash, base_hash(fixture$x))
  expect_identical(contract$identifiers$analysis_hash, result$analysis_hash)
})

test_that("regionalization freeze and apply guard against training leakage", {
  fixture <- p2_chain_fixture()
  trained <- ngeo_contiguous_regionalization(
    fixture$x, fixture$weights, layers = c("s1_x", "s1_y"),
    n_regions = 2L
  )
  frozen <- ngeo_freeze_regionalization(trained)

  expect_s3_class(frozen, "ngeo_frozen_regionalization")
  expect_error(
    ngeo_apply_regionalization(fixture$x, frozen),
    class = "ngeo_error_data_leakage"
  )
  application <- fixture$x
  application$values[, c("s1_x", "s1_y")] <-
    application$values[, c("s1_x", "s1_y")] + 0.5
  applied <- ngeo_apply_regionalization(application, frozen)
  expect_s3_class(applied, "ngeo_partition")
  expect_identical(applied$membership, trained$membership)
  expect_identical(applied$regionalization$workflow_state, "applied")
  expect_false(applied$regionalization$training_values_reused)
})

test_that("least-cost distance follows the anatomy-conditioned path", {
  fixture <- p2_chain_fixture()
  pairs <- data.frame(from = c(1L, 1L), to = c(2L, 8L))
  result <- ngeo_resistance_distance(
    fixture$x,
    fixture$weights,
    layer = "s1_x",
    pairs = pairs,
    interpretation = "barrier",
    method = "least_cost",
    beta = 0.5
  )

  expect_s3_class(result, "ngeo_resistance_distance")
  expect_identical(result$history$status, "stable")
  expect_equal(nrow(result$distances), 2L)
  expect_gt(result$distances$distance[[2L]], result$distances$distance[[1L]])
  expect_true(all(result$edge$conductance > 0))
  expect_identical(result$history$causal_propagation_claimed, FALSE)
  contract <- ngeo_inference_contract(result)
  expect_identical(contract$identifiers$layer_id, "s1_x")
  expect_identical(contract$identifiers$result_hash, result$result_hash)
})

test_that("effective resistance agrees with a unit-conductance chain", {
  fixture <- p2_chain_fixture()
  fixture$x$values[, "s1_x"] <- 1
  result <- ngeo_resistance_distance(
    fixture$x,
    fixture$weights,
    layer = "s1_x",
    pairs = data.frame(from = c(1L, 1L), to = c(2L, 8L)),
    interpretation = "conductance",
    method = "effective_resistance"
  )

  expect_equal(result$distances$distance, c(1, 7), tolerance = 1e-10)
})

test_that("diffusion distance uses the declared heat time", {
  fixture <- p2_chain_fixture()
  fixture$x$values[, "s1_x"] <- 1
  pairs <- data.frame(from = c(1L, 1L), to = c(2L, 8L))
  early <- ngeo_resistance_distance(
    fixture$x,
    fixture$weights,
    layer = "s1_x",
    pairs = pairs,
    interpretation = "conductance",
    method = "diffusion_distance",
    diffusion_time = 0.25
  )
  late <- ngeo_resistance_distance(
    fixture$x,
    fixture$weights,
    layer = "s1_x",
    pairs = pairs,
    interpretation = "conductance",
    method = "diffusion_distance",
    diffusion_time = 2
  )

  expect_gt(early$distances$distance[[2L]], early$distances$distance[[1L]])
  expect_true(all(late$distances$distance < early$distances$distance))
  expect_identical(
    early$history$diffusion_kernel,
    "equal_support_combinatorial_laplacian_heat_kernel"
  )
  expect_equal(early$history$diffusion_time, 0.25)
})

test_that("cotangent geometry is translation invariant and metric eligible", {
  coordinates <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0
  ), ncol = 3L, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), ncol = 3L)
  first <- ngeo_surface(coordinates, faces)
  shifted <- ngeo_surface(coordinates + 1e8, faces)
  first_basis <- ngeo_spatial_basis(first, operator = "cotangent", n_modes = 2L)
  shifted_basis <- ngeo_spatial_basis(
    shifted, operator = "cotangent", n_modes = 2L
  )
  expect_equal(
    first_basis$components[[1L]]$eigenvalues,
    shifted_basis$components[[1L]]$eigenvalues,
    tolerance = 1e-7
  )
  chart <- ngeo_surface(
    list(anatomical = coordinates, flat = coordinates[, 1:2]), faces,
    active_coordinates = "flat",
    coordinate_roles = c("anatomical", "chart")
  )
  expect_error(
    ngeo_spatial_basis(chart, operator = "cotangent", coordinates = "flat"),
    class = "ngeo_error_metric"
  )
})

test_that("cotangent basis rejects non-manifold edges", {
  coordinates <- matrix(c(
    0, 0, 0, 1, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 1
  ), ncol = 3L, byrow = TRUE)
  surface <- ngeo_surface(
    coordinates,
    matrix(c(1, 2, 3, 2, 1, 4, 1, 2, 5), ncol = 3L, byrow = TRUE)
  )
  expect_error(
    ngeo_spatial_basis(surface, operator = "cotangent", n_modes = 2L),
    class = "ngeo_error_geometry"
  )
})

test_that("landscape layer identity and flat-boundary behavior are explicit", {
  fixture <- p2_chain_fixture()
  fixture$x$layers$name[] <- "duplicate_name"
  result <- ngeo_brain_landscape(
    fixture$x, fixture$weights, layers = 1:2,
    threshold = 0.5, boundary_quantile = 1
  )
  expect_equal(nrow(result$cross_layer_overlap), 1L)
  expect_equal(length(unique(result$nodes$layer_id)), 2L)

  fixture$x$values[, ] <- rep(c(rep(0, 7), 1), 2)
  result <- ngeo_brain_landscape(
    fixture$x, fixture$weights, layers = 1:2,
    threshold = 0.5, boundary_quantile = 0.9
  )
  expect_equal(sum(result$edges$boundary), 2L)
})

test_that("effective resistance retains very small positive conductance", {
  fixture <- p2_chain_fixture()
  fixture$x$values[, "s1_x"] <- 1e-12
  result <- ngeo_resistance_distance(
    fixture$x, fixture$weights, layer = "s1_x",
    pairs = data.frame(from = 1L, to = 8L),
    interpretation = "conductance", method = "effective_resistance"
  )
  expect_equal(result$distances$distance, 7e12, tolerance = 1e-6)
})

test_that("resistance rejects numerical conductance underflow", {
  fixture <- p2_chain_fixture()
  expect_error(
    ngeo_resistance_distance(
      fixture$x, fixture$weights, layer = "s1_x",
      pairs = data.frame(from = 1L, to = 8L), beta = 1e308
    ),
    class = "ngeo_error_measure"
  )

  overflow <- p2_chain_fixture()
  overflow$x$values[, "s1_x"] <- 1.1e-308
  expect_error(
    ngeo_resistance_distance(
      overflow$x, overflow$weights, "s1_x",
      pairs = data.frame(from = 1L, to = 8L),
      interpretation = "conductance"
    ),
    class = "ngeo_error_measure"
  )
})

test_that("cotangent rejects a bow-tie vertex and detects basis tampering", {
  bow_tie <- ngeo_surface(
    matrix(c(
      0, 0, 0, 1, 0, 0, 0, 1, 0,
      -1, 0, 0, 0, -1, 0
    ), ncol = 3L, byrow = TRUE),
    matrix(c(1, 2, 3, 1, 4, 5), ncol = 3L, byrow = TRUE)
  )
  expect_error(
    ngeo_spatial_basis(bow_tie, operator = "cotangent", n_modes = 2L),
    class = "ngeo_error_geometry"
  )

  fixture <- p2_chain_fixture()
  basis <- ngeo_spatial_basis(
    fixture$x, fixture$weights, support = "identity", n_modes = 7L
  )
  basis$components[[1L]]$vectors[1L, 1L] <-
    basis$components[[1L]]$vectors[1L, 1L] + 0.1
  expect_error(
    ngeo_wavelet_coupling(fixture$x, fixture$index, basis, scales = 1),
    class = "ngeo_error_identity"
  )

  surface <- ngeo_surface(
    matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3L, byrow = TRUE),
    matrix(c(1, 2, 3), ncol = 3L)
  )
  cotangent <- ngeo_spatial_basis(
    surface, operator = "cotangent", n_modes = 2L
  )
  cotangent$tolerance <- cotangent$tolerance * 2
  expect_error(
    neurogeo:::.ngeo_validate_spatial_basis(cotangent, surface),
    class = "ngeo_error_identity"
  )
})

test_that("regionalization backtracks to a feasible constrained cut", {
  values <- c(0, 1, 2, 12, 13, 14)
  x <- ngeo_point(
    cbind(x = 0:5, y = 0),
    values = cbind(signal = values),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  partition <- ngeo_contiguous_regionalization(
    x, weights, "signal", n_regions = 3L, min_elements = 2L
  )
  expect_identical(
    sort(as.integer(table(partition$membership))), c(2L, 2L, 2L)
  )
  expect_false(partition$regionalization$globally_optimal)
})

test_that("directed inverse-distance union and resistance extremes are typed", {
  x <- ngeo_point(
    cbind(x = c(0, 1, 10), y = 0),
    values = cbind(signal = rep(1, 3L)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  directed <- ngeo_spatial_weights(
    x, method = "inverse_distance", k = 1L,
    symmetry = "directed", style = "none"
  )
  result <- ngeo_resistance_distance(
    x, directed, "signal", pairs = data.frame(from = 2L, to = 3L),
    interpretation = "conductance", edge_cost = "spatial_weights"
  )
  expect_equal(result$distances$distance, 9)

  triangle <- ngeo_point(
    matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2L, byrow = TRUE),
    values = cbind(signal = rep(9e307, 3L)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  complete <- ngeo_spatial_weights(
    triangle, method = "distance_band", threshold = 2, style = "B"
  )
  for (method in c("effective_resistance", "diffusion_distance")) {
    current <- ngeo_resistance_distance(
      triangle, complete, "signal",
      pairs = data.frame(from = 1L, to = 2L),
      interpretation = "conductance", method = method
    )
    expect_true(all(is.finite(current$distances$distance)))
  }

  tiny <- p2_chain_fixture()
  tiny$x$values[, "s1_x"] <- 1e-320
  expect_error(
    ngeo_resistance_distance(
      tiny$x, tiny$weights, "s1_x",
      pairs = data.frame(from = 1L, to = 8L),
      interpretation = "conductance"
    ),
    class = "ngeo_error_measure"
  )
})

test_that("landscape refuses unrepresentable edge contrasts", {
  x <- ngeo_point(
    matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2L, byrow = TRUE),
    values = cbind(signal = c(9e307, -9e307, 0)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 2, style = "B"
  )
  expect_error(
    ngeo_brain_landscape(x, weights, "signal", threshold_type = "value",
                         threshold = 0),
    class = "ngeo_error_measure"
  )
})

test_that("landscape gradients retain representable contrasts at large offsets", {
  x <- ngeo_point(
    cbind(x = 0:2, y = 0),
    values = cbind(signal = c(1e16, 1e16 + 2, 1e16 + 4)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  result <- ngeo_brain_landscape(
    x, weights, "signal", threshold_type = "value", threshold = 1e16
  )
  expect_equal(result$edges$absolute_difference, c(2, 2))
  expect_equal(result$nodes$gradient, rep(2, 3L))
})

test_that("two-node diffusion matches the analytic heat kernel", {
  x <- ngeo_point(
    cbind(x = 0:1, y = 0), values = cbind(signal = c(2, 2)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  time <- 0.4
  result <- ngeo_resistance_distance(
    x, weights, "signal", pairs = data.frame(from = 1L, to = 2L),
    interpretation = "conductance", method = "diffusion_distance",
    diffusion_time = time
  )
  expect_equal(
    result$distances$distance,
    sqrt(2) * exp(-2 * 2 * time),
    tolerance = 1e-12
  )
})

test_that("landscape permits zero contrast within disconnected components", {
  x <- ngeo_point(
    cbind(x = c(0, 1, 10, 11), y = 0),
    values = cbind(signal = c(0, 0, 1, 1)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  result <- ngeo_brain_landscape(x, weights, "signal")
  expect_equal(result$nodes$gradient, rep(0, 4L))
  expect_false(any(result$edges$boundary))
})
