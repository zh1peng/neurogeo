local_coupling_fixture <- function() {
  coordinates <- cbind(x = 0:4, y = 0)
  values <- cbind(
    s1_x = c(-2, -1, 0, 1, 2),
    s1_y = c(1, -2, 0, 2, -1),
    s2_x = c(2, 1, 0, -1, -2),
    s2_y = c(-1, 2, 0, -2, 1)
  )
  layers <- data.frame(
    layer_id = colnames(values),
    name = colnames(values),
    measure_id = paste0("measure_", seq_len(ncol(values))),
    subject_id = rep(c("s1", "s2"), each = 2L),
    feature = rep(c("x", "y"), 2L),
    stringsAsFactors = FALSE
  )
  measures <- do.call(rbind, lapply(layers$measure_id, function(id) {
    ngeo_measure(
      measure_id = id,
      support_behavior = "intensive",
      unit = "ratio"
    )
  }))
  x <- ngeo_point(
    coordinates, values = values, layers = layers, measures = measures
  )
  weights <- ngeo_spatial_weights(
    x, method = "distance_band", threshold = 1.01, style = "W"
  )
  list(
    x = x,
    index = ngeo_validate_layers(x, complete = "error"),
    weights = weights
  )
}

test_that("local cross-layer coupling returns exact directed local statistics", {
  fixture <- local_coupling_fixture()
  result <- ngeo_local_layer_coupling(
    fixture$x,
    fixture$index,
    fixture$weights,
    metrics = "local_cross_moran",
    direction = "x_to_y"
  )

  expect_s3_class(result, "ngeo_local_layer_coupling")
  expect_identical(result$history$status, "stable")
  expect_equal(nrow(result$values), 10L)
  first <- result$values[result$values$unit_id == "s1", ]
  support <- rep(1, 5L)
  z_x <- neurogeo:::.ngeo_local_z(fixture$x$values[, "s1_x"], support)
  z_y <- neurogeo:::.ngeo_local_z(fixture$x$values[, "s1_y"], support)
  expected <- z_x * as.numeric(fixture$weights$matrix %*% z_y)
  expect_equal(first$statistic, expected, tolerance = 1e-12)
  expect_identical(result$history$population_inference, FALSE)
  expect_identical(result$history$preserves_spatial_autocorrelation, FALSE)
  contract <- ngeo_inference_contract(result)
  expect_identical(contract$identifiers$base_hash, base_hash(fixture$x))
  expect_identical(contract$identifiers$weights_hash, result$weights_hash)
  expect_identical(contract$identifiers$analysis_hash, result$analysis_hash)
  expect_identical(contract$identifiers$result_hash, result$result_hash)
  tampered <- result
  tampered$values$statistic[[1L]] <- 999
  expect_error(
    ngeo_inference_contract(tampered), class = "ngeo_error_identity"
  )
})

test_that("local coupling maxT inference is reproducible and bounded", {
  fixture <- local_coupling_fixture()
  first <- ngeo_local_layer_coupling(
    fixture$x,
    fixture$index,
    fixture$weights,
    metrics = c("local_cross_moran", "local_geary"),
    direction = "x_to_y",
    permutations = 19L,
    seed = 61L,
    null = "free"
  )
  second <- ngeo_local_layer_coupling(
    fixture$x,
    fixture$index,
    fixture$weights,
    metrics = c("local_cross_moran", "local_geary"),
    direction = "x_to_y",
    permutations = 19L,
    seed = 61L,
    null = "free"
  )

  expect_equal(first$values$p_value, second$values$p_value)
  expect_true(all(first$values$p_value > 0 & first$values$p_value <= 1))
  expect_true(all(first$values$p_adjusted >= first$values$p_value))
  expect_match(first$history$null, "unconstrained")
  expect_error(
    ngeo_local_layer_coupling(
      fixture$x, fixture$index, fixture$weights,
      metrics = "local_cross_moran", direction = "both",
      permutations = 9L
    ),
    class = "ngeo_error_argument"
  )
})

test_that("MAUP robustness separates scale and zoning contributions", {
  fixture <- inference_fixture()
  fine_a <- fixture$layers$A
  fine_b <- fixture$layers$B
  coarse_a <- ngeo_atlas_map(
    fixture$source, c("L", "L", "L", "R", "R", "R")
  )
  coarse_b <- ngeo_atlas_map(
    fixture$source, c("L", "L", "R", "R", "R", "L"),
    target = coarse_a$target
  )
  maps <- list(
    fine_a = fine_a,
    fine_b = fine_b,
    coarse_a = coarse_a,
    coarse_b = coarse_b
  )
  targets <- list(
    fixture$targets[[1L]], fixture$targets[[2L]],
    coarse_a$target, coarse_a$target
  )
  result <- ngeo_maup_sensitivity(
    fixture$source,
    maps,
    targets,
    outcome = "outcome",
    predictor = "predictor",
    scale = c("fine", "fine", "coarse", "coarse"),
    zoning = c("a", "b", "a", "b"),
    statistic = "correlation"
  )

  expect_s3_class(result, "ngeo_maup_sensitivity")
  expect_identical(result$status, "stable")
  expect_equal(nrow(result$estimates), 4L)
  expect_equal(nrow(result$decomposition$scale), 2L)
  expect_equal(
    sum(result$decomposition$fraction), 1, tolerance = 1e-12
  )
  contribution_sum <- tapply(
    result$regional_contribution$contribution,
    result$regional_contribution$family_id,
    sum
  )
  expect_equal(
    as.numeric(contribution_sum[result$estimates$family_id]),
    result$estimates$estimate,
    tolerance = 1e-12
  )
  expect_match(result$interpretation, "not atlas invariance")
})

operator_graph_fixture <- function() {
  source <- ngeo_point(
    cbind(x = 0:5, y = 0),
    values = cbind(signal = seq_len(6L)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  middle <- ngeo_parcellation(
    data.frame(region_id = c("A", "B", "C")),
    support_size = c(2, 2, 2),
    coordinate_space = source$base$coordinate_space
  )
  target <- ngeo_parcellation(
    data.frame(region_id = c("L", "R")),
    support_size = c(4, 2),
    coordinate_space = source$base$coordinate_space
  )
  first <- ngeo_support_map(
    source, middle, c("A", "A", "B", "B", "C", "C"),
    source_support = rep(1, 6L)
  )
  second <- ngeo_support_map(
    middle, target, c("L", "L", "R"),
    source_support = c(2, 2, 2)
  )
  list(source = source, middle = middle, target = target,
       first = first, second = second)
}

test_that("operator graph composes an auditable unique path", {
  fixture <- operator_graph_fixture()
  graph <- ngeo_operator_graph(
    list(source = fixture$source, middle = fixture$middle,
         target = fixture$target),
    list(source_to_middle = fixture$first,
         middle_to_target = fixture$second)
  )
  path <- ngeo_operator_path(graph, "source", "target")
  reference <- ngeo_compose_support_map(fixture$first, fixture$second)

  expect_s3_class(graph, "ngeo_operator_graph")
  expect_s3_class(path, "ngeo_operator_path")
  expect_identical(path$status, "stable")
  expect_equal(path$support_map$operator, reference$operator)
  expect_identical(
    path$edge_id, c("source_to_middle", "middle_to_target")
  )
  expect_equal(nrow(path$diagnostics), 3L)
  expect_true(all(c(
    "source_coverage_fraction", "mean_normalized_entropy",
    "stable_rank", "condition_number"
  ) %in% names(path$diagnostics)))
})

test_that("operator graph rejects ambiguous shortest paths", {
  fixture <- operator_graph_fixture()
  direct <- ngeo_compose_support_map(fixture$first, fixture$second)
  graph <- ngeo_operator_graph(
    list(source = fixture$source, middle = fixture$middle,
         target = fixture$target),
    list(direct_a = direct, direct_b = direct,
         source_to_middle = fixture$first,
         middle_to_target = fixture$second)
  )
  expect_error(
    ngeo_operator_path(graph, "source", "target"),
    class = "ngeo_error_path_ambiguous"
  )
  explicit <- ngeo_operator_path(
    graph, "source", "target", edge_id = "direct_a"
  )
  expect_identical(explicit$edge_id, "direct_a")
})

test_that("local permutation p-values use cumulative exceedance counts", {
  fixture <- local_coupling_fixture()
  permutations <- 7L
  seed <- 614L
  result <- ngeo_local_layer_coupling(
    fixture$x, fixture$index, fixture$weights,
    metrics = "local_cross_moran", direction = "x_to_y",
    permutations = permutations, seed = seed, null = "free", adjust = "none"
  )
  observed <- abs(result$values$statistic)
  index <- fixture$index
  lookup <- neurogeo:::.ngeo_coupling_index(fixture$x, index)
  pairs <- neurogeo:::.ngeo_coupling_pairs(index, NULL, TRUE)
  specs <- neurogeo:::.ngeo_local_coupling_specs(
    pairs, "x_to_y", "local_cross_moran"
  )
  support <- neurogeo:::.ngeo_coupling_support(fixture$x, NULL)$values
  weights <- neurogeo:::.ngeo_coupling_weights(
    fixture$x, fixture$weights
  )$matrix
  simulated <- neurogeo:::.ngeo_with_seed(seed, function() {
    replicate(permutations, abs(
      neurogeo:::.ngeo_local_coupling_simulation(
        fixture$x, index, lookup, specs, support, weights, 0,
        sample(seq_len(nrow(fixture$x$base$elements)))
      )
    ))
  })
  expected <- (rowSums(simulated >= observed) + 1) / (permutations + 1)
  expect_equal(result$values$p_value, expected)
  expect_gt(length(unique(result$values$p_value)), 2L)
})

test_that("local coupling supports Moran null and subject-level maxT", {
  fixture <- local_coupling_fixture()
  result <- ngeo_local_layer_coupling(
    fixture$x, fixture$index, fixture$weights,
    metrics = c("local_cross_moran", "local_geary"),
    direction = "x_to_y", permutations = 19L, seed = 6201L
  )

  expect_true(result$history$preserves_spatial_autocorrelation)
  expect_identical(result$history$maxT_scope, "subject")
  expect_match(result$history$null, "Moran spectral")
  expect_true(all(result$values$p_adjusted >= result$values$p_value))
  expect_match(result$diagnostics$family, "independent unit")
})

test_that("MAUP correlation is support weighted and requires crossed zoning", {
  fixture <- inference_fixture()
  maps <- list(a = fixture$layers$A, b = fixture$layers$B)
  targets <- fixture$targets
  result <- ngeo_maup_sensitivity(
    fixture$source, maps, targets,
    outcome = "outcome", predictor = "predictor",
    scale = c("fine", "fine"), zoning = c("a", "b")
  )
  contribution <- split(
    result$regional_contribution, result$regional_contribution$family_id
  )
  expect_equal(
    vapply(contribution, function(z) sum(z$contribution), numeric(1)),
    stats::setNames(result$estimates$estimate, result$estimates$family_id)
  )
  expect_error(
    ngeo_maup_sensitivity(
      fixture$source, maps, targets,
      outcome = "outcome", predictor = "predictor",
      scale = c("fine", "coarse"), zoning = c("a", "b")
    ),
    class = "ngeo_error_argument"
  )
})

test_that("MAUP slope handles a constant outcome and obeys budgets", {
  fixture <- inference_fixture()
  coordinate_name <- fixture$source$base$geometry$active_coordinates
  source <- ngeo_surface(
    fixture$source$base$geometry$coordinates[[coordinate_name]],
    fixture$source$base$geometry$faces,
    values = cbind(
      outcome = rep(0, 6L),
      predictor = fixture$source$values[, "predictor"]
    ),
    measures = rbind(
      ngeo_measure(support_behavior = "intensive"),
      ngeo_measure(support_behavior = "intensive")
    ),
    coordinate_space = fixture$source$base$coordinate_space
  )
  first <- ngeo_atlas_map(source, c("A", "A", "B", "B", "C", "C"))
  second <- ngeo_atlas_map(
    source, c("A", "B", "B", "C", "C", "A"), target = first$target
  )
  result <- ngeo_maup_sensitivity(
    source, list(a = first, b = second), list(first$target, first$target),
    "outcome", "predictor", c("fine", "fine"), c("a", "b"),
    statistic = "slope"
  )
  expect_equal(result$estimates$estimate, c(0, 0))
  expect_error(
    ngeo_maup_sensitivity(
      source, list(a = first, b = second), list(first$target, first$target),
      "outcome", "predictor", c("fine", "fine"), c("a", "b"),
      budget = ngeo_resource_budget(materialized_elements = 1)
    ),
    class = "ngeo_error_resource"
  )

  linear <- ngeo_surface(
    source$base$geometry$coordinates[[source$base$geometry$active_coordinates]],
    source$base$geometry$faces,
    values = cbind(
      outcome = 6 * (0:5) + 1e16,
      predictor = 2 * (0:5) + 1e16
    ),
    measures = rbind(
      ngeo_measure(support_behavior = "intensive"),
      ngeo_measure(support_behavior = "intensive")
    ),
    coordinate_space = source$base$coordinate_space
  )
  linear_map <- ngeo_atlas_map(linear, as.character(seq_len(6L)))
  linear_result <- ngeo_maup_sensitivity(
    linear, list(a = linear_map, b = linear_map),
    list(linear_map$target, linear_map$target),
    "outcome", "predictor", c("fine", "fine"), c("a", "b"),
    statistic = "slope"
  )
  expect_equal(linear_result$estimates$estimate, c(3, 3))
})

test_that("operator graph identity and route choice resist tampering", {
  fixture <- operator_graph_fixture()
  graph <- ngeo_operator_graph(
    list(source = fixture$source, middle = fixture$middle,
         target = fixture$target),
    list(source_to_middle = fixture$first,
         middle_to_target = fixture$second)
  )
  tampered_edge <- graph
  tampered_edge$edge_table$from[[1L]] <- "middle"
  expect_error(
    ngeo_operator_path(tampered_edge, "source", "target"),
    class = "ngeo_error_identity"
  )
  tampered_node <- graph
  tampered_node$nodes$elements[[1L]] <- 999L
  expect_error(
    ngeo_operator_path(tampered_node, "source", "target"),
    class = "ngeo_error_identity"
  )

  direct <- ngeo_compose_support_map(fixture$first, fixture$second)
  graph_with_alternative <- ngeo_operator_graph(
    list(source = fixture$source, middle = fixture$middle,
         target = fixture$target),
    list(direct = direct, source_to_middle = fixture$first,
         middle_to_target = fixture$second)
  )
  expect_error(
    ngeo_operator_path(graph_with_alternative, "source", "target"),
    class = "ngeo_error_path_ambiguous"
  )
  explicit <- ngeo_operator_path(
    graph_with_alternative, "source", "target", edge_id = "direct"
  )
  expect_identical(explicit$source_base_hash, base_hash(fixture$source))
  expect_identical(explicit$target_base_hash, base_hash(fixture$target))
})
