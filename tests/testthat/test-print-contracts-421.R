expect_print_contract <- function(x, header) {
  expect_output(print(x), header, fixed = TRUE)
  visibility <- NULL
  capture.output(visibility <- withVisible(print(x)))
  expect_false(visibility$visible)
}

test_that("analysis result print methods expose their object type", {
  sparse <- Matrix::Matrix(diag(2), sparse = TRUE)
  objects <- list(
    list(
      structure(
        list(
          tolerance = 1e-6,
          max_iterations = 20L,
          trace_order = 4L,
          trace_probes = 8L,
          workers = 1L
        ),
        class = "ngeo_solver_control"
      ),
      "<ngeo_solver_control>"
    ),
    list(
      structure(
        list(
          method = "cg",
          converged = TRUE,
          iterations = 3L,
          relative_residual = 1e-8
        ),
        class = "ngeo_iterative_solution"
      ),
      "<ngeo_iterative_solution>"
    ),
    list(
      structure(
        list(
          method = "series",
          estimate = 1.2,
          standard_error = 0.1,
          truncation_bound = 0.01
        ),
        class = "ngeo_logdet_estimate"
      ),
      "<ngeo_logdet_estimate>"
    ),
    list(
      structure(
        list(
          model = "sar",
          response = "y",
          parameter_name = "rho",
          spatial_parameter = 0.2,
          log_determinant = list(method = "series"),
          optimization = list(converged = TRUE)
        ),
        class = "ngeo_iterative_spatial_regression"
      ),
      "<ngeo_iterative_spatial_regression>"
    ),
    list(
      structure(
        list(
          type = "proper",
          response = "y",
          precision = 2,
          solve = list(converged = TRUE)
        ),
        class = "ngeo_iterative_car"
      ),
      "<ngeo_iterative_car>"
    ),
    list(
      structure(
        list(
          method = "spin",
          simulations = matrix(1:6, nrow = 3)
        ),
        class = "ngeo_null"
      ),
      "<ngeo_null>"
    ),
    list(
      structure(
        data.frame(prediction = c(1, 2)),
        response = "signal",
        metric = "euclidean",
        bandwidth = 2,
        class = c("ngeo_kernel_regression", "data.frame")
      ),
      "<ngeo_kernel_regression>"
    ),
    list(
      structure(
        list(type = "proper", precision = 2, effective_df = 1.5),
        class = "ngeo_car"
      ),
      "<ngeo_car>"
    ),
    list(
      structure(
        list(model = "ols", fits = list(list())),
        class = "ngeo_support_model"
      ),
      "<ngeo_support_model>"
    ),
    list(
      structure(
        list(bandwidth = 2, criterion = "cv", cv = 0.1, score = 0.1),
        class = "ngeo_gwr_bandwidth"
      ),
      "<ngeo_gwr_bandwidth>"
    )
  )

  for (item in objects) {
    expect_print_contract(item[[1L]], item[[2L]])
  }
})

test_that("support result print methods expose bounded summaries", {
  sparse <- Matrix::Matrix(diag(2), sparse = TRUE)
  estimates <- data.frame(atlas = c("a", "b"), statistic = c(0.1, 0.2))
  support_objects <- list(
    list(
      structure(
        list(
          raw = c(0.1, 0.2),
          adjusted = c(0.2, 0.2),
          method = "BH",
          alternative = "two.sided"
        ),
        class = "ngeo_support_adjustment"
      ),
      "<ngeo_support_adjustment>"
    ),
    list(
      structure(
        list(
          method = "random",
          atlas = data.frame(atlas = c("a", "b")),
          estimate = 0.3,
          heterogeneity = list(i_squared = 0.1)
        ),
        class = "ngeo_cross_atlas_consensus"
      ),
      "<ngeo_cross_atlas_consensus>"
    ),
    list(
      structure(
        list(
          null = "permutation",
          statistic = "slope",
          estimates = estimates,
          adjustment = "maxT",
          preserves_spatial_autocorrelation = FALSE
        ),
        class = "ngeo_common_support_test"
      ),
      "<ngeo_common_support_test>"
    ),
    list(
      structure(
        list(
          estimates = data.frame(scale = c("fine", "coarse")),
          stability = c(range = 0.2),
          hierarchy = "declared"
        ),
        class = "ngeo_multiscale_inference"
      ),
      "<ngeo_multiscale_inference>"
    ),
    list(
      structure(
        list(
          effects = data.frame(estimate = c(0.1, 0.2)),
          observed_dispersion = 0.1,
          p_value = 0.5
        ),
        class = "ngeo_boundary_test"
      ),
      "<ngeo_boundary_test>"
    ),
    list(
      structure(
        list(
          predictor = "x",
          outcome = "y",
          estimates = data.frame(atlas = c("a", "b")),
          consensus = c(median = 0.2, range = 0.1)
        ),
        class = "ngeo_atlas_robust_effect"
      ),
      "<ngeo_atlas_robust_effect>"
    ),
    list(
      structure(
        list(
          statistic = "correlation",
          estimates = estimates,
          nsim = 19L,
          permutation_domain = "common_source"
        ),
        class = "ngeo_support_test"
      ),
      "<ngeo_support_test>"
    ),
    list(
      structure(
        list(
          representation = "diagonal",
          dimension = 2L,
          factor = NULL
        ),
        class = "ngeo_support_covariance"
      ),
      "<ngeo_support_covariance>"
    ),
    list(
      structure(
        list(
          method = "delta",
          variance = matrix(c(0.1, 0.2), ncol = 1),
          nsim = NULL
        ),
        class = "ngeo_support_uncertainty"
      ),
      "<ngeo_support_uncertainty>"
    ),
    list(
      structure(
        list(
          stable_rank = 1.5,
          numerical_rank = 2L,
          source = data.frame(isolate = c(FALSE, FALSE)),
          target = data.frame(isolate = c(FALSE, FALSE))
        ),
        class = "ngeo_support_condition"
      ),
      "<ngeo_support_condition>"
    ),
    list(
      structure(
        list(
          kind = "segmentation",
          maps = list(sparse, sparse),
          source_domain_hash = "source",
          target_domain_hash = "target"
        ),
        class = "ngeo_support_ensemble"
      ),
      "<ngeo_support_ensemble>"
    ),
    list(
      structure(
        list(
          model = "row",
          semantics = "intensive",
          values = matrix(1:4, 2)
        ),
        class = "ngeo_cross_atlas"
      ),
      "<ngeo_cross_atlas>"
    ),
    list(
      structure(
        list(
          statistic = "mean",
          estimates = data.frame(reference = 1:3),
          max_deviation = 0.2
        ),
        class = "ngeo_parcellation_inference"
      ),
      "<ngeo_parcellation_inference>"
    )
  )

  for (item in support_objects) {
    expect_print_contract(item[[1L]], item[[2L]])
  }
})

test_that("uncertainty and governance print methods are stable", {
  objects <- list(
    list(
      structure(
        list(successful_simulations = 10L),
        class = "ngeo_variogram_uncertainty"
      ),
      "<ngeo_variogram_uncertainty>"
    ),
    list(
      structure(
        list(coefficients = data.frame(beta = c(1, 2))),
        class = "ngeo_gwr_uncertainty"
      ),
      "<ngeo_gwr_uncertainty>"
    ),
    list(
      structure(
        list(fit = list(model = "sar"), successful_simulations = 10L),
        class = "ngeo_spatial_regression_uncertainty"
      ),
      "<ngeo_spatial_regression_uncertainty>"
    ),
    list(
      structure(
        list(type = "proper", map = data.frame(mean = c(1, 2))),
        class = "ngeo_car_uncertainty"
      ),
      "<ngeo_car_uncertainty>"
    ),
    list(
      structure(
        list(effects = list(a = 1, b = 2)),
        class = "ngeo_support_model_ensemble"
      ),
      "<ngeo_support_model_ensemble>"
    ),
    list(
      structure(
        list(nodes = list(1, 2), edges = list(1), dag_sha256 = "hash"),
        class = "ngeo_provenance_dag"
      ),
      "<ngeo_provenance_dag>"
    ),
    list(
      structure(
        list(steps = list(1), output_hashes = list("x"), canonical_sha256 = "hash"),
        class = "ngeo_replay_manifest"
      ),
      "<ngeo_replay_manifest>"
    ),
    list(
      structure(
        list(outputs = list(1), verified = TRUE),
        class = "ngeo_replay_result"
      ),
      "<ngeo_replay_result>"
    ),
    list(
      structure(
        list(entries = list(1), complete = TRUE, canonical_sha256 = "hash"),
        class = "ngeo_artifact_manifest"
      ),
      "<ngeo_artifact_manifest>"
    ),
    list(
      structure(
        list(
          scope = "batch",
          artifacts = list(entries = list(1)),
          complete = TRUE
        ),
        class = "ngeo_batch_manifest"
      ),
      "<ngeo_batch_manifest>"
    ),
    list(
      structure(
        list(spaces = list(a = 1), aliases = list()),
        class = "ngeo_space_registry"
      ),
      "<ngeo_space_registry>"
    ),
    list(
      structure(
        list(
          registry = list(spaces = list(a = 1, b = 2)),
          edges = data.frame(from = "a", to = "b")
        ),
        class = "ngeo_transform_graph"
      ),
      "<ngeo_transform_graph>"
    ),
    list(
      structure(
        list(tokens = "edge", applicable = TRUE),
        class = "ngeo_transform_path"
      ),
      "<ngeo_transform_path>"
    )
  )

  for (item in objects) {
    expect_print_contract(item[[1L]], item[[2L]])
  }
})

test_that("temporal and resampling print methods expose execution summaries", {
  sparse <- Matrix::Matrix(diag(2), sparse = TRUE)
  objects <- list(
    list(
      structure(
        list(time = c(0, 1), unit = "second", support = "point", regular = TRUE),
        class = "ngeo_time_axis"
      ),
      "<ngeo_time_axis>"
    ),
    list(
      structure(
        list(
          n_time = 2L,
          method = "consecutive",
          matrix = sparse,
          normalization = "W"
        ),
        class = "ngeo_temporal_weights"
      ),
      "<ngeo_temporal_weights>"
    ),
    list(
      structure(
        list(
          n_space = 2L,
          n_time = 2L,
          combination = "sum",
          matrix_materialized = FALSE
        ),
        class = "ngeo_spatiotemporal_weights"
      ),
      "<ngeo_spatiotemporal_weights>"
    ),
    list(
      structure(
        list(
          estimate = 0.2,
          n_observation = 4L,
          permutations = 19L,
          matrix_materialized = FALSE
        ),
        class = "ngeo_spatiotemporal_moran"
      ),
      "<ngeo_spatiotemporal_moran>"
    ),
    list(
      structure(
        list(method = "nearest", path = list(path_hash = "path"), plan_hash = "plan"),
        class = "ngeo_resampling_plan"
      ),
      "<ngeo_resampling_plan>"
    ),
    list(
      structure(
        list(
          method = "nearest",
          mapped_source = 2L,
          source_elements = 2L,
          nonzero = 2L,
          conservative = FALSE
        ),
        class = "ngeo_resampling_diagnostics"
      ),
      "<ngeo_resampling_diagnostics>"
    ),
    list(
      structure(
        list(
          data = list(
            domain = list(type = "points"),
            maps = data.frame(map_id = "value")
          ),
          variance = NULL,
          provenance = list(joint_hash = "joint")
        ),
        class = "ngeo_resampling_result"
      ),
      "<ngeo_resampling_result>"
    )
  )

  for (item in objects) {
    expect_print_contract(item[[1L]], item[[2L]])
  }
})

test_that("data-frame statistic print methods preserve attributes", {
  getis <- structure(
    data.frame(z_score = c(0.1, 0.2)),
    statistic = "Gi*",
    map_name = "signal",
    permutations = 19L,
    class = c("ngeo_getis", "data.frame")
  )
  correlogram <- structure(
    data.frame(lag = 1:2, estimate = c(0.1, 0.2)),
    map_name = "signal",
    permutations = 19L,
    class = c("ngeo_correlogram", "data.frame")
  )
  expect_print_contract(getis, "<ngeo_getis>")
  expect_print_contract(correlogram, "<ngeo_correlogram>")
})
