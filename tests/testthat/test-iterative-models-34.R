iterative_model_fixture <- function(n_side = 5L, seed = 34L) {
  coordinates <- as.matrix(expand.grid(
    x = seq_len(n_side) - 1,
    y = seq_len(n_side) - 1
  ))
  base <- ngeo_point(
    coordinates,
    values = cbind(
      response = seq_len(nrow(coordinates)),
      x = coordinates[, 1L],
      y = coordinates[, 2L]
    )
  )
  spatial_weights <- ngeo_spatial_weights(
    base,
    method = "distance_band",
    threshold = 1.01,
    style = "W"
  )
  binary <- ngeo_spatial_weights(
    base,
    method = "distance_band",
    threshold = 1.01,
    style = "B"
  )
  design <- cbind(1, coordinates[, 1L], coordinates[, 2L])
  error <- neurogeo:::.ngeo_with_seed(seed, function() {
    stats::rnorm(nrow(coordinates), sd = 0.2)
  })
  sar_response <- as.numeric(solve(
    Matrix::Diagonal(nrow(coordinates)) - 0.25 * spatial_weights$matrix,
    design %*% c(2, 1, -0.5) + error
  ))
  sem_error <- as.numeric(solve(
    Matrix::Diagonal(nrow(coordinates)) - 0.3 * spatial_weights$matrix,
    error
  ))
  data <- ngeo_point(
    coordinates,
    values = cbind(
      sar = sar_response,
      sem = as.numeric(design %*% c(2, 1, -0.5)) + sem_error,
      x = coordinates[, 1L],
      y = coordinates[, 2L]
    )
  )
  list(data = data, spatial_weights = spatial_weights, binary = binary)
}

test_that("solver controls are immutable and resource-bound", {
  control <- ngeo_solver_control(
    tolerance = 1e-9,
    max_iterations = 200,
    trace_order = 15,
    trace_probes = 20,
    seed = 34,
    workers = 2
  )

  expect_s3_class(control, "ngeo_solver_control")
  expect_invisible(ngeo_validate_solver_control(control))
  expect_identical(
    ngeo_object_manifest(control)$object_schema_version, "6.0"
  )

  changed <- control
  changed$tolerance <- 1e-4
  expect_error(
    ngeo_validate_solver_control(changed),
    class = "ngeo_error_solver_control"
  )
  expect_error(
    ngeo_solver_control(tolerance = 2),
    class = "ngeo_error_solver_control"
  )
})

test_that("CG and BiCGSTAB agree with direct small solutions", {
  symmetric <- Matrix::Matrix(
    matrix(c(4, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3L),
    sparse = TRUE
  )
  general <- Matrix::Matrix(
    matrix(c(4, 1, 0, 0, 3, 1, 1, 0, 2), nrow = 3L),
    sparse = TRUE
  )
  rhs <- c(1, 2, 3)
  control <- ngeo_solver_control(tolerance = 1e-8)

  cg <- ngeo_iterative_solve(
    symmetric, rhs, method = "cg", control = control
  )
  bicg <- ngeo_iterative_solve(
    general, rhs, method = "bicgstab", control = control
  )
  function_solution <- ngeo_iterative_solve(
    function(value) as.numeric(general %*% value),
    rhs,
    method = "bicgstab",
    control = control,
    n = 3
  )

  expect_true(cg$converged)
  expect_true(bicg$converged)
  expect_equal(
    cg$solution, as.numeric(solve(symmetric, rhs)),
    tolerance = 1e-9
  )
  expect_equal(
    bicg$solution, as.numeric(solve(general, rhs)),
    tolerance = 1e-9
  )
  expect_equal(function_solution$solution, bicg$solution)
  expect_true(all(diff(cg$residual_history) <= 1e-12))
  expect_false(cg$matrix_materialized)
})

test_that("non-convergence and resource boundaries are visible", {
  matrix <- Matrix::Diagonal(20, x = seq(1, 10, length.out = 20))
  rhs <- rep(1, 20)
  returning <- ngeo_solver_control(
    tolerance = 1e-14,
    max_iterations = 1,
    on_nonconvergence = "return"
  )
  failing <- ngeo_solver_control(
    tolerance = 1e-14,
    max_iterations = 1,
    on_nonconvergence = "error"
  )

  result <- ngeo_iterative_solve(
    matrix, rhs, method = "cg", control = returning
  )
  expect_false(result$converged)
  expect_identical(result$reason, "iteration_limit")
  expect_error(
    ngeo_iterative_solve(
      matrix, rhs, method = "cg", control = failing
    ),
    class = "ngeo_error_nonconvergence"
  )
  expect_error(
    ngeo_iterative_solve(
      matrix,
      rhs,
      control = ngeo_solver_control(
        budget = ngeo_resource_budget(materialized_elements = 10)
      )
    ),
    class = "ngeo_error_resource"
  )
})

test_that("log-determinants expose exact and deterministic approximate error", {
  fixture <- iterative_model_fixture()
  parameter <- 0.2
  exact <- ngeo_logdet_approx(
    fixture$spatial_weights,
    parameter,
    control = ngeo_solver_control(exact_threshold = 100),
    exact = TRUE
  )
  serial <- ngeo_logdet_approx(
    fixture$spatial_weights,
    parameter,
    control = ngeo_solver_control(
      trace_order = 30,
      trace_probes = 120,
      seed = 3402,
      workers = 1,
      exact_threshold = 10
    ),
    exact = FALSE
  )
  parallel <- ngeo_logdet_approx(
    fixture$spatial_weights,
    parameter,
    control = ngeo_solver_control(
      trace_order = 30,
      trace_probes = 120,
      seed = 3402,
      workers = 2,
      exact_threshold = 10
    ),
    exact = FALSE
  )
  direct <- determinant(
    diag(25) - parameter * as.matrix(fixture$spatial_weights$matrix),
    logarithm = TRUE
  )

  expect_s3_class(exact, "ngeo_logdet_estimate")
  expect_equal(
    exact$estimate, as.numeric(direct$modulus),
    tolerance = 1e-12
  )
  expect_identical(serial$estimate, parallel$estimate)
  expect_identical(
    serial$standard_error, parallel$standard_error
  )
  expect_lte(
    abs(serial$estimate - exact$estimate),
    4 * serial$standard_error + serial$truncation_bound
  )
  expect_gt(serial$standard_error, 0)
  expect_gte(serial$truncation_bound, 0)
  expect_error(
    ngeo_logdet_approx(fixture$spatial_weights, 1.1),
    class = "ngeo_error_logdet_bound"
  )
  expect_error(
    ngeo_logdet_approx(
      fixture$spatial_weights,
      parameter,
      control = ngeo_solver_control(exact_threshold = 10),
      exact = TRUE
    ),
    class = "ngeo_error_resource"
  )
})

test_that("iterative SAR and SEM agree with exact-small likelihoods", {
  fixture <- iterative_model_fixture()
  control <- ngeo_solver_control(
    tolerance = 1e-7,
    exact_threshold = 100,
    max_iterations = 1000
  )

  for (model in c("sar", "sem")) {
    exact <- ngeo_spatial_regression(
      fixture$data,
      response = model,
      predictors = c("x", "y"),
      spatial_weights = fixture$spatial_weights,
      model = model
    )
    iterative <- ngeo_spatial_regression_iterative(
      fixture$data,
      response = model,
      predictors = c("x", "y"),
      spatial_weights = fixture$spatial_weights,
      model = model,
      control = control,
      logdet = "exact"
    )

    expect_s3_class(
      iterative, "ngeo_iterative_spatial_regression"
    )
    expect_equal(
      iterative$spatial_parameter,
      exact$spatial_parameter,
      tolerance = 1e-5
    )
    expect_equal(
      iterative$coefficients$estimate,
      exact$coefficients$estimate,
      tolerance = 1e-5
    )
    expect_equal(iterative$logLik, exact$logLik, tolerance = 1e-5)
    expect_true(iterative$optimization$converged)
    expect_identical(
      iterative$log_determinant$method, "exact_small"
    )
    if (model == "sar") {
      expect_true(iterative$solve$converged)
      expect_equal(
        iterative$fitted, exact$fitted, tolerance = 1e-5
      )
    } else {
      expect_null(iterative$solve)
    }
  }
})

test_that("approximate spatial likelihood is explicit and deterministic", {
  fixture <- iterative_model_fixture()
  control <- ngeo_solver_control(
    tolerance = 1e-6,
    trace_order = 20,
    trace_probes = 60,
    seed = 3403,
    workers = 1,
    exact_threshold = 10
  )
  first <- ngeo_spatial_regression_iterative(
    fixture$data,
    "sar",
    c("x", "y"),
    fixture$spatial_weights,
    control = control,
    logdet = "approximate"
  )
  second <- ngeo_spatial_regression_iterative(
    fixture$data,
    "sar",
    c("x", "y"),
    fixture$spatial_weights,
    control = control,
    logdet = "approximate"
  )

  expect_identical(
    first$spatial_parameter, second$spatial_parameter
  )
  expect_identical(
    first$log_determinant$method,
    "hutchinson_power_series"
  )
  expect_false(first$matrix_materialized)
  expect_gt(first$log_determinant$standard_error, 0)
  expect_gte(first$log_determinant$truncation_bound, 0)
})

test_that("iterative CAR agrees with the direct small smoother", {
  fixture <- iterative_model_fixture()
  control <- ngeo_solver_control(tolerance = 1e-8)

  for (type in c("proper", "intrinsic")) {
    exact <- ngeo_car(
      fixture$data,
      "sar",
      fixture$binary,
      type = type,
      rho = 0.5,
      precision = 2
    )
    iterative <- ngeo_car_iterative(
      fixture$data,
      "sar",
      fixture$binary,
      type = type,
      rho = 0.5,
      precision = 2,
      control = control
    )

    expect_s3_class(iterative, "ngeo_iterative_car")
    expect_true(iterative$solve$converged)
    expect_equal(
      iterative$fitted, exact$fitted, tolerance = 1e-7
    )
    expect_equal(
      iterative$residuals, exact$residuals, tolerance = 1e-7
    )
    expect_false(iterative$matrix_materialized)
  }

  expect_error(
    ngeo_car_iterative(
      fixture$data,
      "sar",
      fixture$spatial_weights,
      precision = 2
    ),
    class = "ngeo_error_weights"
  )
})

test_that("6.0 manifests cover iterative model objects", {
  fixture <- iterative_model_fixture()
  control <- ngeo_solver_control(exact_threshold = 100)
  solution <- ngeo_iterative_solve(
    Matrix::Diagonal(3), 1:3, control = control
  )
  logdet <- ngeo_logdet_approx(
    fixture$spatial_weights, 0.2, control = control, exact = TRUE
  )
  model <- ngeo_spatial_regression_iterative(
    fixture$data,
    "sar",
    c("x", "y"),
    fixture$spatial_weights,
    control = control,
    logdet = "exact"
  )
  car <- ngeo_car_iterative(
    fixture$data,
    "sar",
    fixture$binary,
    rho = 0.5,
    precision = 2,
    control = control
  )

  expect_true(all(vapply(
    list(control, solution, logdet, model, car),
    function(object) {
      ngeo_validate(object)
      TRUE
    },
    logical(1)
  )))
  expect_true(all(vapply(
    list(control, solution, logdet, model, car),
    function(object) {
      identical(
        ngeo_object_manifest(object)$specification,
        "NGCS 6.0"
      )
    },
    logical(1)
  )))
})
