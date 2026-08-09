# Sparse iterative solvers and spatial models.
.ngeo_solver_control_payload <- function(x) {
  list(
    schema = "NGCS-solver-control-1",
    tolerance = x$tolerance,
    max_iterations = x$max_iterations,
    trace_order = x$trace_order,
    trace_probes = x$trace_probes,
    seed = x$seed,
    workers = x$workers,
    exact_threshold = x$exact_threshold,
    on_nonconvergence = x$on_nonconvergence,
    budget = unclass(x$budget)
  )
}

#' Declare deterministic iterative solver and approximation controls
#'
#' @param tolerance Positive relative residual tolerance.
#' @param max_iterations Positive solver iteration limit.
#' @param trace_order Positive power-series order for approximate log
#' determinants.
#' @param trace_probes Positive Hutchinson probe count.
#' @param seed Required deterministic probe seed.
#' @param workers Positive deterministic probe worker count.
#' @param exact_threshold Maximum dimension for explicitly requested
#' exact-small log determinants.
#' @param on_nonconvergence Raise a classed error or return diagnostics.
#' @param budget Hard execution resource limits.
#' @return An immutable `ngeo_solver_control`.
#' @export
ngeo_solver_control <- function(
    tolerance = 1e-8,
    max_iterations = 1000L,
    trace_order = 20L,
    trace_probes = 30L,
    seed = 3401L,
    workers = 1L,
    exact_threshold = 200L,
    on_nonconvergence = c("error", "return"),
    budget = ngeo_resource_budget()) {
  integer_field <- c(
    max_iterations = max_iterations,
    trace_order = trace_order,
    trace_probes = trace_probes,
    seed = seed,
    workers = workers,
    exact_threshold = exact_threshold
  )
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0 ||
      tolerance >= 1 ||
      !is.numeric(integer_field) || anyNA(integer_field) ||
      any(!is.finite(integer_field)) ||
      any(integer_field < 1) ||
      any(integer_field != floor(integer_field)) ||
      !inherits(budget, "ngeo_resource_budget")) {
    .ngeo_abort(
      "Solver tolerances, integer controls, seed, or budget are invalid.",
      "ngeo_error_solver_control"
    )
  }
  result <- structure(
    list(
      tolerance = tolerance,
      max_iterations = as.integer(max_iterations),
      trace_order = as.integer(trace_order),
      trace_probes = as.integer(trace_probes),
      seed = as.integer(seed),
      workers = as.integer(workers),
      exact_threshold = as.integer(exact_threshold),
      on_nonconvergence = match.arg(on_nonconvergence),
      budget = budget,
      control_hash = NULL
    ),
    class = "ngeo_solver_control"
  )
  result$control_hash <- digest::digest(
    .ngeo_solver_control_payload(result), algo = "sha256"
  )
  ngeo_validate_solver_control(result)
  result
}

#' Validate iterative solver controls
#'
#' @param x An `ngeo_solver_control`.
#' @return `x`, invisibly.
#' @export
ngeo_validate_solver_control <- function(x) {
  integer_field <- if (inherits(x, "ngeo_solver_control")) {
    c(
      x$max_iterations, x$trace_order, x$trace_probes,
      x$seed, x$workers, x$exact_threshold
    )
  } else {
    numeric()
  }
  if (!inherits(x, "ngeo_solver_control") ||
      !is.numeric(x$tolerance) || length(x$tolerance) != 1L ||
      is.na(x$tolerance) || !is.finite(x$tolerance) ||
      x$tolerance <= 0 || x$tolerance >= 1 ||
      length(integer_field) != 6L || anyNA(integer_field) ||
      any(!is.finite(integer_field)) ||
      any(integer_field < 1) ||
      any(integer_field != floor(integer_field)) ||
      !x$on_nonconvergence %in% c("error", "return") ||
      !inherits(x$budget, "ngeo_resource_budget") ||
      !is.character(x$control_hash) ||
      length(x$control_hash) != 1L ||
      !identical(
        x$control_hash,
        digest::digest(
          .ngeo_solver_control_payload(x), algo = "sha256"
        )
      )) {
    .ngeo_abort(
      "Solver control fields or immutable identity are invalid.",
      "ngeo_error_solver_control"
    )
  }
  invisible(x)
}

.ngeo_operator <- function(operator, n = NULL) {
  if (inherits(operator, "Matrix")) {
    if (nrow(operator) != ncol(operator) ||
        any(!is.finite(operator@x))) {
      .ngeo_abort(
        "Solver matrix must be square and finite.",
        "ngeo_error_solver"
      )
    }
    return(list(
      n = nrow(operator),
      multiply = function(value) {
        as.numeric(operator %*% value)
      },
      matrix = operator
    ))
  }
  if (is.matrix(operator)) {
    if (nrow(operator) != ncol(operator) ||
        any(!is.finite(operator))) {
      .ngeo_abort(
        "Solver matrix must be square and finite.",
        "ngeo_error_solver"
      )
    }
    return(list(
      n = nrow(operator),
      multiply = function(value) {
        as.numeric(operator %*% value)
      },
      matrix = operator
    ))
  }
  if (!is.function(operator) ||
      !is.numeric(n) || length(n) != 1L ||
      is.na(n) || n < 1 || n != floor(n)) {
    .ngeo_abort(
      "A matrix-free operator requires a positive declared dimension.",
      "ngeo_error_solver"
    )
  }
  list(
    n = as.integer(n),
    multiply = function(value) {
      result <- operator(value)
      if (!is.numeric(result) || length(result) != n ||
          any(!is.finite(result))) {
        .ngeo_abort(
          "Matrix-free operator output is invalid.",
          "ngeo_error_solver"
        )
      }
      as.numeric(result)
    },
    matrix = NULL
  )
}

.ngeo_solver_result <- function(
    solution,
    converged,
    iterations,
    residual_history,
    method,
    reason,
    control) {
  result <- structure(
    list(
      solution = solution,
      converged = converged,
      iterations = as.integer(iterations),
      relative_residual = residual_history[[length(residual_history)]],
      residual_history = residual_history,
      method = method,
      reason = reason,
      control_hash = control$control_hash,
      matrix_materialized = FALSE
    ),
    class = "ngeo_iterative_solution"
  )
  if (!converged && identical(control$on_nonconvergence, "error")) {
    condition <- structure(
      list(
        message = paste("Iterative solver did not converge:", reason),
        call = NULL,
        result = result
      ),
      class = c(
        "ngeo_error_nonconvergence", "ngeo_error_solver",
        "ngeo_error", "error", "condition"
      )
    )
    stop(condition)
  }
  result
}

.ngeo_cg <- function(operator, rhs, initial, control) {
  solution <- initial
  residual <- rhs - operator$multiply(solution)
  direction <- residual
  squared <- sum(residual^2)
  norm_rhs <- max(sqrt(sum(rhs^2)), .Machine$double.eps)
  history <- sqrt(squared) / norm_rhs
  if (history[[1L]] <= control$tolerance) {
    return(.ngeo_solver_result(
      solution, TRUE, 0L, history,
      "conjugate_gradient", "initial_residual", control
    ))
  }
  for (iteration in seq_len(control$max_iterations)) {
    product <- operator$multiply(direction)
    denominator <- sum(direction * product)
    if (!is.finite(denominator) ||
        denominator <= .Machine$double.eps) {
      return(.ngeo_solver_result(
        solution, FALSE, iteration - 1L, history,
        "conjugate_gradient", "non_positive_curvature", control
      ))
    }
    step <- squared / denominator
    solution <- solution + step * direction
    residual <- residual - step * product
    updated <- sum(residual^2)
    history <- c(history, sqrt(updated) / norm_rhs)
    if (history[[length(history)]] <= control$tolerance) {
      return(.ngeo_solver_result(
        solution, TRUE, iteration, history,
        "conjugate_gradient", "relative_residual", control
      ))
    }
    direction <- residual + (updated / squared) * direction
    squared <- updated
  }
  .ngeo_solver_result(
    solution, FALSE, control$max_iterations, history,
    "conjugate_gradient", "iteration_limit", control
  )
}

.ngeo_bicgstab <- function(operator, rhs, initial, control) {
  solution <- initial
  residual <- rhs - operator$multiply(solution)
  shadow <- residual
  norm_rhs <- max(sqrt(sum(rhs^2)), .Machine$double.eps)
  history <- sqrt(sum(residual^2)) / norm_rhs
  if (history[[1L]] <= control$tolerance) {
    return(.ngeo_solver_result(
      solution, TRUE, 0L, history,
      "bicgstab", "initial_residual", control
    ))
  }
  rho_old <- alpha <- omega <- 1
  direction <- auxiliary <- numeric(length(rhs))
  for (iteration in seq_len(control$max_iterations)) {
    rho <- sum(shadow * residual)
    if (!is.finite(rho) || abs(rho) <= .Machine$double.eps) {
      return(.ngeo_solver_result(
        solution, FALSE, iteration - 1L, history,
        "bicgstab", "rho_breakdown", control
      ))
    }
    beta <- (rho / rho_old) * (alpha / omega)
    direction <- residual + beta * (direction - omega * auxiliary)
    auxiliary <- operator$multiply(direction)
    denominator <- sum(shadow * auxiliary)
    if (!is.finite(denominator) ||
        abs(denominator) <= .Machine$double.eps) {
      return(.ngeo_solver_result(
        solution, FALSE, iteration - 1L, history,
        "bicgstab", "alpha_breakdown", control
      ))
    }
    alpha <- rho / denominator
    intermediate <- residual - alpha * auxiliary
    intermediate_relative <- sqrt(sum(intermediate^2)) / norm_rhs
    if (intermediate_relative <= control$tolerance) {
      solution <- solution + alpha * direction
      history <- c(history, intermediate_relative)
      return(.ngeo_solver_result(
        solution, TRUE, iteration, history,
        "bicgstab", "relative_residual", control
      ))
    }
    transformed <- operator$multiply(intermediate)
    transformed_squared <- sum(transformed^2)
    if (!is.finite(transformed_squared) ||
        transformed_squared <= .Machine$double.eps) {
      return(.ngeo_solver_result(
        solution, FALSE, iteration - 1L, history,
        "bicgstab", "omega_breakdown", control
      ))
    }
    omega <- sum(transformed * intermediate) / transformed_squared
    if (!is.finite(omega) || abs(omega) <= .Machine$double.eps) {
      return(.ngeo_solver_result(
        solution, FALSE, iteration - 1L, history,
        "bicgstab", "omega_breakdown", control
      ))
    }
    solution <- solution + alpha * direction + omega * intermediate
    residual <- intermediate - omega * transformed
    history <- c(history, sqrt(sum(residual^2)) / norm_rhs)
    if (history[[length(history)]] <= control$tolerance) {
      return(.ngeo_solver_result(
        solution, TRUE, iteration, history,
        "bicgstab", "relative_residual", control
      ))
    }
    rho_old <- rho
  }
  .ngeo_solver_result(
    solution, FALSE, control$max_iterations, history,
    "bicgstab", "iteration_limit", control
  )
}

#' Solve a bounded linear system iteratively
#'
#' @param operator A finite square matrix or matrix-vector function.
#' @param rhs Numeric right-hand-side vector.
#' @param method Conjugate gradient for symmetric positive-definite systems
#' or BiCGSTAB for general systems.
#' @param control An `ngeo_solver_control`.
#' @param n Required dimension for a function operator.
#' @param initial Optional finite starting vector.
#' @return An `ngeo_iterative_solution` with convergence evidence.
#' @export
ngeo_iterative_solve <- function(
    operator,
    rhs,
    method = c("cg", "bicgstab"),
    control = ngeo_solver_control(),
    n = NULL,
    initial = NULL) {
  ngeo_validate_solver_control(control)
  method <- match.arg(method)
  operator <- .ngeo_operator(operator, n)
  if (!is.numeric(rhs) || length(rhs) != operator$n ||
      any(!is.finite(rhs))) {
    .ngeo_abort(
      "Solver right-hand side must be finite and aligned.",
      "ngeo_error_solver"
    )
  }
  if (is.null(initial)) initial <- numeric(operator$n)
  if (!is.numeric(initial) || length(initial) != operator$n ||
      any(!is.finite(initial))) {
    .ngeo_abort(
      "Solver initial values must be finite and aligned.",
      "ngeo_error_solver"
    )
  }
  .ngeo_budget_assert(
    control$budget, "materialized_elements", 8 * operator$n
  )
  .ngeo_budget_assert(
    control$budget, "memory_bytes", 8 * 8 * operator$n
  )
  if (method == "cg") {
    .ngeo_cg(operator, as.numeric(rhs), as.numeric(initial), control)
  } else {
    .ngeo_bicgstab(
      operator, as.numeric(rhs), as.numeric(initial), control
    )
  }
}

.ngeo_logdet_prepare <- function(weight, control, exact = NULL) {
  ngeo_validate_solver_control(control)
  if (!inherits(weight, "Matrix") ||
      nrow(weight) != ncol(weight) ||
      any(!is.finite(weight@x))) {
    .ngeo_abort(
      "Log-determinant spatial_weights must be a finite sparse square matrix.",
      "ngeo_error_logdet"
    )
  }
  n <- nrow(weight)
  exact <- exact %||% (n <= control$exact_threshold)
  if (!is.logical(exact) || length(exact) != 1L || is.na(exact)) {
    .ngeo_abort("`exact` must be TRUE or FALSE.",
                "ngeo_error_argument")
  }
  bound <- max(Matrix::rowSums(abs(weight)))
  if (!is.finite(bound) || bound <= 0) {
    .ngeo_abort(
      "Log-determinant spatial_weights must contain a nonzero relation.",
      "ngeo_error_logdet"
    )
  }
  if (exact) {
    if (n > control$exact_threshold) {
      .ngeo_abort(
        "Exact log-determinant exceeds `exact_threshold`.",
        "ngeo_error_resource"
      )
    }
    return(list(
      method = "exact_small",
      weight = as.matrix(weight),
      n = n,
      norm_bound = bound,
      control_hash = control$control_hash
    ))
  }
  .ngeo_budget_assert(
    control$budget, "blocks", control$trace_probes
  )
  .ngeo_budget_assert(
    control$budget, "materialized_elements",
    as.double(n) * (control$trace_order + 4)
  )
  .ngeo_budget_assert(
    control$budget, "memory_bytes",
    8 * as.double(n) * (control$trace_order + 4)
  )
  samples <- .ngeo_simulate(
    control$trace_probes,
    control$seed,
    control$workers,
    function(...) {
      probe <- sample(c(-1, 1), n, replace = TRUE)
      current <- probe
      vapply(seq_len(control$trace_order), function(k) {
        current <<- as.numeric(weight %*% current)
        sum(probe * current)
      }, numeric(1))
    }
  )
  list(
    method = "hutchinson_power_series",
    samples = do.call(rbind, samples),
    n = n,
    norm_bound = bound,
    control_hash = control$control_hash,
    trace_order = control$trace_order,
    trace_probes = control$trace_probes,
    seed = control$seed,
    workers = control$workers
  )
}

.ngeo_logdet_evaluate <- function(prepared, parameter) {
  if (!is.numeric(parameter) || length(parameter) != 1L ||
      is.na(parameter) || !is.finite(parameter)) {
    .ngeo_abort(
      "Spatial parameter must be one finite number.",
      "ngeo_error_logdet_bound"
    )
  }
  q <- abs(parameter) * prepared$norm_bound
  if (q >= 1) {
    .ngeo_abort(
      "Spatial parameter lies outside the declared convergent bound.",
      "ngeo_error_logdet_bound"
    )
  }
  if (prepared$method == "exact_small") {
    determinant <- determinant(
      diag(prepared$n) - parameter * prepared$weight,
      logarithm = TRUE
    )
    if (determinant$sign <= 0) {
      .ngeo_abort(
        "Spatial transform determinant is not positive.",
        "ngeo_error_logdet"
      )
    }
    return(list(
      estimate = as.numeric(determinant$modulus),
      standard_error = 0,
      truncation_bound = 0,
      method = prepared$method,
      order = NA_integer_,
      probes = 0L
    ))
  }
  coefficient <- -parameter^seq_len(prepared$trace_order) /
    seq_len(prepared$trace_order)
  estimates <- as.numeric(prepared$samples %*% coefficient)
  standard_error <- if (length(estimates) > 1L) {
    stats::sd(estimates) / sqrt(length(estimates))
  } else {
    NA_real_
  }
  order <- prepared$trace_order
  truncation <- prepared$n * q^(order + 1L) /
    ((order + 1L) * (1 - q))
  list(
    estimate = mean(estimates),
    standard_error = standard_error,
    truncation_bound = truncation,
    method = prepared$method,
    order = order,
    probes = prepared$trace_probes
  )
}

#' Estimate a bounded sparse spatial log determinant
#'
#' @param spatial_weights Domain-bound `ngeo_spatial_weights` or a sparse square matrix.
#' @param parameter Spatial dependence parameter.
#' @param control Solver and approximation controls.
#' @param exact Use guarded exact-small evaluation; by default dimensions no
#' larger than `exact_threshold` are exact.
#' @return An `ngeo_logdet_estimate` with Monte Carlo and truncation
#' diagnostics.
#' @export
ngeo_logdet_approx <- function(
    spatial_weights,
    parameter,
    control = ngeo_solver_control(),
    exact = NULL) {
  matrix <- if (inherits(spatial_weights, "ngeo_spatial_weights")) {
    spatial_weights$matrix
  } else {
    spatial_weights
  }
  prepared <- .ngeo_logdet_prepare(matrix, control, exact)
  result <- c(
    .ngeo_logdet_evaluate(prepared, parameter),
    list(
      parameter = parameter,
      dimension = prepared$n,
      norm_bound = prepared$norm_bound,
      seed = if (prepared$method == "exact_small") {
        NA_integer_
      } else {
        prepared$seed
      },
      workers = if (prepared$method == "exact_small") {
        0L
      } else {
        prepared$workers
      },
      control_hash = control$control_hash,
      matrix_materialized = prepared$method == "exact_small"
    )
  )
  class(result) <- "ngeo_logdet_estimate"
  result
}

.ngeo_iterative_model_input <- function(
    x, response, predictors, spatial_weights, na_action, zero_policy) {
  layers <- .ngeo_model_maps(x, response, predictors)
  y <- as.numeric(x$values[, layers$response])
  predictor <- x$values[, layers$predictors, drop = FALSE]
  finite <- is.finite(y)
  if (ncol(predictor)) {
    finite <- finite & apply(is.finite(predictor), 1L, all)
  }
  if (na_action == "fail" && !all(finite)) {
    .ngeo_abort(
      "Model layers contain non-finite values.",
      "ngeo_error_missing"
    )
  }
  index <- which(finite)
  design <- cbind(
    `(Intercept)` = 1,
    predictor[index, , drop = FALSE]
  )
  colnames(design) <- c("(Intercept)", layers$predictor_names)
  if (nrow(design) <= ncol(design)) {
    .ngeo_abort(
      "The spatial design is underpowered.",
      "ngeo_error_model"
    )
  }
  weight <- .ngeo_model_weights(
    x, spatial_weights, index, zero_policy
  )
  list(
    y = y[index],
    design = design,
    weight = weight,
    index = index,
    layers = layers
  )
}

#' Fit deterministic bounded iterative SAR or SEM regression
#'
#' @param x An `ngeo` dataset.
#' @param response One numeric response map.
#' @param predictors Optional numeric predictor layers.
#' @param spatial_weights Matching sparse spatial weights.
#' @param model Spatial lag (SAR) or spatial error (SEM).
#' @param control Solver and log-determinant controls.
#' @param logdet Exact-small or deterministic approximate evaluation.
#' @param interval Optional spatial-parameter search interval inside the
#' convergent norm bound.
#' @param na_action Fail or omit incomplete rows.
#' @param zero_policy Whether isolates are retained.
#' @return An `ngeo_iterative_spatial_regression`.
#' @examples
#' \dontrun{
#' ngeo_spatial_regression_iterative(
#'   x, "response", "predictor", spatial_weights, model = "sar"
#' )
#' }
#' @template stable-statistical-method
#' @export
ngeo_spatial_regression_iterative <- function(
    x,
    response,
    predictors = character(),
    spatial_weights,
    model = c("sar", "sem"),
    control = ngeo_solver_control(),
    logdet = c("auto", "exact", "approximate"),
    interval = NULL,
    na_action = c("fail", "omit"),
    zero_policy = FALSE) {
  model <- match.arg(model)
  logdet <- match.arg(logdet)
  na_action <- match.arg(na_action)
  ngeo_validate_solver_control(control)
  input <- .ngeo_iterative_model_input(
    x, response, predictors, spatial_weights, na_action, zero_policy
  )
  n <- length(input$y)
  .ngeo_budget_assert(
    control$budget, "materialized_elements",
    as.double(n) * (ncol(input$design) + 12)
  )
  .ngeo_budget_assert(
    control$budget, "memory_bytes",
    8 * as.double(n) * (ncol(input$design) + 12)
  )
  exact <- switch(
    logdet,
    auto = NULL,
    exact = TRUE,
    approximate = FALSE
  )
  prepared <- .ngeo_logdet_prepare(input$weight, control, exact)
  limit <- 0.98 / prepared$norm_bound
  if (is.null(interval)) interval <- c(-limit, limit)
  if (!is.numeric(interval) || length(interval) != 2L ||
      anyNA(interval) || any(!is.finite(interval)) ||
      interval[[1L]] >= interval[[2L]] ||
      max(abs(interval)) * prepared$norm_bound >= 1) {
    .ngeo_abort(
      "Model interval must lie strictly within the convergent bound.",
      "ngeo_error_logdet_bound"
    )
  }
  wy <- as.numeric(input$weight %*% input$y)
  wx <- as.matrix(input$weight %*% input$design)
  objective <- function(parameter, details = FALSE) {
    transformed_y <- input$y - parameter * wy
    transformed_design <- if (model == "sem") {
      input$design - parameter * wx
    } else {
      input$design
    }
    fit <- stats::lm.fit(transformed_design, transformed_y)
    if (fit$rank < ncol(input$design)) {
      return(if (details) NULL else Inf)
    }
    sigma2 <- sum(fit$residuals^2) / n
    if (!is.finite(sigma2) || sigma2 <= 0) {
      return(if (details) NULL else Inf)
    }
    determinant <- .ngeo_logdet_evaluate(prepared, parameter)
    negative <- n / 2 * (
      log(2 * pi) + 1 + log(sigma2)
    ) - determinant$estimate
    if (!details) return(negative)
    list(
      parameter = parameter,
      coefficients = as.numeric(fit$coefficients),
      sigma2 = sigma2,
      logLik = -negative,
      determinant = determinant
    )
  }
  optimized <- stats::optimize(
    objective,
    interval = interval,
    tol = control$tolerance
  )
  details <- objective(optimized$minimum, details = TRUE)
  if (is.null(details)) {
    .ngeo_abort(
      "Spatial parameter optimization produced an invalid fit.",
      "ngeo_error_nonconvergence"
    )
  }
  boundary_distance <- min(
    optimized$minimum - interval[[1L]],
    interval[[2L]] - optimized$minimum
  )
  optimization_converged <- !is.null(details) &&
    is.finite(optimized$objective) &&
    boundary_distance > sqrt(control$tolerance)
  if (!optimization_converged &&
      control$on_nonconvergence == "error") {
    .ngeo_abort(
      "Spatial parameter optimization reached a boundary or invalid fit.",
      "ngeo_error_nonconvergence"
    )
  }
  linear_predictor <- as.numeric(
    input$design %*% details$coefficients
  )
  solve_report <- NULL
  fitted <- if (model == "sar") {
    parameter <- details$parameter
    operator <- function(value) {
      value - parameter * as.numeric(input$weight %*% value)
    }
    solve_report <- ngeo_iterative_solve(
      operator,
      linear_predictor,
      method = "bicgstab",
      control = control,
      n = n
    )
    solve_report$solution
  } else {
    linear_predictor
  }
  residuals <- input$y - fitted
  result <- list(
    model = model,
    coefficients = data.frame(
      term = colnames(input$design),
      estimate = details$coefficients,
      stringsAsFactors = FALSE
    ),
    spatial_parameter = details$parameter,
    parameter_name = if (model == "sar") "rho" else "lambda",
    fitted = fitted,
    residuals = residuals,
    element_id = x$base$elements$element_id[input$index],
    complete_index = input$index,
    response = input$layers$response_name,
    predictors = input$layers$predictor_names,
    sigma = sqrt(details$sigma2),
    logLik = details$logLik,
    residual_moran = if (stats::var(residuals) > 0) {
      .ngeo_moran_value(residuals, input$weight)
    } else {
      NA_real_
    },
    log_determinant = details$determinant,
    optimization = list(
      converged = optimization_converged,
      objective = optimized$objective,
      interval = interval,
      boundary_distance = boundary_distance
    ),
    solve = solve_report,
    control_hash = control$control_hash,
    base_hash = base_hash(x),
    weights_method = spatial_weights$method,
    matrix_materialized =
      details$determinant$method == "exact_small"
  )
  class(result) <- "ngeo_iterative_spatial_regression"
  result
}

#' Fit a bounded iterative Gaussian CAR smoother
#'
#' @param x An `ngeo` dataset.
#' @param response One finite numeric map.
#' @param spatial_weights Matching symmetric sparse spatial_weights.
#' @param type Proper or intrinsic CAR.
#' @param rho Proper-CAR dependence in `[0, 1)`.
#' @param precision Positive declared smoothing precision.
#' @param control Iterative solver controls.
#' @param zero_policy Whether isolates are retained.
#' @return An `ngeo_iterative_car`.
#' @examples
#' \dontrun{
#' ngeo_car_iterative(
#'   x, "response", spatial_weights, type = "proper", precision = 1
#' )
#' }
#' @template stable-statistical-method
#' @export
ngeo_car_iterative <- function(
    x,
    response,
    spatial_weights,
    type = c("proper", "intrinsic"),
    rho = 0.95,
    precision = 1,
    control = ngeo_solver_control(),
    zero_policy = FALSE) {
  type <- match.arg(type)
  ngeo_validate_solver_control(control)
  layers <- .ngeo_model_maps(x, response, character())
  y <- as.numeric(x$values[, layers$response])
  if (any(!is.finite(y))) {
    .ngeo_abort(
      "CAR response must be finite.",
      "ngeo_error_missing"
    )
  }
  weight <- .ngeo_model_weights(
    x, spatial_weights, seq_along(y), zero_policy
  )
  asymmetry <- weight - Matrix::t(weight)
  if (length(asymmetry@x) &&
      max(abs(asymmetry@x)) > sqrt(control$tolerance)) {
    .ngeo_abort(
      "Iterative CAR requires symmetric spatial_weights.",
      "ngeo_error_weights"
    )
  }
  weight <- .ngeo_as_dgCMatrix(
    (weight + Matrix::t(weight)) / 2
  )
  degree <- Matrix::rowSums(abs(weight))
  if (any(degree == 0) && !isTRUE(zero_policy)) {
    .ngeo_abort(
      "CAR spatial_weights contain isolates.",
      "ngeo_error_zero_policy"
    )
  }
  if (!is.numeric(rho) || length(rho) != 1L ||
      is.na(rho) || !is.finite(rho) || rho < 0 || rho >= 1 ||
      !is.numeric(precision) || length(precision) != 1L ||
      is.na(precision) || !is.finite(precision) ||
      precision <= 0) {
    .ngeo_abort(
      "CAR `rho` or `precision` is invalid.",
      "ngeo_error_argument"
    )
  }
  q <- Matrix::Diagonal(x = degree) - if (type == "proper") {
    rho * weight
  } else {
    weight
  }
  operator <- Matrix::Diagonal(n = length(y)) + precision * q
  solution <- ngeo_iterative_solve(
    operator, y, method = "cg", control = control
  )
  fitted <- solution$solution
  if (type == "intrinsic") {
    fitted <- fitted - mean(fitted) + mean(y)
  }
  result <- list(
    fitted = fitted,
    residuals = y - fitted,
    precision = precision,
    type = type,
    rho = if (type == "proper") rho else NA_real_,
    constraint = if (type == "intrinsic") {
      "sum-to-zero spatial effect"
    } else {
      "proper precision"
    },
    isolates = which(degree == 0),
    response = layers$response_name,
    base_hash = base_hash(x),
    weights_method = spatial_weights$method,
    solve = solution,
    control_hash = control$control_hash,
    matrix_materialized = FALSE
  )
  class(result) <- "ngeo_iterative_car"
  result
}

#' @export
print.ngeo_solver_control <- function(x, ...) {
  cat(
    "<ngeo_solver_control>\n  tolerance: ", x$tolerance,
    "\n  max iterations: ", x$max_iterations,
    "\n  trace order/probes: ", x$trace_order, "/",
    x$trace_probes,
    "\n  workers: ", x$workers, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_iterative_solution <- function(x, ...) {
  cat(
    "<ngeo_iterative_solution>\n  method: ", x$method,
    "\n  converged: ", x$converged,
    "\n  iterations: ", x$iterations,
    "\n  relative residual: ",
    format(x$relative_residual, digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_logdet_estimate <- function(x, ...) {
  cat(
    "<ngeo_logdet_estimate>\n  method: ", x$method,
    "\n  estimate: ", format(x$estimate, digits = 7L),
    "\n  standard error: ",
    format(x$standard_error, digits = 5L),
    "\n  truncation bound: ",
    format(x$truncation_bound, digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_iterative_spatial_regression <- function(x, ...) {
  cat(
    "<ngeo_iterative_spatial_regression>\n  model: ", x$model,
    "\n  response: ", x$response,
    "\n  ", x$parameter_name, ": ",
    format(x$spatial_parameter, digits = 6L),
    "\n  log determinant: ", x$log_determinant$method,
    "\n  converged: ", x$optimization$converged, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_iterative_car <- function(x, ...) {
  cat(
    "<ngeo_iterative_car>\n  type: ", x$type,
    "\n  response: ", x$response,
    "\n  precision: ", x$precision,
    "\n  converged: ", x$solve$converged, "\n",
    sep = ""
  )
  invisible(x)
}
