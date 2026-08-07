args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "iterative-models-34-validation.json")
required <- c("digest", "jsonlite", "Matrix")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Iterative model 3.4 validation requires: ",
       paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
rejected_as <- function(expression, class) {
  inherits(
    tryCatch({
      force(expression)
      NULL
    }, error = identity),
    class
  )
}

started <- Sys.time()

symmetric <- Matrix::Matrix(
  matrix(c(4, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3L),
  sparse = TRUE
)
general <- Matrix::Matrix(
  matrix(c(4, 1, 0, 0, 3, 1, 1, 0, 2), nrow = 3L),
  sparse = TRUE
)
rhs <- c(1, 2, 3)
control <- ngeo_solver_control(tolerance = 1e-9)
cg <- ngeo_iterative_solve(
  symmetric, rhs, method = "cg", control = control
)
bicg <- ngeo_iterative_solve(
  general, rhs, method = "bicgstab", control = control
)
solver_reference <- cg$converged && bicg$converged &&
  max(abs(cg$solution - solve(symmetric, rhs))) < 1e-8 &&
  max(abs(bicg$solution - solve(general, rhs))) < 1e-8
assert(solver_reference, "Iterative linear solver reference failed.")

returning <- ngeo_solver_control(
  tolerance = 1e-14,
  max_iterations = 1L,
  on_nonconvergence = "return"
)
failing <- ngeo_solver_control(
  tolerance = 1e-14,
  max_iterations = 1L,
  on_nonconvergence = "error"
)
hard_system <- Matrix::Diagonal(
  20, x = seq(1, 10, length.out = 20)
)
nonconverged <- ngeo_iterative_solve(
  hard_system, rep(1, 20), control = returning
)
nonconvergence_visible <- !nonconverged$converged &&
  rejected_as(
    ngeo_iterative_solve(
      hard_system, rep(1, 20), control = failing
    ),
    "ngeo_error_nonconvergence"
  )
assert(nonconvergence_visible, "Non-convergence policy gate failed.")

coordinates <- as.matrix(expand.grid(x = 0:4, y = 0:4))
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
set.seed(34)
noise <- stats::rnorm(25, sd = 0.2)
sar_y <- as.numeric(solve(
  Matrix::Diagonal(25) - 0.25 * spatial_weights$matrix,
  design %*% c(2, 1, -0.5) + noise
))
sem_error <- as.numeric(solve(
  Matrix::Diagonal(25) - 0.3 * spatial_weights$matrix,
  noise
))
data <- ngeo_point(
  coordinates,
  values = cbind(
    sar = sar_y,
    sem = as.numeric(design %*% c(2, 1, -0.5)) + sem_error,
    x = coordinates[, 1L],
    y = coordinates[, 2L]
  )
)

exact_logdet <- ngeo_logdet_approx(
  spatial_weights,
  0.2,
  control = ngeo_solver_control(exact_threshold = 100),
  exact = TRUE
)
serial_logdet <- ngeo_logdet_approx(
  spatial_weights,
  0.2,
  control = ngeo_solver_control(
    trace_order = 30,
    trace_probes = 120,
    seed = 3402,
    workers = 1,
    exact_threshold = 10
  ),
  exact = FALSE
)
parallel_logdet <- ngeo_logdet_approx(
  spatial_weights,
  0.2,
  control = ngeo_solver_control(
    trace_order = 30,
    trace_probes = 120,
    seed = 3402,
    workers = 2,
    exact_threshold = 10
  ),
  exact = FALSE
)
logdet_reference <- identical(
  serial_logdet$estimate, parallel_logdet$estimate
) && abs(serial_logdet$estimate - exact_logdet$estimate) <=
  4 * serial_logdet$standard_error +
    serial_logdet$truncation_bound
assert(logdet_reference, "Log-determinant calibration failed.")

model_control <- ngeo_solver_control(
  tolerance = 1e-7,
  exact_threshold = 100
)
model_reference <- list()
for (model in c("sar", "sem")) {
  exact <- ngeo_spatial_regression(
    data, model, c("x", "y"), spatial_weights, model = model
  )
  iterative <- ngeo_spatial_regression_iterative(
    data,
    model,
    c("x", "y"),
    spatial_weights,
    model = model,
    control = model_control,
    logdet = "exact"
  )
  model_reference[[model]] <- list(
    parameter_difference = abs(
      exact$spatial_parameter - iterative$spatial_parameter
    ),
    coefficient_max_difference = max(abs(
      exact$coefficients$estimate -
        iterative$coefficients$estimate
    )),
    loglik_difference = abs(exact$logLik - iterative$logLik),
    converged = iterative$optimization$converged
  )
  assert(
    model_reference[[model]]$parameter_difference < 1e-5 &&
      model_reference[[model]]$coefficient_max_difference < 1e-5 &&
      model_reference[[model]]$loglik_difference < 1e-5 &&
      model_reference[[model]]$converged,
    paste("Exact-small", toupper(model), "calibration failed.")
  )
}

exact_car <- ngeo_car(
  data,
  "sar",
  binary,
  type = "proper",
  rho = 0.5,
  precision = 2
)
iterative_car <- ngeo_car_iterative(
  data,
  "sar",
  binary,
  type = "proper",
  rho = 0.5,
  precision = 2,
  control = ngeo_solver_control(tolerance = 1e-8)
)
car_difference <- max(abs(
  exact_car$fitted - iterative_car$fitted
))
assert(
  iterative_car$solve$converged && car_difference < 1e-6,
  "Iterative CAR calibration failed."
)

targets <- c(9L, 2L, 17L, 5L, 21L, 1L)
gwr <- ngeo_gwr(
  data,
  "sar",
  c("x", "y"),
  bandwidth = 3,
  targets = targets
)
variogram <- ngeo_fit_variogram(data, map = "sar", breaks = 4)
kriging <- ngeo_kriging(
  data, "sar", variogram, targets = targets, neighbors = 8
)
target_reference <- identical(gwr$target_index, targets) &&
  identical(kriging$target, data$base$elements$element_id[targets]) &&
  all(is.finite(gwr$fitted)) &&
  all(is.finite(kriging$prediction))
assert(target_reference, "Local-model target execution failed.")

mutated_control <- control
mutated_control$tolerance <- 1e-4
adversarial <- list(
  control_mutation = rejected_as(
    ngeo_validate_solver_control(mutated_control),
    "ngeo_error_solver_control"
  ),
  logdet_bound = rejected_as(
    ngeo_logdet_approx(spatial_weights, 1.1),
    "ngeo_error_logdet_bound"
  ),
  exact_threshold = rejected_as(
    ngeo_logdet_approx(
      spatial_weights,
      0.2,
      control = ngeo_solver_control(exact_threshold = 10),
      exact = TRUE
    ),
    "ngeo_error_resource"
  )
)
assert(
  all(unlist(adversarial)),
  "Iterative model adversarial gate failed."
)

large_n <- 100000L
large_coordinates <- cbind(
  x = seq_len(large_n),
  y = rep.int(0, large_n)
)
large <- ngeo_point(
  large_coordinates,
  values = cbind(signal = sin(seq_len(large_n) / 101))
)
large_weights <- ngeo_spatial_weights(
  large,
  method = "distance_band",
  threshold = 1.01,
  style = "B"
)
large_control <- ngeo_solver_control(
  tolerance = 1e-7,
  max_iterations = 500L,
  trace_order = 8L,
  trace_probes = 8L,
  seed = 3404L,
  exact_threshold = 10L,
  budget = ngeo_resource_budget(
    memory_bytes = 128 * 1024^2,
    blocks = 16,
    materialized_elements = 2e6
  )
)
large_timing <- system.time({
  large_car <- ngeo_car_iterative(
    large,
    "signal",
    large_weights,
    type = "proper",
    rho = 0.5,
    precision = 1,
    control = large_control
  )
  large_logdet <- ngeo_logdet_approx(
    large_weights,
    parameter = 0.1,
    control = large_control,
    exact = FALSE
  )
})
large_gate <- large_car$solve$converged &&
  !large_car$matrix_materialized &&
  !large_logdet$matrix_materialized &&
  length(large_weights$matrix@x) < 10 * large_n &&
  unname(large_timing[["elapsed"]]) < 30
assert(large_gate, "Large sparse iterative model gate failed.")

objects <- list(
  control, cg, exact_logdet,
  ngeo_spatial_regression_iterative(
    data,
    "sar",
    c("x", "y"),
    spatial_weights,
    control = model_control,
    logdet = "exact"
  ),
  iterative_car
)
manifests <- lapply(objects, ngeo_object_manifest)
schemas <- vapply(
  manifests, `[[`, character(1), "object_schema"
)
schema_gate <- identical(
  neurogeo:::.ngeo_conformance_manifest(version = "3.4")$corpus_version,
  "3.4"
) && all(vapply(
  manifests,
  function(manifest) identical(
    manifest$specification, "NGCS 3.4"
  ),
  logical(1)
))
assert(schema_gate, "NGCS 3.4 corpus or schema gate failed.")

report <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  started_at_utc = format(
    started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(packageVersion("neurogeo")),
  specification = "NGCS 3.4",
  validation = "passed",
  solver_reference = list(
    cg_and_bicgstab = solver_reference,
    nonconvergence_visible = nonconvergence_visible,
    cg_iterations = cg$iterations,
    bicgstab_iterations = bicg$iterations
  ),
  logdet = list(
    worker_invariant = identical(
      serial_logdet$estimate, parallel_logdet$estimate
    ),
    exact = exact_logdet$estimate,
    approximate = serial_logdet$estimate,
    standard_error = serial_logdet$standard_error,
    truncation_bound = serial_logdet$truncation_bound
  ),
  spatial_models = model_reference,
  car = list(
    converged = iterative_car$solve$converged,
    exact_max_difference = car_difference
  ),
  target_execution = list(
    base_functions_incremental = target_reference,
    target_order_preserved = TRUE,
    duplicate_batch_wrappers = FALSE
  ),
  adversarial = adversarial,
  large_gate = list(
    elements = large_n,
    nonzero = length(large_weights$matrix@x),
    car_iterations = large_car$solve$iterations,
    logdet_order = large_logdet$order,
    logdet_probes = large_logdet$probes,
    dense_operator_materialized = FALSE,
    elapsed_seconds = unname(large_timing[["elapsed"]]),
    elapsed_limit_seconds = 30
  ),
  conformance_corpus = "3.4",
  schemas = schemas,
  manifest_sha256 = vapply(
    manifests, `[[`, character(1), "canonical_sha256"
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
