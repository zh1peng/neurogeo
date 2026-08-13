args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
candidate_tar <- if (length(args)) args[[1L]] else {
  stop("Usage: run-audit-corpus-60.R CANDIDATE_TAR [OUTPUT]", call. = FALSE)
}
output <- if (length(args) >= 2L) args[[2L]] else
  file.path("check-output", "audit-corpus-60.json")

required <- c("digest", "jsonlite", "Matrix", "dbscan", "neurogeo")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Audit corpus requires: ", paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))
sys.source("tools/evidence-identity-60.R", envir = environment())

condition_class <- function(expr, class) {
  inherits(tryCatch(force(expr), error = identity), class)
}

layer_object <- ngeo_point(
  cbind(x = 0:2, y = 0),
  values = cbind(first = 1:3, second = 4:6),
  layers = data.frame(
    layer_id = c("first-id", "second-id"),
    name = c("signal", "signal")
  )
)
layer_condition <- tryCatch(
  values(layer_object, layers = "signal"),
  error = identity
)

values_matrix <- cbind(
  outcome = c(0, 3, 6, 30),
  predictor = 0:3
)
support <- c(100, 100, 100, 1)
weighted <- getFromNamespace(".ngeo_fit_support_effect", "neurogeo")(
  values_matrix, support
)
reference <- summary(stats::lm(
  outcome ~ predictor,
  data = data.frame(
    outcome = values_matrix[, "outcome"],
    predictor = values_matrix[, "predictor"]
  ),
  weights = support
))$coefficients["predictor", ]

atomic_path <- tempfile(fileext = ".txt")
writeLines("old", atomic_path)
rename_calls <- 0L
atomic_condition <- tryCatch(
  getFromNamespace(".ngeo_atomic_write", "neurogeo")(
    atomic_path,
    function(path) writeLines("new", path),
    overwrite = TRUE,
    .operations = list(
      rename = function(from, to) {
        rename_calls <<- rename_calls + 1L
        if (rename_calls == 2L) return(FALSE)
        file.rename(from, to)
      },
      copy = file.copy,
      unlink = unlink
    )
  ),
  error = identity
)

set.seed(6002)
n_rotation <- 6000L
rotation_function <- getFromNamespace(".ngeo_rotation_matrix", "neurogeo")
rotations <- replicate(n_rotation, rotation_function())
rotation_means <- apply(rotations, c(1L, 2L), mean)
rotation_seconds <- apply(rotations^2, c(1L, 2L), mean)
rotation_mean_tolerance <- 6 * sqrt(1 / (3 * n_rotation))

irregular <- ngeo_point(
  cbind(x = c(0, 1, 2, 5, 9), y = 0),
  values = cbind(signal = c(-2, 0, 1, 4, 10))
)
irregular_weights <- ngeo_spatial_weights(
  irregular, method = "knn", k = 2, symmetry = "union", style = "W"
)
surrogate <- ngeo_moran_null(
  irregular,
  irregular_weights,
  "signal",
  nsim = 12,
  seed = 6003,
  zero_policy = TRUE
)
source_values <- irregular$values[, "signal"]
moran_value <- getFromNamespace(".ngeo_moran_value", "neurogeo")
surrogate_moran <- apply(
  surrogate$simulations,
  2L,
  moran_value,
  matrix = irregular_weights$matrix
)
invariant_deltas <- list(
  mean = max(abs(colMeans(surrogate$simulations) - mean(source_values))),
  variance = max(abs(
    apply(surrogate$simulations, 2L, stats::var) - stats::var(source_values)
  )),
  moran = max(abs(surrogate_moran - surrogate$observed_moran))
)

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
metric_condition <- tryCatch(
  ngeo_distance(registration_active, 1, 2, "euclidean"),
  error = identity
)
duplicate <- ngeo_point(rbind(c(0, 0), c(0, 0), c(1, 0)))
inverse_condition <- tryCatch(
  ngeo_spatial_weights(
    duplicate, method = "inverse_distance", k = 1, style = "none"
  ),
  error = identity
)

fit <- structure(
  list(
    model = "gaussian",
    parameters = c(nugget = 0.1, partial_sill = 1, range = 2)
  ),
  class = "ngeo_variogram_fit"
)
surface <- ngeo_surface(
  sphere,
  rbind(c(1, 2, 3), c(1, 2, 4), c(1, 3, 4), c(2, 3, 4)),
  values = cbind(signal = seq_len(4)),
  measures = ngeo_measure(support_behavior = "intensive")
)
kriging_condition <- tryCatch(
  ngeo_kriging(
    surface, "signal", fit, targets = 1:4,
    neighbors = 4, distance_method = "edge_geodesic"
  ),
  error = identity
)

checks <- list(
  layer_ambiguity = inherits(layer_condition, "ngeo_error_layer_ambiguous") &&
    grepl("first-id, second-id", conditionMessage(layer_condition), fixed = TRUE),
  support_weighted_regression = isTRUE(all.equal(
    unname(weighted[c("estimate", "standard_error", "statistic", "p_value")]),
    unname(reference[c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]),
    tolerance = 1e-12
  )),
  failure_safe_overwrite = inherits(atomic_condition, "ngeo_error_io") &&
    identical(readLines(atomic_path), "old"),
  haar_rotation = max(abs(rotation_means)) < rotation_mean_tolerance &&
    max(abs(rotation_seconds - 1 / 3)) < 0.03,
  moran_surrogate_invariants =
    identical(surrogate$status, "stable") &&
    isTRUE(surrogate$preserves_spatial_autocorrelation) &&
    all(unlist(invariant_deltas) < 1e-10),
  metric_role_gate = inherits(metric_condition, "ngeo_error_metric"),
  inverse_distance_gate = inherits(inverse_condition, "ngeo_error_metric"),
  kriging_covariance_gate = inherits(
    kriging_condition, "ngeo_error_covariance_metric"
  )
)

fixtures <- c(
  regression_tests = "tests/testthat/test-audit-regressions-60.R",
  baseline_failures = "inst/audit-corpus-6.0/baseline-failures.json"
)
identity <- ngeo_evidence_identity(candidate_tar, fixtures)
report <- list(
  schema = "neurogeo/evidence-report/1",
  suite = "audit-regression-corpus-6.0",
  candidate = identity,
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  baseline = jsonlite::read_json(
    fixtures[["baseline_failures"]],
    simplifyVector = FALSE
  ),
  seeds = list(haar_rotation = 6002L, moran_surrogate = 6003L),
  diagnostics = list(
    rotation = list(
      n = n_rotation,
      maximum_absolute_mean = max(abs(rotation_means)),
      simultaneous_mean_tolerance = rotation_mean_tolerance,
      maximum_second_moment_error = max(abs(rotation_seconds - 1 / 3))
    ),
    moran_invariant_deltas = invariant_deltas
  ),
  checks = checks,
  pass = all(unlist(checks, use.names = FALSE))
)
if (!isTRUE(report$pass)) {
  stop(
    "The audit regression corpus failed: ",
    paste(names(checks)[!unlist(checks)], collapse = ", "),
    call. = FALSE
  )
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, auto_unbox = TRUE, pretty = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
