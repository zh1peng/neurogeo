args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "multilayer-45-validation.json")
required <- c("jsonlite", "Matrix", "RSpectra")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Multilayer 4.5 validation requires: ", paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

grid_adjacency <- function(nrow, ncol) {
  id <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol)
  horizontal_first <- as.vector(id[-nrow, , drop = FALSE])
  horizontal_second <- as.vector(id[-1L, , drop = FALSE])
  vertical_first <- as.vector(id[, -ncol, drop = FALSE])
  vertical_second <- as.vector(id[, -1L, drop = FALSE])
  Matrix::sparseMatrix(
    i = c(
      horizontal_first, horizontal_second,
      vertical_first, vertical_second
    ),
    j = c(
      horizontal_second, horizontal_first,
      vertical_second, vertical_first
    ),
    x = 1,
    dims = rep(nrow * ncol, 2L)
  )
}

make_grid <- function(nrow, ncol, map_count = 0L, delayed = FALSE) {
  n <- nrow * ncol
  adjacency <- grid_adjacency(nrow, ncol)
  values <- NULL
  layers <- NULL
  measures <- NULL
  if (map_count > 0L) {
    map_names <- paste0("map_", seq_len(map_count))
    reader <- function(rows, columns) {
      outer(rows, columns, function(row, column) {
        sin(row / 113) + cos(row / 47) * column + column / 10
      })
    }
    values <- if (isTRUE(delayed)) {
      neurogeo:::.ngeo_delayed_values(
        reader,
        c(n, map_count),
        layer_names = map_names,
        source = "deterministic-grid-callback"
      )
    } else {
      reader(seq_len(n), seq_len(map_count))
    }
    layers <- data.frame(
      layer_id = map_names,
      name = map_names,
      subject_id = map_names,
      feature = "signal",
      stringsAsFactors = FALSE
    )
    measures <- do.call(rbind, replicate(
      map_count,
      ngeo_measure(support_behavior = "intensive", unit = "a.u."),
      simplify = FALSE
    ))
  }
  x <- ngeo_parcellation(
    data.frame(region_id = seq_len(n)),
    values = values,
    support_size = rep(1, n),
    adjacency = adjacency,
    layers = layers,
    measures = measures
  )
  spatial_weights <- ngeo_spatial_weights(x, method = "region_contiguity", style = "B")
  list(x = x, spatial_weights = spatial_weights)
}

checks <- list()

# Analytic path spectrum and full small-case reconstruction.
n <- 8L
path <- ngeo_point(
  cbind(x = seq_len(n), y = 0),
  values = cbind(signal = sin(seq_len(n))),
  layers = data.frame(
    layer_id = "signal", name = "signal",
    subject_id = "subject", feature = "signal"
  ),
  measures = ngeo_measure(support_behavior = "intensive")
)
path_weights <- ngeo_spatial_weights(
  path, method = "distance_band", threshold = 1.01, style = "B"
)
path_basis <- ngeo_spatial_basis(
  path, path_weights, support = "identity", n_modes = n - 1L
)
expected <- 2 - 2 * cos(pi * (1:(n - 1L)) / n)
checks$path_spectrum <- list(
  maximum_absolute_error = max(abs(
    path_basis$components[[1L]]$eigenvalues - expected
  )),
  tolerance = 1e-8
)
checks$path_spectrum$pass <-
  checks$path_spectrum$maximum_absolute_error <= checks$path_spectrum$tolerance
path_index <- ngeo_validate_layers(path, complete = "error")
path_projection <- ngeo_basis_project(
  path,
  path_basis,
  path_index,
  summaries = c("retained_variance", "residual_energy")
)
checks$full_reconstruction <- list(
  retained_variance = unname(path_projection$values[
    , path_projection$endpoints$estimand == "retained_variance"
  ]),
  residual_energy = unname(path_projection$values[
    , path_projection$endpoints$estimand == "residual_energy"
  ])
)
checks$full_reconstruction$pass <-
  max(abs(checks$full_reconstruction$retained_variance - 1)) <= 1e-8 &&
  max(abs(checks$full_reconstruction$residual_energy)) <= 1e-8

# Deterministic delayed/dense equivalence.
dense <- make_grid(6L, 7L, map_count = 4L, delayed = FALSE)
delayed <- make_grid(6L, 7L, map_count = 4L, delayed = TRUE)
dense_basis <- ngeo_spatial_basis(
  dense$x, dense$spatial_weights, n_modes = 12L
)
dense_projection <- ngeo_basis_project(
  dense$x, dense_basis,
  summaries = c("absolute_energy", "roughness"),
  chunk_rows = 11L,
  chunk_layers = 3L
)
delayed_projection <- ngeo_basis_project(
  delayed$x, dense_basis,
  summaries = c("absolute_energy", "roughness"),
  chunk_rows = 7L,
  chunk_layers = 1L
)
checks$delayed_projection <- list(
  maximum_absolute_error = max(abs(
    dense_projection$values - delayed_projection$values
  )),
  tolerance = 1e-10
)
checks$delayed_projection$pass <-
  checks$delayed_projection$maximum_absolute_error <=
  checks$delayed_projection$tolerance

run_scale <- function(label, nrow, ncol, modes) {
  fixture <- make_grid(nrow, ncol, map_count = 5L, delayed = TRUE)
  timing <- system.time({
    basis <- ngeo_spatial_basis(
      fixture$x,
      fixture$spatial_weights,
      n_modes = modes,
      budget = ngeo_resource_budget(
        memory_bytes = 3e9,
        materialized_elements = 2e8
      )
    )
    features <- ngeo_basis_project(
      fixture$x,
      basis,
      summaries = c(
        "absolute_energy", "relative_energy", "roughness",
        "retained_variance", "residual_energy"
      ),
      chunk_rows = 4096L,
      chunk_layers = 2L
    )
  })
  list(
    elements = nrow * ncol,
    modes = modes,
    layers = 5L,
    elapsed_seconds = unname(timing[["elapsed"]]),
    basis_bytes = as.numeric(object.size(basis)),
    feature_bytes = as.numeric(object.size(features)),
    max_residual = basis$diagnostics$max_residual,
    max_orthogonality_error = basis$diagnostics$max_orthogonality_error,
    dense_full_base_matrix = basis$diagnostics$dense_full_base_matrix,
    solver = basis$components[[1L]]$solver,
    finite_features = all(is.finite(features$values)),
    pass = identical(basis$diagnostics$dense_full_base_matrix, FALSE) &&
      basis$diagnostics$max_residual <= 1e-6 &&
      basis$diagnostics$max_orthogonality_error <= 1e-6 &&
      all(is.finite(features$values))
  )
}

full_performance <- identical(
  tolower(Sys.getenv("NEUROGEO_MULTILAYER_FULL_PERF")), "true"
)
if (full_performance) {
  checks$scale_32k <- run_scale("32k", 180L, 180L, 64L)
  checks$scale_91k <- run_scale("91k", 300L, 304L, 64L)
} else {
  checks$scale_32k <- list(
    pass = NA,
    status = "not_evaluated",
    reason = "Set NEUROGEO_MULTILAYER_FULL_PERF=true for the 32k gate."
  )
  checks$scale_91k <- list(
    pass = NA,
    status = "not_evaluated",
    reason = "Set NEUROGEO_MULTILAYER_FULL_PERF=true for the 91k gate."
  )
}

required_checks <- checks[!vapply(checks, function(x) is.na(x$pass), logical(1))]
pass <- all(vapply(required_checks, `[[`, logical(1), "pass"))
report <- list(
  schema = "neurogeo/multilayer-45-validation",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  full_performance = full_performance,
  checks = checks,
  pass = pass,
  claim = paste(
    "Fixed graph basis and chunked projection validation;",
    "no claim about layer coupling or subject inference."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE,
                     digits = 16)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) stop("Multilayer 4.5 validation failed.", call. = FALSE)
