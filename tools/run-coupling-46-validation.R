args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "coupling-46-validation.json")
required <- c("jsonlite", "Matrix", "RSpectra", "spdep")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Coupling 4.6 validation requires: ", paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

grid_adjacency <- function(nrow, ncol) {
  id <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol)
  first_h <- as.vector(id[-nrow, , drop = FALSE])
  second_h <- as.vector(id[-1L, , drop = FALSE])
  first_v <- as.vector(id[, -ncol, drop = FALSE])
  second_v <- as.vector(id[, -1L, drop = FALSE])
  Matrix::sparseMatrix(
    i = c(first_h, second_h, first_v, second_v),
    j = c(second_h, first_h, second_v, first_v),
    x = 1, dims = rep(nrow * ncol, 2L)
  )
}

make_grid <- function(nrow, ncol, unit_count = 3L, delayed = TRUE) {
  n <- nrow * ncol
  map_count <- 2L * unit_count
  map_names <- paste0("map_", seq_len(map_count))
  reader <- function(rows, columns) {
    outer(rows, columns, function(row, column) {
      unit <- ceiling(column / 2)
      base <- sin(row / 113) + cos(row / 47) * unit
      ifelse(column %% 2 == 1, base, 0.65 * base + sin(row / 29))
    })
  }
  values <- if (delayed) {
    neurogeo:::.ngeo_delayed_values(
      reader, c(n, map_count), layer_names = map_names,
      source = "deterministic-coupling-grid"
    )
  } else {
    reader(seq_len(n), seq_len(map_count))
  }
  layers <- rep(c("x", "y"), unit_count)
  units <- rep(paste0("unit_", seq_len(unit_count)), each = 2L)
  x <- ngeo_parcellation(
    data.frame(region_id = seq_len(n)),
    values = values,
    support_size = rep.int(1, n),
    adjacency = grid_adjacency(nrow, ncol),
    layers = data.frame(
      layer_id = map_names, name = paste(units, layers, sep = "_"),
      subject_id = units, feature = layers
    ),
    measures = do.call(rbind, replicate(
      map_count,
      ngeo_measure(support_behavior = "intensive", unit = "a.u."),
      simplify = FALSE
    ))
  )
  list(x = x, spatial_weights = ngeo_spatial_weights(
    x, method = "region_contiguity", style = "B"
  ))
}

checks <- list()

# Fixed-basis spectral identities and energy separation.
n <- 8L
path <- ngeo_point(
  cbind(x = seq_len(n), y = 0),
  values = cbind(x = sin(seq_len(n)), y = sin(seq_len(n))),
  layers = data.frame(
    layer_id = c("x", "y"), name = c("x", "y"),
    subject_id = "reference", feature = c("x", "y")
  ),
  measures = rbind(
    ngeo_measure(support_behavior = "intensive", unit = "a.u."),
    ngeo_measure(support_behavior = "intensive", unit = "a.u.")
  )
)
path_weights <- ngeo_spatial_weights(
  path, method = "distance_band", threshold = 1.01, style = "W"
)
path_basis <- ngeo_spatial_basis(
  path, path_weights, support = "identity", n_modes = n - 1L
)
path_result <- ngeo_layer_coupling(
  path, ngeo_validate_layers(path), basis = path_basis,
  bands = list(low = 1:3, high = 4:7),
  estimands = c("same_location", "spectral_coupling")
)
coupling_values <- path_result$values[
  , path_result$endpoints$estimand == "spectral_coupling"
]
checks$spectral_identity <- list(
  maximum_absolute_error = max(abs(coupling_values - 1)),
  energy_endpoints = sum(path_result$endpoints$estimand %in%
    c("band_energy_x", "band_energy_y", "spectral_cross_energy")),
  pass = max(abs(coupling_values - 1)) <= 1e-10 &&
    sum(path_result$endpoints$estimand %in%
      c("band_energy_x", "band_energy_y", "spectral_cross_energy")) == 6L
)

# Classic cross-Moran numerical compatibility.
path$values[, 2L] <- c(2, -1, 4, 0, 3, 5, -2, 1)
moran <- ngeo_layer_coupling(
  path, ngeo_validate_layers(path), spatial_weights = path_weights,
  estimands = "classic_cross_moran", lag_direction = "x_to_y"
)
listw <- spdep::mat2listw(as.matrix(path_weights$matrix), style = "W")
reference <- spdep::moran_bv(
  path$values[, 1L], path$values[, 2L], listw,
  nsim = 2L, scale = TRUE
)$t0
checks$classic_cross_moran <- list(
  observed = as.numeric(moran$values),
  reference = as.numeric(reference),
  absolute_error = abs(as.numeric(moran$values) - as.numeric(reference)),
  pass = abs(as.numeric(moran$values) - as.numeric(reference)) <= 1e-12
)

# Reference-map null history and regime separation.
mappings <- cbind(c(2:n, 1L), rev(seq_len(n)))
group <- structure(list(
  method = "declared_permutation_group", mappings = mappings,
  base_hash = base_hash(path), nsim = ncol(mappings)
), class = "ngeo_null")
null_result <- ngeo_layer_coupling(
  path, ngeo_validate_layers(path), estimands = "same_location",
  null = list(
    randomized_stack = "y", fixed_stack = "x", group = group,
    shared_transformation = TRUE,
    preserved_properties = "shared_element_mapping"
  )
)
checks$reference_null <- list(
  inference_unit = null_result$null$inference_unit,
  population_inference = null_result$null$population_inference,
  null_hash = null_result$null$null_hash,
  simulations = null_result$null$simulations,
  pass = identical(null_result$null$inference_unit, "spatial_map") &&
    identical(null_result$null$population_inference, FALSE) &&
    nzchar(null_result$null$null_hash)
)

run_scale <- function(nrow, ncol) {
  fixture <- make_grid(nrow, ncol, unit_count = 3L, delayed = TRUE)
  timing <- system.time({
    basis <- ngeo_spatial_basis(
      fixture$x, fixture$spatial_weights, n_modes = 64L,
      budget = ngeo_resource_budget(
        memory_bytes = 3e9, materialized_elements = 2e8
      )
    )
    features <- ngeo_layer_coupling(
      fixture$x, ngeo_validate_layers(fixture$x), basis = basis,
      estimands = c("same_location", "spectral_coupling"),
      chunk_layers = 2L
    )
  })
  list(
    elements = nrow * ncol,
    layers = 6L,
    modes = 64L,
    elapsed_seconds = unname(timing[["elapsed"]]),
    basis_bytes = as.numeric(object.size(basis)),
    feature_bytes = as.numeric(object.size(features)),
    maximum_residual = basis$diagnostics$max_residual,
    maximum_orthogonality_error =
      basis$diagnostics$max_orthogonality_error,
    dense_full_base_matrix = basis$diagnostics$dense_full_base_matrix,
    finite_features = all(is.finite(features$values)),
    chunk_layers = features$diagnostics$chunk_layers,
    pass = identical(basis$diagnostics$dense_full_base_matrix, FALSE) &&
      basis$diagnostics$max_residual <= 1e-6 &&
      basis$diagnostics$max_orthogonality_error <= 1e-6 &&
      all(is.finite(features$values)) &&
      identical(features$diagnostics$chunk_layers, 2L)
  )
}

full_performance <- identical(
  tolower(Sys.getenv("NEUROGEO_COUPLING_FULL_PERF")), "true"
)
if (full_performance) {
  checks$scale_32k <- run_scale(180L, 180L)
  checks$scale_91k <- run_scale(300L, 304L)
} else {
  checks$scale_32k <- list(
    pass = NA, status = "not_evaluated",
    reason = "Set NEUROGEO_COUPLING_FULL_PERF=true for the 32k gate."
  )
  checks$scale_91k <- list(
    pass = NA, status = "not_evaluated",
    reason = "Set NEUROGEO_COUPLING_FULL_PERF=true for the 91k gate."
  )
}

required_checks <- checks[!vapply(checks, function(x) is.na(x$pass), logical(1))]
pass <- all(vapply(required_checks, `[[`, logical(1), "pass"))
report <- list(
  schema = "neurogeo/coupling-46-validation",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  full_performance = full_performance,
  checks = checks,
  pass = pass,
  claim = paste(
    "Support-aware same-location, spectral, and directional endpoints;",
    "reference nulls are spatial-map rather than population inference."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, auto_unbox = TRUE, pretty = TRUE, digits = 16
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) stop("Coupling 4.6 validation failed.", call. = FALSE)
