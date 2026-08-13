.ngeo_nsim <- function(nsim) {
  nsim <- .ngeo_permutations(nsim)
  if (nsim < 1L) {
    .ngeo_abort(
      "`nsim` must be at least one.",
      "ngeo_error_argument"
    )
  }
  nsim
}

.ngeo_workers <- function(workers) {
  if (!is.numeric(workers) || length(workers) != 1L ||
      is.na(workers) || !is.finite(workers) ||
      workers < 1L || workers != floor(workers)) {
    .ngeo_abort(
      "`workers` must be one positive integer.",
      "ngeo_error_argument"
    )
  }
  as.integer(workers)
}

.ngeo_simulate <- function(nsim, seed, workers, fun) {
  nsim <- .ngeo_nsim(nsim)
  workers <- .ngeo_workers(workers)
  seeds <- .ngeo_with_seed(seed, function() {
    sample.int(.Machine$integer.max, nsim)
  })
  run <- function(i) {
    set.seed(seeds[[i]])
    fun(i)
  }
  if (workers == 1L) {
    return(lapply(seq_len(nsim), run))
  }
  cluster <- parallel::makeCluster(min(workers, nsim))
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  initialize_worker <- function(library_paths) {
    .libPaths(library_paths)
    loadNamespace("Matrix")
    loadNamespace("neurogeo")
    as.character(utils::packageVersion("neurogeo"))
  }
  environment(initialize_worker) <- baseenv()
  worker_versions <- unlist(parallel::clusterCall(
    cluster,
    initialize_worker,
    library_paths = .libPaths()
  ))
  expected_version <- .ngeo_package_version()
  if (any(worker_versions != expected_version)) {
    .ngeo_abort(
      sprintf(
        "Parallel workers loaded neurogeo %s; expected %s.",
        paste(unique(worker_versions), collapse = ", "),
        expected_version
      ),
      "ngeo_error_worker_version"
    )
  }
  parallel::parLapply(
    cluster,
    seq_len(nsim),
    function(i, seeds, simulation_fun) {
      set.seed(seeds[[i]])
      simulation_fun(i)
    },
    seeds = seeds,
    simulation_fun = fun
  )
}

.ngeo_experimental_null <- function(experimental, method) {
  if (!is.logical(experimental) || length(experimental) != 1L ||
      is.na(experimental)) {
    .ngeo_abort(
      "`experimental` must be TRUE or FALSE.",
      "ngeo_error_argument"
    )
  }
  if (!isTRUE(experimental)) {
    .ngeo_abort(
      sprintf(
        paste(
          "%s is uncalibrated and cannot be used for stable inference;",
          "set `experimental = TRUE` only for method evaluation."
        ),
        method
      ),
      "ngeo_error_experimental"
    )
  }
  invisible(TRUE)
}

.ngeo_rotation_matrix <- function() {
  uniform <- stats::runif(3L)
  quaternion <- c(
    x = sqrt(1 - uniform[[1L]]) * sin(2 * pi * uniform[[2L]]),
    y = sqrt(1 - uniform[[1L]]) * cos(2 * pi * uniform[[2L]]),
    z = sqrt(uniform[[1L]]) * sin(2 * pi * uniform[[3L]]),
    w = sqrt(uniform[[1L]]) * cos(2 * pi * uniform[[3L]])
  )
  x <- quaternion[["x"]]
  y <- quaternion[["y"]]
  z <- quaternion[["z"]]
  w <- quaternion[["w"]]
  matrix(c(
    1 - 2 * (y^2 + z^2), 2 * (x * y + z * w),
    2 * (x * z - y * w), 2 * (x * y - z * w),
    1 - 2 * (x^2 + z^2), 2 * (y * z + x * w),
    2 * (x * z + y * w), 2 * (y * z - x * w),
    1 - 2 * (x^2 + y^2)
  ), nrow = 3L, byrow = TRUE)
}

.ngeo_spherical_coordinates <- function(x, coordinates) {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "Surface spin requires a surface base.",
      "ngeo_error_base"
    )
  }
  metadata <- x$base$geometry$coordinate_meta
  if (is.null(coordinates)) {
    candidates <- metadata$name[metadata$role == "registration"]
    if (length(candidates) != 1L) {
      .ngeo_abort(
        "Select one 3D registration coordinate set for surface spin.",
        "ngeo_error_capability"
      )
    }
    coordinates <- candidates[[1L]]
  }
  .ngeo_assert_scalar_character(coordinates, "coordinates")
  if (!coordinates %in% metadata$name ||
      metadata$role[metadata$name == coordinates] != "registration") {
    .ngeo_abort(
      "Spin coordinates must have the explicit `registration` role.",
      "ngeo_error_capability"
    )
  }
  result <- x$base$geometry$coordinates[[coordinates]]
  if (ncol(result) != 3L) {
    .ngeo_abort(
      "Surface spin requires 3D spherical coordinates.",
      "ngeo_error_geometry"
    )
  }
  result
}

#' Generate spherical surface-spin null layers
#'
#' Spins require an explicitly declared 3D registration coordinate set.
#' No anatomical registration is estimated. Optional strata are rotated
#' independently and mappings never cross stratum boundaries.
#'
#' @param x An `ngeo_surface`.
#' @param layer One numeric layer.
#' @param coordinates Registration coordinate-set name.
#' @param nsim Number of null layers.
#' @param seed Reproducible seed.
#' @param strata Optional element-aligned hemisphere or structure strata.
#' @param workers Number of R worker processes.
#' @param sphere_tolerance Maximum relative radial standard deviation.
#' @param experimental Must be `TRUE`. Surface-spin inference remains
#'   experimental until its type-I error has been calibrated.
#'
#' @return An `ngeo_null` object containing simulations and index mappings.
#' @export
ngeo_spin_null <- function(
    x,
    layer = 1L,
    coordinates = NULL,
    nsim = 999L,
    seed = NULL,
    strata = NULL,
    workers = 1L,
    sphere_tolerance = 0.1,
    experimental = FALSE) {
  .ngeo_experimental_null(experimental, "Surface spin")
  .ngeo_require("dbscan", "surface-spin nearest-neighbor mapping")
  ngeo_validate(x, "strict")
  spherical <- .ngeo_spherical_coordinates(x, coordinates)
  layer_index <- .ngeo_layer_selection(x, layer)
  if (length(layer_index) != 1L || is.null(x$values)) {
    .ngeo_abort(
      "Surface spin requires exactly one loaded layer.",
      "ngeo_error_values"
    )
  }
  values <- as.numeric(x$values[, layer_index])
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Surface spin does not accept missing layer values.",
      "ngeo_error_missing"
    )
  }
  if (!is.numeric(sphere_tolerance) ||
      length(sphere_tolerance) != 1L ||
      is.na(sphere_tolerance) || sphere_tolerance <= 0) {
    .ngeo_abort(
      "`sphere_tolerance` must be one positive number.",
      "ngeo_error_argument"
    )
  }
  if (is.null(strata)) {
    component <- ngeo_components(ngeo_adjacency(x, method = "mesh"))
    if (max(component, 0L) > 1L) {
      .ngeo_abort(
        paste(
          "The surface has multiple connected components; provide explicit",
          "hemisphere or structure `strata` to prevent cross-component spins."
        ),
        "ngeo_error_strata_required"
      )
    }
    strata <- rep.int("all", length(values))
  }
  if (length(strata) != length(values) || anyNA(strata)) {
    .ngeo_abort(
      "`strata` must align with surface elements and contain no missing values.",
      "ngeo_error_alignment"
    )
  }
  strata <- as.character(strata)
  groups <- split(seq_along(strata), strata)
  normalized <- matrix(NA_real_, nrow(spherical), 3L)
  for (index in groups) {
    centered <- sweep(
      spherical[index, , drop = FALSE],
      2L,
      colMeans(spherical[index, , drop = FALSE]),
      "-"
    )
    radius <- sqrt(rowSums(centered^2))
    if (any(radius == 0) ||
        stats::sd(radius) / mean(radius) > sphere_tolerance) {
      .ngeo_abort(
        "Registration coordinates are not sufficiently spherical.",
        "ngeo_error_geometry"
      )
    }
    normalized[index, ] <- centered / radius
  }
  simulations <- .ngeo_simulate(nsim, seed, workers, function(...) {
    mapping <- integer(length(values))
    for (index in groups) {
      rotation <- .ngeo_rotation_matrix()
      query <- normalized[index, , drop = FALSE] %*% rotation
      nearest <- dbscan::kNN(
        normalized[index, , drop = FALSE],
        k = 1L,
        query = query
      )
      mapping[index] <- index[as.integer(nearest$id[, 1L])]
    }
    list(values = values[mapping], mapping = mapping)
  })
  mappings <- do.call(cbind, lapply(simulations, `[[`, "mapping"))
  diagnostics <- data.frame(
    simulation = seq_len(ncol(mappings)),
    unique_targets = apply(mappings, 2L, function(mapping) {
      length(unique(mapping))
    }),
    collisions = apply(mappings, 2L, function(mapping) {
      length(mapping) - length(unique(mapping))
    }),
    coverage = apply(mappings, 2L, function(mapping) {
      length(unique(mapping)) / length(mapping)
    }),
    cross_stratum = apply(mappings, 2L, function(mapping) {
      sum(strata != strata[mapping])
    }),
    stringsAsFactors = FALSE
  )
  result <- list(
    method = "surface_spin",
    status = "experimental_uncalibrated",
    simulations = do.call(cbind, lapply(simulations, `[[`, "values")),
    mappings = mappings,
    mapping_diagnostics = diagnostics,
    mapping_policy = "nearest_with_replacement",
    collision_estimand = paste(
      "vertex-assigned rotated field; repeated nearest targets duplicate",
      "source values"
    ),
    preserves_spatial_autocorrelation = FALSE,
    element_id = x$base$elements$element_id,
    layer_id = x$layers$layer_id[[layer_index]],
    layer_name = x$layers$name[[layer_index]],
    base_hash = base_hash(x),
    coordinate_set = coordinates %||%
      x$base$geometry$coordinate_meta$name[
        x$base$geometry$coordinate_meta$role == "registration"
      ][[1L]],
    strata = strata,
    nsim = .ngeo_nsim(nsim),
    seed = .ngeo_seed(seed),
    workers = .ngeo_workers(workers)
  )
  class(result) <- "ngeo_null"
  result
}

.ngeo_moran_basis <- function(matrix) {
  matrix <- as.matrix((matrix + Matrix::t(matrix)) / 2)
  n <- nrow(matrix)
  if (n < 2L) {
    .ngeo_abort(
      "Moran spectral randomization requires at least two elements.",
      "ngeo_error_statistic"
    )
  }
  centered_basis <- qr.Q(qr(stats::contr.helmert(n)))
  reduced <- crossprod(centered_basis, matrix %*% centered_basis)
  decomposition <- eigen(reduced, symmetric = TRUE)
  list(
    vectors = centered_basis %*% decomposition$vectors,
    values = decomposition$values
  )
}

.ngeo_moran_randomizations <- function(values, matrix, nsim, seed = NULL) {
  values <- as.matrix(values)
  basis <- .ngeo_moran_basis(matrix)
  means <- colMeans(values)
  centered <- sweep(values, 2L, means, "-")
  coefficients <- crossprod(basis$vectors, centered)
  draws <- .ngeo_with_seed(seed, function() {
    lapply(seq_len(nsim), function(...) {
      signs <- sample(c(-1, 1), nrow(coefficients), replace = TRUE)
      sweep(
        basis$vectors %*% (coefficients * signs),
        2L, means, "+"
      )
    })
  })
  list(draws = draws, basis = basis)
}

#' Generate Moran spectral randomizations
#'
#' Uses the singleton Moran spectral randomization on a centered orthonormal
#' Moran eigenvector basis. Random sign changes preserve each map's sample
#' mean, centered sum of squares, and Moran quadratic form, including for
#' irregular or row-standardized graphs. When several layers are supplied
#' internally, a shared sign action also preserves their cross-layer spectral
#' dependence. A dense eigendecomposition is guarded by
#' `options(neurogeo.max_spectral_null_elements)`.
#'
#' @inheritParams ngeo_moran
#' @param nsim Number of null layers.
#' @param workers Number of R worker processes.
#' @param experimental Retained for backward compatibility and ignored. The
#'   singleton implementation is algebraically checked in version 6.2; whether
#'   its exchangeability assumptions answer a particular scientific question
#'   remains the caller's responsibility.
#'
#' @return An `ngeo_null` object.
#' @export
ngeo_moran_null <- function(
    x,
    spatial_weights,
    layer = 1L,
    nsim = 999L,
    seed = NULL,
    na_action = c("fail", "omit"),
    zero_policy = FALSE,
    workers = 1L,
    experimental = FALSE) {
  if (!is.logical(experimental) || length(experimental) != 1L ||
      is.na(experimental)) {
    .ngeo_abort(
      "`experimental` must be TRUE or FALSE.",
      "ngeo_error_argument"
    )
  }
  na_action <- match.arg(na_action)
  input <- .ngeo_spatial_inputs(
    x,
    spatial_weights,
    layer,
    na_action,
    zero_policy
  )
  maximum <- getOption("neurogeo.max_spectral_null_elements", 2000L)
  if (length(input$values) > maximum) {
    .ngeo_abort(
      sprintf(
        "Spectral nulls are limited to %d analysed elements.",
        maximum
      ),
      "ngeo_error_resource"
    )
  }
  basis <- .ngeo_moran_basis(input$matrix)
  centered <- input$values - mean(input$values)
  coefficients <- as.numeric(crossprod(basis$vectors, centered))
  simulations <- .ngeo_simulate(nsim, seed, workers, function(...) {
    signs <- sample(c(-1, 1), length(coefficients), replace = TRUE)
    as.numeric(mean(input$values) + basis$vectors %*% (coefficients * signs))
  })
  simulation_matrix <- do.call(cbind, simulations)
  observed_moran <- .ngeo_moran_value(input$values, input$matrix)
  simulated_moran <- apply(
    simulation_matrix, 2L, .ngeo_moran_value, matrix = input$matrix
  )
  result <- list(
    method = "moran_spectral_randomization_singleton",
    status = "stable",
    preserves_spatial_autocorrelation = TRUE,
    preserved_properties = c(
      "sample_mean", "centered_sum_of_squares", "moran_quadratic_form"
    ),
    simulations = simulation_matrix,
    element_id = input$element_id,
    layer_id = input$layer_id,
    layer_name = input$layer_name,
    base_hash = input$base_hash,
    weights_method = input$weights_method,
    normalization = input$normalization,
    observed_moran = observed_moran,
    maximum_moran_error = max(abs(simulated_moran - observed_moran)),
    maximum_mean_error = max(abs(colMeans(simulation_matrix) -
      mean(input$values))),
    maximum_centered_ss_error = max(abs(apply(
      simulation_matrix, 2L,
      function(value) sum((value - mean(value))^2)
    ) - sum(centered^2))),
    nsim = .ngeo_nsim(nsim),
    seed = .ngeo_seed(seed),
    workers = .ngeo_workers(workers),
    omitted = nrow(x$base$elements) - length(input$values)
  )
  class(result) <- "ngeo_null"
  result
}

#' @export
print.ngeo_null <- function(x, ...) {
  cat(
    "<ngeo_null>\n",
    "  method: ", x$method, "\n",
    "  elements: ", nrow(x$simulations), "\n",
    "  simulations: ", ncol(x$simulations), "\n",
    sep = ""
  )
  invisible(x)
}
