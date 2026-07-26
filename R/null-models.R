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

.ngeo_rotation_matrix <- function() {
  matrix <- matrix(stats::rnorm(9L), nrow = 3L)
  rotation <- qr.Q(qr(matrix))
  if (det(rotation) < 0) {
    rotation[, 1L] <- -rotation[, 1L]
  }
  rotation
}

.ngeo_spherical_coordinates <- function(x, coordinates) {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "Surface spin requires a surface domain.",
      "ngeo_error_domain"
    )
  }
  metadata <- x$domain$coordinate_meta
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
  result <- x$domain$coordinates[[coordinates]]
  if (ncol(result) != 3L) {
    .ngeo_abort(
      "Surface spin requires 3D spherical coordinates.",
      "ngeo_error_geometry"
    )
  }
  result
}

#' Generate spherical surface-spin null maps
#'
#' Spins require an explicitly declared 3D registration coordinate set.
#' No anatomical registration is estimated. Optional strata are rotated
#' independently and mappings never cross stratum boundaries.
#'
#' @param x An `ngeo_surface`.
#' @param map One numeric map.
#' @param coordinates Registration coordinate-set name.
#' @param nsim Number of null maps.
#' @param seed Reproducible seed.
#' @param strata Optional element-aligned hemisphere or structure strata.
#' @param workers Number of R worker processes.
#' @param sphere_tolerance Maximum relative radial standard deviation.
#'
#' @return An `ngeo_null` object containing simulations and index mappings.
#' @export
ngeo_spin_null <- function(
    x,
    map = 1L,
    coordinates = NULL,
    nsim = 999L,
    seed = NULL,
    strata = NULL,
    workers = 1L,
    sphere_tolerance = 0.1) {
  .ngeo_require("dbscan", "surface-spin nearest-neighbor mapping")
  ngeo_validate(x, "strict")
  spherical <- .ngeo_spherical_coordinates(x, coordinates)
  map_index <- .ngeo_map_selection(x, map)
  if (length(map_index) != 1L || is.null(x$values)) {
    .ngeo_abort(
      "Surface spin requires exactly one loaded map.",
      "ngeo_error_values"
    )
  }
  values <- as.numeric(x$values[, map_index])
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Surface spin does not accept missing map values.",
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
  result <- list(
    method = "surface_spin",
    simulations = do.call(cbind, lapply(simulations, `[[`, "values")),
    mappings = do.call(cbind, lapply(simulations, `[[`, "mapping")),
    element_id = x$domain$elements$element_id,
    map_id = x$maps$map_id[[map_index]],
    map_name = x$maps$name[[map_index]],
    domain_hash = ngeo_domain_hash(x),
    coordinate_set = coordinates %||%
      x$domain$coordinate_meta$name[
        x$domain$coordinate_meta$role == "registration"
      ][[1L]],
    strata = strata,
    nsim = .ngeo_nsim(nsim),
    seed = .ngeo_seed(seed),
    workers = .ngeo_workers(workers)
  )
  class(result) <- "ngeo_null"
  result
}

#' Generate Moran spectral randomizations
#'
#' Random sign changes in the eigenbasis of the symmetric graph operator
#' preserve the centered sum of squares and Moran quadratic form, subject to
#' numerical tolerance. A dense eigendecomposition is guarded by
#' `options(neurogeo.max_spectral_null_elements)`.
#'
#' @inheritParams ngeo_moran
#' @param nsim Number of null maps.
#' @param workers Number of R worker processes.
#'
#' @return An `ngeo_null` object.
#' @export
ngeo_moran_null <- function(
    x,
    weights,
    map = 1L,
    nsim = 999L,
    seed = NULL,
    na_action = c("fail", "omit"),
    zero_policy = FALSE,
    workers = 1L) {
  na_action <- match.arg(na_action)
  input <- .ngeo_spatial_inputs(
    x,
    weights,
    map,
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
  operator <- (input$matrix + Matrix::t(input$matrix)) / 2
  decomposition <- eigen(
    as.matrix(operator),
    symmetric = TRUE
  )
  centered <- input$values - mean(input$values)
  coefficients <- as.numeric(
    crossprod(decomposition$vectors, centered)
  )
  simulations <- .ngeo_simulate(nsim, seed, workers, function(...) {
    signs <- sample(c(-1, 1), length(coefficients), replace = TRUE)
    as.numeric(
      mean(input$values) +
        decomposition$vectors %*% (coefficients * signs)
    )
  })
  result <- list(
    method = "moran_spectral",
    simulations = do.call(cbind, simulations),
    element_id = input$element_id,
    map_id = input$map_id,
    map_name = input$map_name,
    domain_hash = input$domain_hash,
    weights_method = input$weights_method,
    normalization = input$normalization,
    observed_moran = .ngeo_moran_value(input$values, input$matrix),
    nsim = .ngeo_nsim(nsim),
    seed = .ngeo_seed(seed),
    workers = .ngeo_workers(workers),
    omitted = nrow(x$domain$elements) - length(input$values)
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
