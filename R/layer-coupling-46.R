.ngeo_coupling_allowed <- c(
  "same_location", "spectral_coupling", "band_energy", "roughness",
  "directional_lag", "classic_cross_moran"
)

.ngeo_coupling_pairs <- function(index, pairs, required) {
  if (!required) {
    return(data.frame(x = character(), y = character(),
                      stringsAsFactors = FALSE))
  }
  layers <- index$layers
  if (is.null(pairs)) {
    if (length(layers) != 2L) {
      .ngeo_abort(
        "More than two layers require an explicit pair table or `pairs = \"all\"`.",
        "ngeo_error_layer_pairs"
      )
    }
    pairs <- data.frame(x = layers[[1L]], y = layers[[2L]],
                        stringsAsFactors = FALSE)
  } else if (is.character(pairs) && length(pairs) == 1L &&
             identical(pairs, "all")) {
    if (length(layers) < 2L) {
      .ngeo_abort("Layer coupling requires at least two layers.",
                  "ngeo_error_layer_pairs")
    }
    combinations <- utils::combn(layers, 2L)
    pairs <- data.frame(
      x = combinations[1L, ], y = combinations[2L, ],
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(pairs)) {
    if (all(c("layer_x", "layer_y") %in% names(pairs)) &&
        !all(c("x", "y") %in% names(pairs))) {
      pairs <- data.frame(
        x = pairs$layer_x, y = pairs$layer_y,
        stringsAsFactors = FALSE
      )
    } else if (all(c("x", "y") %in% names(pairs))) {
      pairs <- data.frame(
        x = pairs$x, y = pairs$y, stringsAsFactors = FALSE
      )
    } else {
      .ngeo_abort("`pairs` must contain `x` and `y` layer columns.",
                  "ngeo_error_layer_pairs")
    }
  } else {
    .ngeo_abort("`pairs` must be a pair table or the string `all`.",
                "ngeo_error_layer_pairs")
  }
  pairs$x <- as.character(pairs$x)
  pairs$y <- as.character(pairs$y)
  invalid <- is.na(pairs$x) | is.na(pairs$y) |
    !nzchar(pairs$x) | !nzchar(pairs$y) |
    pairs$x == pairs$y |
    !pairs$x %in% layers | !pairs$y %in% layers
  if (!nrow(pairs) || any(invalid)) {
    .ngeo_abort("Layer pairs must be distinct declared layer names.",
                "ngeo_error_layer_pairs")
  }
  key <- paste(pairs$x, pairs$y, sep = "\u001f")
  if (anyDuplicated(key)) {
    .ngeo_abort("Layer pairs may not be duplicated.",
                "ngeo_error_layer_pairs")
  }
  maximum <- getOption("neurogeo.max_layer_pairs", 1000L)
  if (nrow(pairs) > maximum) {
    .ngeo_abort(
      sprintf("The requested pair family exceeds the limit of %d.", maximum),
      "ngeo_error_resource"
    )
  }
  pairs$pair_id <- paste(pairs$x, pairs$y, sep = "__")
  pairs
}

.ngeo_coupling_index <- function(x, index) {
  if (!inherits(index, "ngeo_layer_index") ||
      !identical(index$base_hash, base_hash(x))) {
    .ngeo_abort("The layer index does not match the coupling base.",
                "ngeo_error_base_mismatch")
  }
  rows <- index$layer_index$layer_index
  valid_rows <- length(rows) == nrow(index$layer_index) &&
    all(rows >= 1L & rows <= nrow(x$layers))
  if (!valid_rows || !identical(
      as.character(x$layers$layer_id[rows]),
      as.character(index$layer_index$source_layer_id))) {
    .ngeo_abort("The layer index is stale relative to the map table.",
                "ngeo_error_alignment")
  }
  lookup <- matrix(
    NA_integer_, nrow(index$unit), length(index$layers),
    dimnames = list(index$unit$unit_id, index$layers)
  )
  lookup[cbind(
    match(index$layer_index$unit_id, index$unit$unit_id),
    match(index$layer_index$layer_id, index$layers)
  )] <- rows
  lookup
}

.ngeo_coupling_support <- function(x, basis) {
  if (is.null(basis)) return(.ngeo_support_weights(x, "auto"))
  if (!inherits(basis, "ngeo_spatial_basis") ||
      !identical(basis$base_hash, base_hash(x))) {
    .ngeo_abort("The spatial basis does not match the coupling base.",
                "ngeo_error_base_mismatch")
  }
  values <- numeric(nrow(x$base$elements))
  for (component in basis$components) {
    values[component$rows] <- component$support
  }
  if (any(values <= 0) || any(!is.finite(values))) {
    .ngeo_abort("The basis does not define positive support for every element.",
                "ngeo_error_support")
  }
  list(values = values, type = basis$support$type,
       hash = basis$support$hash)
}

.ngeo_coupling_measures <- function(x, layers) {
  layers <- unique(layers[is.finite(layers)])
  .ngeo_projection_measures(x, layers)
  invisible(TRUE)
}

.ngeo_coupling_weights <- function(x, spatial_weights) {
  if (!inherits(spatial_weights, "ngeo_spatial_weights")) {
    .ngeo_abort("Directional coupling requires `ngeo_spatial_weights`.",
                "ngeo_error_argument")
  }
  if (!identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort("Spatial spatial_weights do not match the coupling base.",
                "ngeo_error_base_mismatch")
  }
  matrix <- .ngeo_as_dgCMatrix(spatial_weights$matrix)
  if (!identical(dim(matrix), rep(nrow(x$base$elements), 2L)) ||
      any(!is.finite(matrix@x))) {
    .ngeo_abort("Coupling spatial_weights are non-finite or misaligned.",
                "ngeo_error_weights")
  }
  if (any(Matrix::rowSums(abs(matrix)) == 0)) {
    .ngeo_abort(
      "Directional coupling does not retain isolated elements in 4.6.",
      "ngeo_error_zero_policy"
    )
  }
  if (!is.finite(sum(matrix)) || sum(matrix) == 0) {
    .ngeo_abort("Coupling spatial_weights have zero total weight.",
                "ngeo_error_weights")
  }
  list(
    matrix = matrix,
    normalization = spatial_weights$normalization,
    hash = .ngeo_layer_digest(list(
      base_hash = spatial_weights$base_hash,
      method = spatial_weights$method,
      normalization = spatial_weights$normalization,
      matrix = matrix
    ))
  )
}

.ngeo_coupling_values <- function(x, layers) {
  values <- as.matrix(x$values[, layers, drop = FALSE])
  if (any(!is.finite(values))) {
    .ngeo_abort("Layer coupling requires complete finite layers.",
                "ngeo_error_missing")
  }
  values
}

.ngeo_directional_lag <- function(x, y, support, spatial_weights) {
  x <- .ngeo_weighted_center(x, support)$values
  y <- .ngeo_weighted_center(y, support)$values
  lag_y <- as.numeric(spatial_weights %*% y)
  denominator <- .ngeo_weighted_norm(x, support) *
    .ngeo_weighted_norm(lag_y, support)
  if (!is.finite(denominator) || denominator <= 1e-12) {
    .ngeo_abort("Directional lag is undefined for a constant map or lag.",
                "ngeo_error_measure")
  }
  .ngeo_weighted_inner(x, lag_y, support) / denominator
}

.ngeo_classic_cross_moran <- function(x, y, spatial_weights) {
  if (stats::sd(x) <= 0 || stats::sd(y) <= 0) {
    .ngeo_abort("Classic cross-Moran is undefined for a constant map.",
                "ngeo_error_measure")
  }
  x <- (x - mean(x)) / stats::sd(x)
  y <- (y - mean(y)) / stats::sd(y)
  n <- length(x)
  s0 <- sum(spatial_weights)
  (n / s0) * sum(x * as.numeric(spatial_weights %*% y)) / sum(x^2)
}

.ngeo_coupling_layer_unit <- function(x, index, layer) {
  row <- index$layer_index$layer_index[
    match(layer, index$layer_index$layer_id)
  ]
  .ngeo_measures_for_layers(x, row)$unit[[1L]]
}

.ngeo_coupling_endpoint <- function(
    estimand, layer_x, layer_y = NA_character_, direction = "none",
    component = "whole_domain", band = "all", modes = integer(),
    eigenvalues = numeric(), basis = NULL, support_hash,
    weights_info = NULL, energy_floor = NA_real_, unit = "1",
    bounds = "unbounded", standardization = "none",
    recommended_transform = "none", null_target = "subject_or_spatial_map") {
  eigenvalue_min <- if (length(eigenvalues)) min(eigenvalues) else NA_real_
  eigenvalue_max <- if (length(eigenvalues)) max(eigenvalues) else NA_real_
  endpoint_id <- paste(
    estimand, layer_x, ifelse(is.na(layer_y), "none", layer_y), direction,
    component, band, sep = "::"
  )
  data.frame(
    endpoint_id = endpoint_id,
    family = "layer_coupling",
    estimand = estimand,
    layer_x = layer_x,
    layer_y = layer_y,
    direction = direction,
    component = component,
    band = band,
    scale_type = if (identical(component, "whole_domain")) "base" else
      "rank_matched",
    centering = if (identical(estimand, "classic_cross_moran"))
      "arithmetic_mean" else "support_weighted_mean",
    support_weighting = if (identical(estimand, "classic_cross_moran"))
      "none" else "positive_support_mass",
    standardization = standardization,
    weights_normalization = if (is.null(weights_info)) NA_character_ else
      weights_info$normalization,
    energy_floor = energy_floor,
    bounds = bounds,
    unit = unit,
    mode_count = length(modes),
    eigenvalue_min = eigenvalue_min,
    eigenvalue_max = eigenvalue_max,
    recommended_transform = recommended_transform,
    null_target = null_target,
    basis_hash = if (is.null(basis)) NA_character_ else basis$basis_hash,
    operator_hash = if (is.null(basis)) NA_character_ else basis$operator_hash,
    support_hash = support_hash,
    weights_hash = if (is.null(weights_info)) NA_character_ else
      weights_info$hash,
    stringsAsFactors = FALSE
  )
}

.ngeo_coupling_projection <- function(x, basis, selected_layers, chunk_layers) {
  lapply(basis$components, function(component) {
    .ngeo_project_component(
      x, component, selected_layers, center = TRUE, scale = FALSE,
      chunk_rows = min(8192L, length(component$rows)),
      chunk_layers = chunk_layers
    )
  })
}

.ngeo_coupling_null <- function(
    x, index, pairs, basis, bands, spatial_weights, estimands, lag_direction,
    energy_floor, chunk_layers, specification, observed) {
  if (!is.list(specification)) {
    .ngeo_abort("`null` must be an explicit reference-map null specification.",
                "ngeo_error_null_target")
  }
  if (nrow(index$unit) != 1L) {
    .ngeo_abort("Reference-map nulls require exactly one map unit.",
                "ngeo_error_null_target")
  }
  randomized <- specification$randomized_stack
  fixed <- specification$fixed_stack
  preserved <- specification$preserved_properties
  group <- specification$group
  valid_names <- function(value) {
    is.character(value) && length(value) && !anyNA(value) &&
      all(nzchar(value)) && !anyDuplicated(value)
  }
  if (!valid_names(randomized) || !valid_names(fixed) ||
      any(randomized %in% fixed) ||
      any(!c(randomized, fixed) %in% index$layers) ||
      !isTRUE(specification$shared_transformation) ||
      !valid_names(preserved) || !inherits(group, "ngeo_null")) {
    .ngeo_abort(
      "Reference nulls require disjoint randomized/fixed stacks, one shared declared group, and preserved properties.",
      "ngeo_error_null_target"
    )
  }
  if (isTRUE(specification$population_inference)) {
    .ngeo_abort("A spatial-map null cannot request population inference.",
                "ngeo_error_null_target")
  }
  if (!is.null(group$base_hash) &&
      !identical(group$base_hash, base_hash(x))) {
    .ngeo_abort("The null transformation group does not match the base.",
                "ngeo_error_base_mismatch")
  }
  for (row in seq_len(nrow(pairs))) {
    membership <- c(pairs$x[[row]], pairs$y[[row]]) %in% randomized
    fixed_membership <- c(pairs$x[[row]], pairs$y[[row]]) %in% fixed
    if (sum(membership) != 1L || sum(fixed_membership) != 1L) {
      .ngeo_abort(
        "Every tested pair must cross one randomized and one fixed stack.",
        "ngeo_error_null_target"
      )
    }
  }

  mappings <- group$mappings
  simulations <- group$simulations
  n <- nrow(x$base$elements)
  if (!is.null(mappings)) {
    mappings <- as.matrix(mappings)
    if (nrow(mappings) != n || !is.numeric(mappings) ||
        anyNA(mappings) || any(mappings != floor(mappings)) ||
        any(mappings < 1L | mappings > n)) {
      .ngeo_abort("Null mappings must contain valid element indices.",
                  "ngeo_error_null_group")
    }
    nsim <- ncol(mappings)
  } else if (!is.null(simulations) && length(randomized) == 1L) {
    simulations <- as.matrix(simulations)
    if (nrow(simulations) != n || !is.numeric(simulations) ||
        any(!is.finite(simulations))) {
      .ngeo_abort("Null simulations do not align with the randomized layer.",
                  "ngeo_error_null_group")
    }
    nsim <- ncol(simulations)
  } else {
    .ngeo_abort(
      "A shared stack null needs element mappings; simulations support one randomized layer only.",
      "ngeo_error_null_group"
    )
  }
  if (nsim < 1L) {
    .ngeo_abort("The null transformation group is empty.",
                "ngeo_error_null_group")
  }

  base_values <- as.matrix(x$values[, , drop = FALSE])
  randomized_maps <- index$layer_index$layer_index[
    index$layer_index$layer_id %in% randomized
  ]
  simulated_endpoints <- matrix(
    NA_real_, nsim, nrow(observed$endpoints),
    dimnames = list(NULL, observed$endpoints$endpoint_id)
  )
  for (simulation in seq_len(nsim)) {
    current <- x
    current_values <- base_values
    if (!is.null(mappings)) {
      current_values[, randomized_maps] <-
        base_values[mappings[, simulation], randomized_maps, drop = FALSE]
    } else {
      current_values[, randomized_maps] <- simulations[, simulation]
    }
    current$values <- current_values
    repeated <- ngeo_layer_coupling(
      current, index, pairs = pairs[c("x", "y")], basis = basis,
      bands = bands, spatial_weights = spatial_weights, estimands = estimands,
      lag_direction = lag_direction, energy_floor = energy_floor,
      null = NULL, chunk_layers = chunk_layers
    )
    position <- match(observed$endpoints$endpoint_id,
                      repeated$endpoints$endpoint_id)
    if (anyNA(position)) {
      .ngeo_abort("Null endpoint identities changed across transformations.",
                  "ngeo_error_inference")
    }
    simulated_endpoints[simulation, ] <- repeated$values[1L, position]
  }
  observed_values <- observed$values[1L, ]
  p_value <- vapply(seq_along(observed_values), function(column) {
    current <- simulated_endpoints[, column]
    if (!is.finite(observed_values[[column]]) || any(!is.finite(current))) {
      return(NA_real_)
    }
    (1 + sum(abs(current) >= abs(observed_values[[column]]))) / (nsim + 1)
  }, numeric(1))
  identity <- list(
    algorithm = group$method %||% "declared_transformation_group",
    base_hash = base_hash(x), randomized_stack = randomized,
    fixed_stack = fixed, preserved_properties = preserved,
    mappings = mappings, simulations = if (is.null(mappings)) simulations else NULL
  )
  list(
    inference_unit = "spatial_map",
    population_inference = FALSE,
    randomized_stack = randomized,
    fixed_stack = fixed,
    shared_transformation = TRUE,
    preserved_properties = preserved,
    null_algorithm = identity$algorithm,
    null_hash = .ngeo_layer_digest(identity),
    simulations = nsim,
    simulated = simulated_endpoints,
    p_value = p_value
  )
}

#' Generate support-aware layer structure and coupling endpoints
#'
#' One API produces same-location, fixed-basis spectral, and directional layer
#' endpoints. Spatial-map nulls are optional and remain explicitly distinct
#' from subject or population inference.
#'
#' @param x An aligned `ngeo` object.
#' @param index A matching `ngeo_layer_index`.
#' @param pairs Optional `x`/`y` layer table, or `"all"`.
#' @param basis A fixed `ngeo_spatial_basis` for spectral endpoints.
#' @param bands Named non-overlapping retained-mode groups.
#' @param spatial_weights Matching `ngeo_spatial_weights` for directional endpoints.
#' @param estimands Coupling or structure summaries.
#' @param lag_direction Directional lag endpoints to retain.
#' @param energy_floor Minimum band energy for normalized spectral coupling.
#' @param null Optional explicit reference-map spatial-null specification.
#' @param chunk_layers Maximum layers projected together.
#'
#' @return An `ngeo_subject_features` object.
#' @export
ngeo_layer_coupling <- function(
    x,
    index,
    pairs = NULL,
    basis = NULL,
    bands = NULL,
    spatial_weights = NULL,
    estimands = c("same_location", "spectral_coupling"),
    lag_direction = c("both", "x_to_y", "y_to_x"),
    energy_floor = 1e-10,
    null = NULL,
    chunk_layers = 32L) {
  ngeo_validate(x, "basic")
  lookup <- .ngeo_coupling_index(x, index)
  if (any(!index$measure_consistency$consistent)) {
    .ngeo_abort(
      "Layer coupling requires one measurement contract per layer.",
      "ngeo_error_layer_measure"
    )
  }
  if (!is.character(estimands) || !length(estimands) || anyNA(estimands) ||
      any(!estimands %in% .ngeo_coupling_allowed) ||
      anyDuplicated(estimands)) {
    .ngeo_abort("Unknown or duplicate layer coupling estimands.",
                "ngeo_error_argument")
  }
  lag_direction <- match.arg(lag_direction)
  if (!is.numeric(energy_floor) || length(energy_floor) != 1L ||
      is.na(energy_floor) || !is.finite(energy_floor) || energy_floor < 0) {
    .ngeo_abort("`energy_floor` must be one non-negative finite number.",
                "ngeo_error_argument")
  }
  chunk_layers <- .ngeo_as_integer(chunk_layers, "chunk_layers")
  if (length(chunk_layers) != 1L || chunk_layers < 1L) {
    .ngeo_abort("`chunk_layers` must be one positive integer.",
                "ngeo_error_argument")
  }
  pair_estimands <- c(
    "same_location", "spectral_coupling", "directional_lag",
    "classic_cross_moran"
  )
  pair_table <- .ngeo_coupling_pairs(
    index, pairs, any(estimands %in% pair_estimands)
  )
  if (!is.null(null) && !nrow(pair_table)) {
    .ngeo_abort("A reference-map null requires at least one coupling pair.",
                "ngeo_error_null_target")
  }
  need_basis <- any(estimands %in%
    c("spectral_coupling", "band_energy", "roughness"))
  if (need_basis && is.null(basis)) {
    .ngeo_abort("Spectral and structure endpoints require a fixed basis.",
                "ngeo_error_argument")
  }
  support <- .ngeo_coupling_support(x, basis)
  need_weights <- any(estimands %in%
    c("directional_lag", "classic_cross_moran"))
  weights_info <- if (need_weights) .ngeo_coupling_weights(x, spatial_weights) else NULL
  used_layers <- if (any(estimands %in% c("band_energy", "roughness"))) {
    index$layers
  } else {
    unique(c(pair_table$x, pair_table$y))
  }
  used_maps <- as.integer(lookup[, used_layers, drop = FALSE])
  .ngeo_coupling_measures(x, used_maps)

  endpoint_tables <- list()
  endpoint_values <- list()
  low_energy <- list()
  add_endpoint <- function(metadata, values, low = NULL) {
    endpoint_tables[[length(endpoint_tables) + 1L]] <<- metadata
    endpoint_values[[length(endpoint_values) + 1L]] <<-
      matrix(values, ncol = 1L)
    if (!is.null(low) && any(low, na.rm = TRUE)) {
      low_energy[[length(low_energy) + 1L]] <<- data.frame(
        unit_id = index$unit$unit_id[which(low %in% TRUE)],
        endpoint_id = metadata$endpoint_id,
        low_energy = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  if ("same_location" %in% estimands) {
    for (pair in seq_len(nrow(pair_table))) {
      values <- rep.int(NA_real_, nrow(index$unit))
      for (unit in seq_len(nrow(index$unit))) {
        layers <- lookup[unit, c(pair_table$x[[pair]], pair_table$y[[pair]])]
        if (anyNA(layers)) next
        current <- .ngeo_coupling_values(x, layers)
        values[[unit]] <- .ngeo_weighted_correlation(
          current[, 1L], current[, 2L], support$values
        )
      }
      add_endpoint(.ngeo_coupling_endpoint(
        "same_location", pair_table$x[[pair]], pair_table$y[[pair]],
        support_hash = support$hash, bounds = "[-1,1]",
        standardization = "support_weighted_norm"
      ), values)
    }
  }

  selected_layers <- sort(unique(used_maps[is.finite(used_maps)]))
  if (need_basis) {
    projection <- .ngeo_coupling_projection(
      x, basis, selected_layers, chunk_layers
    )
    position <- matrix(
      match(as.integer(lookup), selected_layers),
      nrow = nrow(lookup), dimnames = dimnames(lookup)
    )
    for (component_index in seq_along(basis$components)) {
      component <- basis$components[[component_index]]
      projected <- projection[[component_index]]
      component_bands <- .ngeo_validate_projection_bands(component, bands)
      if ("spectral_coupling" %in% estimands) {
        for (pair in seq_len(nrow(pair_table))) {
          layer_x <- pair_table$x[[pair]]
          layer_y <- pair_table$y[[pair]]
          unit_x <- .ngeo_coupling_layer_unit(x, index, layer_x)
          unit_y <- .ngeo_coupling_layer_unit(x, index, layer_y)
          for (band_name in names(component_bands)) {
            modes <- component_bands[[band_name]]
            energy_x <- energy_y <- cross <- coupling <-
              retained_x <- retained_y <- rep.int(
                NA_real_, nrow(index$unit)
              )
            low <- rep.int(FALSE, nrow(index$unit))
            for (unit in seq_len(nrow(index$unit))) {
              px <- position[unit, layer_x]
              py <- position[unit, layer_y]
              if (is.na(px) || is.na(py)) next
              coefficients_x <- projected$coefficients[modes, px]
              coefficients_y <- projected$coefficients[modes, py]
              energy_x[[unit]] <- sum(coefficients_x^2)
              energy_y[[unit]] <- sum(coefficients_y^2)
              cross[[unit]] <- sum(coefficients_x * coefficients_y)
              retained_x[[unit]] <- projected$retained_variance[[px]]
              retained_y[[unit]] <- projected$retained_variance[[py]]
              low[[unit]] <- energy_x[[unit]] <= energy_floor ||
                energy_y[[unit]] <= energy_floor
              if (!low[[unit]]) {
                coupling[[unit]] <- cross[[unit]] /
                  sqrt(energy_x[[unit]] * energy_y[[unit]])
              }
            }
            common <- list(
              layer_x = layer_x, layer_y = layer_y,
              component = component$component_id, band = band_name,
              modes = modes, eigenvalues = component$eigenvalues[modes],
              basis = basis, support_hash = support$hash,
              energy_floor = energy_floor
            )
            add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
              estimand = "band_energy_x", unit = paste0(unit_x, "^2*support"),
              recommended_transform = "log1p"
            ), common)), energy_x)
            add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
              estimand = "band_energy_y", unit = paste0(unit_y, "^2*support"),
              recommended_transform = "log1p"
            ), common)), energy_y)
            add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
              estimand = "spectral_cross_energy",
              unit = paste(unit_x, unit_y, "support", sep = "*")
            ), common)), cross)
            add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
              estimand = "spectral_coupling", unit = "1", bounds = "[-1,1]",
              standardization = "band_energy_norm"
            ), common)), coupling, low)
            add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
              estimand = "retained_variance_x", unit = "1", bounds = "[0,1]",
              recommended_transform = "logit"
            ), common)), retained_x)
            add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
              estimand = "retained_variance_y", unit = "1", bounds = "[0,1]",
              recommended_transform = "logit"
            ), common)), retained_y)
          }
        }
      }
      if (any(estimands %in% c("band_energy", "roughness"))) {
        for (layer in index$layers) {
          layers <- position[, layer]
          layer_units <- .ngeo_coupling_layer_unit(x, index, layer)
          if ("band_energy" %in% estimands) {
            for (band_name in names(component_bands)) {
              modes <- component_bands[[band_name]]
              absolute <- relative <- rep.int(NA_real_, nrow(index$unit))
              complete <- which(!is.na(layers))
              if (length(complete)) {
                absolute[complete] <- colSums(
                  projected$coefficients[modes, layers[complete], drop = FALSE]^2
                )
                denominator <- projected$retained_energy[layers[complete]]
                stable <- denominator > energy_floor
                relative[complete[stable]] <-
                  absolute[complete[stable]] / denominator[stable]
              }
              common <- list(
                layer_x = layer, component = component$component_id,
                band = band_name, modes = modes,
                eigenvalues = component$eigenvalues[modes], basis = basis,
                support_hash = support$hash
              )
              add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
                estimand = "absolute_energy", unit =
                  paste0(layer_units, "^2*support"),
                recommended_transform = "log1p"
              ), common)), absolute)
              add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
                estimand = "relative_energy", unit = "1", bounds = "[0,1]",
                recommended_transform = "logit"
              ), common)), relative)
            }
          }
          complete <- which(!is.na(layers))
          retained <- residual <- roughness_values <-
            rep.int(NA_real_, nrow(index$unit))
          if (length(complete)) {
            retained[complete] <- projected$retained_variance[layers[complete]]
            residual[complete] <- projected$residual_energy[layers[complete]]
            roughness_values[complete] <- projected$roughness[layers[complete]]
          }
          all_modes <- seq_along(component$eigenvalues)
          common <- list(
            layer_x = layer, component = component$component_id,
            band = "retained", modes = all_modes,
            eigenvalues = component$eigenvalues, basis = basis,
            support_hash = support$hash
          )
          add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
            estimand = "retained_variance", unit = "1", bounds = "[0,1]",
            recommended_transform = "logit"
          ), common)), retained)
          add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
            estimand = "residual_energy", unit =
              paste0(layer_units, "^2*support"),
            recommended_transform = "log1p"
          ), common)), residual)
          if ("roughness" %in% estimands) {
            add_endpoint(do.call(.ngeo_coupling_endpoint, c(list(
              estimand = "roughness", unit = "operator_eigenvalue",
              bounds = "[0,Inf)", recommended_transform = "log"
            ), common)), roughness_values)
          }
        }
      }
    }
  }

  if (need_weights) {
    directions <- switch(
      lag_direction,
      both = c("x_to_y", "y_to_x"),
      x_to_y = "x_to_y",
      y_to_x = "y_to_x"
    )
    for (pair in seq_len(nrow(pair_table))) {
      for (direction in directions) {
        first_layer <- if (direction == "x_to_y") pair_table$x[[pair]] else
          pair_table$y[[pair]]
        second_layer <- if (direction == "x_to_y") pair_table$y[[pair]] else
          pair_table$x[[pair]]
        if ("directional_lag" %in% estimands) {
          values <- rep.int(NA_real_, nrow(index$unit))
          for (unit in seq_len(nrow(index$unit))) {
            layers <- lookup[unit, c(first_layer, second_layer)]
            if (anyNA(layers)) next
            current <- .ngeo_coupling_values(x, layers)
            values[[unit]] <- .ngeo_directional_lag(
              current[, 1L], current[, 2L], support$values,
              weights_info$matrix
            )
          }
          add_endpoint(.ngeo_coupling_endpoint(
            "directional_lag", pair_table$x[[pair]], pair_table$y[[pair]],
            direction = direction, support_hash = support$hash,
            weights_info = weights_info, bounds = "[-1,1]",
            standardization = "support_weighted_lag_norm"
          ), values)
        }
        if ("classic_cross_moran" %in% estimands) {
          values <- rep.int(NA_real_, nrow(index$unit))
          for (unit in seq_len(nrow(index$unit))) {
            layers <- lookup[unit, c(first_layer, second_layer)]
            if (anyNA(layers)) next
            current <- .ngeo_coupling_values(x, layers)
            values[[unit]] <- .ngeo_classic_cross_moran(
              current[, 1L], current[, 2L], weights_info$matrix
            )
          }
          add_endpoint(.ngeo_coupling_endpoint(
            "classic_cross_moran", pair_table$x[[pair]], pair_table$y[[pair]],
            direction = direction, support_hash = support$hash,
            weights_info = weights_info, bounds = "unbounded",
            standardization = "sample_z_n_over_S0"
          ), values)
        }
      }
    }
  }

  endpoints <- do.call(rbind, endpoint_tables)
  rownames(endpoints) <- NULL
  values <- do.call(cbind, endpoint_values)
  colnames(values) <- endpoints$endpoint_id
  rownames(values) <- index$unit$unit_id
  low_table <- if (length(low_energy)) do.call(rbind, low_energy) else
    data.frame(unit_id = character(), endpoint_id = character(),
               low_energy = logical(), stringsAsFactors = FALSE)
  result <- list(
    values = values,
    unit = index$unit,
    endpoints = endpoints,
    diagnostics = list(
      unit = nrow(index$unit), endpoints = nrow(endpoints),
      missing_endpoints = sum(!is.finite(values)),
      low_energy = low_table,
      chunk_layers = chunk_layers,
      elementwise_missingness = "fail",
      absent_unit_layer = "endpoint_NA",
      isolates = "error"
    ),
    history = list(
      method = "support_aware_layer_coupling",
      base_hash = base_hash(x), index_hash = index$index_hash,
      basis_hash = if (is.null(basis)) NA_character_ else basis$basis_hash,
      operator_hash = if (is.null(basis)) NA_character_ else
        basis$operator_hash,
      support_hash = support$hash,
      weights_hash = if (is.null(weights_info)) NA_character_ else
        weights_info$hash,
      source_layer_id = x$layers$layer_id[selected_layers],
      values_materialized = FALSE,
      inference_unit = "independent_unit"
    )
  )
  class(result) <- "ngeo_subject_features"
  if (!is.null(null)) {
    result$null <- .ngeo_coupling_null(
      x, index, pair_table, basis, bands, spatial_weights, estimands,
      lag_direction, energy_floor, chunk_layers, null, result
    )
  }
  result
}
