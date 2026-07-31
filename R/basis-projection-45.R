.ngeo_projection_index <- function(x, index, maps) {
  selected <- if (is.null(maps)) seq_len(nrow(x$maps)) else
    .ngeo_map_selection(x, maps)
  if (is.null(index)) {
    map_index <- data.frame(
      map_index = selected,
      map_id = x$maps$map_id[selected],
      map_name = x$maps$name[selected],
      unit_id = x$maps$map_id[selected],
      layer_id = "map",
      stringsAsFactors = FALSE
    )
    units <- data.frame(
      unit_id = map_index$unit_id,
      map_id = map_index$map_id,
      stringsAsFactors = FALSE
    )
    return(structure(list(
      map_index = map_index,
      units = units,
      layers = "map",
      domain_hash = ngeo_domain_hash(x),
      index_hash = .ngeo_layer_digest(map_index)
    ), class = "ngeo_layer_index"))
  }
  if (!inherits(index, "ngeo_layer_index") ||
      !identical(index$domain_hash, ngeo_domain_hash(x))) {
    .ngeo_abort("The layer index does not match the projection domain.",
                "ngeo_error_domain_mismatch")
  }
  missing <- setdiff(selected, index$map_index$map_index)
  if (length(missing)) {
    .ngeo_abort("Selected maps are absent from the supplied layer index.",
                "ngeo_error_alignment")
  }
  result <- index
  result$map_index <- result$map_index[
    result$map_index$map_index %in% selected, , drop = FALSE
  ]
  result$layers <- unique(result$map_index$layer_id)
  result
}

.ngeo_projection_measures <- function(x, maps) {
  measures <- x$measures[maps, , drop = FALSE]
  valid <- measures$value_type == "continuous" &
    measures$spatial_semantics == "intensive"
  if (anyNA(valid) || any(!valid)) {
    invalid <- x$maps$name[maps[which(!valid | is.na(valid))]]
    .ngeo_abort(
      sprintf(
        "Spatial projection requires continuous intensive maps: %s.",
        paste(utils::head(invalid, 10L), collapse = ", ")
      ),
      "ngeo_error_measure"
    )
  }
  invisible(TRUE)
}

.ngeo_validate_projection_bands <- function(component, bands) {
  n <- length(component$eigenvalues)
  if (is.null(bands)) {
    return(list(retained = seq_len(n)))
  }
  if (!is.list(bands) || !length(bands)) {
    .ngeo_abort("`bands` must be a non-empty named list of mode indices.",
                "ngeo_error_band")
  }
  band_names <- names(bands)
  if (is.null(band_names) || anyNA(band_names) || any(!nzchar(band_names)) ||
      anyDuplicated(band_names)) {
    .ngeo_abort("Every projection band must have one unique name.",
                "ngeo_error_band")
  }
  bands <- lapply(bands, function(value) {
    value <- .ngeo_as_integer(value, "band modes")
    if (!length(value) || any(value < 1L) || any(value > n) ||
        anyDuplicated(value)) {
      .ngeo_abort("Band modes must be unique retained-mode indices.",
                  "ngeo_error_band")
    }
    sort(value)
  })
  if (anyDuplicated(unlist(bands, use.names = FALSE))) {
    .ngeo_abort("Projection bands may not overlap.", "ngeo_error_band")
  }
  clusters <- component$degenerate_cluster
  for (band in bands) {
    touched <- unique(clusters[band])
    for (cluster in touched) {
      complete <- which(clusters == cluster)
      if (!all(complete %in% band)) {
        .ngeo_abort(
          "The requested band splits a near-degenerate eigenspace.",
          "ngeo_error_band"
        )
      }
    }
  }
  bands
}

.ngeo_project_component <- function(x, component, maps, center, scale,
                                    chunk_rows, chunk_maps) {
  rows <- component$rows
  support <- component$support
  vectors <- component$vectors
  result <- list()
  map_groups <- split(
    seq_along(maps),
    ceiling(seq_along(maps) / chunk_maps)
  )
  row_groups <- split(
    seq_along(rows),
    ceiling(seq_along(rows) / chunk_rows)
  )
  coefficients <- matrix(0, ncol(vectors), length(maps))
  total_energy <- numeric(length(maps))
  means <- numeric(length(maps))
  scales <- rep.int(1, length(maps))
  for (map_group in map_groups) {
    selected_maps <- maps[map_group]
    weighted_sum <- numeric(length(map_group))
    weighted_square <- numeric(length(map_group))
    for (row_group in row_groups) {
      block <- x$values[rows[row_group], selected_maps, drop = FALSE]
      if (any(!is.finite(block))) {
        .ngeo_abort(
          "Basis projection requires one complete finite analysis domain.",
          "ngeo_error_missing"
        )
      }
      current_support <- support[row_group]
      weighted_sum <- weighted_sum + colSums(current_support * block)
      weighted_square <- weighted_square + colSums(current_support * block^2)
    }
    current_means <- if (isTRUE(center)) {
      weighted_sum / sum(support)
    } else {
      rep.int(0, length(map_group))
    }
    centered_energy <- weighted_square -
      2 * current_means * weighted_sum + current_means^2 * sum(support)
    centered_energy[abs(centered_energy) < 1e-12] <- 0
    if (any(centered_energy <= 0)) {
      .ngeo_abort("Basis projection is undefined for a constant map.",
                  "ngeo_error_measure")
    }
    current_scales <- if (isTRUE(scale)) {
      sqrt(centered_energy / sum(support))
    } else {
      rep.int(1, length(map_group))
    }
    current_coefficients <- matrix(0, ncol(vectors), length(map_group))
    current_total <- numeric(length(map_group))
    for (row_group in row_groups) {
      block <- x$values[rows[row_group], selected_maps, drop = FALSE]
      block <- sweep(block, 2L, current_means, "-")
      block <- sweep(block, 2L, current_scales, "/")
      weighted <- support[row_group] * block
      current_coefficients <- current_coefficients +
        crossprod(vectors[row_group, , drop = FALSE], weighted)
      current_total <- current_total + colSums(block * weighted)
    }
    coefficients[, map_group] <- current_coefficients
    total_energy[map_group] <- current_total
    means[map_group] <- current_means
    scales[map_group] <- current_scales
  }
  retained_energy <- colSums(coefficients^2)
  residual_energy <- pmax(0, total_energy - retained_energy)
  roughness <- vapply(seq_len(ncol(coefficients)), function(i) {
    if (retained_energy[[i]] <= 0) return(NA_real_)
    sum(component$eigenvalues * coefficients[, i]^2) /
      retained_energy[[i]]
  }, numeric(1))
  list(
    coefficients = coefficients,
    total_energy = total_energy,
    retained_energy = retained_energy,
    residual_energy = residual_energy,
    retained_variance = retained_energy / total_energy,
    roughness = roughness,
    means = means,
    scales = scales
  )
}

.ngeo_projection_endpoint <- function(estimand, layer, component, band,
                                      modes, basis, recommended = "none") {
  eigenvalues <- component$eigenvalues[modes]
  data.frame(
    endpoint_id = paste(
      estimand, layer, component$component_id, band,
      if (identical(estimand, "coefficient")) sprintf("mode_%04d", modes) else
        "summary",
      sep = "::"
    ),
    family = "basis_projection",
    estimand = estimand,
    layer_x = layer,
    layer_y = NA_character_,
    direction = "none",
    component = component$component_id,
    band = band,
    scale_type = "rank_matched",
    eigenvalue_min = min(eigenvalues),
    eigenvalue_max = max(eigenvalues),
    mode_count = length(modes),
    recommended_transform = recommended,
    basis_hash = basis$basis_hash,
    support_hash = basis$support$hash,
    stringsAsFactors = FALSE
  )
}

#' Project aligned maps into a fixed spatial basis
#'
#' Projection is component-, row-, and map-chunked and returns a small
#' independent-unit by endpoint matrix. Reconstruction is not performed
#' implicitly.
#'
#' @param x An aligned `ngeo` object.
#' @param basis A matching `ngeo_spatial_basis`.
#' @param index Optional `ngeo_layer_index`.
#' @param maps Optional map selection.
#' @param bands Optional named, non-overlapping retained-mode groups.
#' @param center Whether to support-weight center each map.
#' @param scale Whether to support-weight scale each centered map.
#' @param summaries Projection summaries to return.
#' @param chunk_rows Maximum component rows read together.
#' @param chunk_maps Maximum maps read together.
#'
#' @return An `ngeo_subject_features` object.
#' @export
ngeo_basis_project <- function(
    x,
    basis,
    index = NULL,
    maps = NULL,
    bands = NULL,
    center = TRUE,
    scale = FALSE,
    summaries = c(
      "coefficients", "absolute_energy", "relative_energy", "roughness"
    ),
    chunk_rows = 8192L,
    chunk_maps = 32L) {
  ngeo_validate(x, "basic")
  if (!inherits(basis, "ngeo_spatial_basis") ||
      !identical(basis$domain_hash, ngeo_domain_hash(x))) {
    .ngeo_abort("The spatial basis does not match the projection domain.",
                "ngeo_error_domain_mismatch")
  }
  allowed <- c(
    "coefficients", "absolute_energy", "relative_energy", "roughness",
    "retained_variance", "residual_energy"
  )
  if (!is.character(summaries) || !length(summaries) || anyNA(summaries) ||
      any(!summaries %in% allowed) || anyDuplicated(summaries)) {
    .ngeo_abort("Unknown or duplicate basis projection summaries.",
                "ngeo_error_argument")
  }
  chunk_rows <- .ngeo_as_integer(chunk_rows, "chunk_rows")
  chunk_maps <- .ngeo_as_integer(chunk_maps, "chunk_maps")
  if (length(chunk_rows) != 1L || chunk_rows < 1L ||
      length(chunk_maps) != 1L || chunk_maps < 1L) {
    .ngeo_abort("Projection chunk sizes must be positive integers.",
                "ngeo_error_argument")
  }
  index <- .ngeo_projection_index(x, index, maps)
  selected_maps <- index$map_index$map_index
  .ngeo_projection_measures(x, selected_maps)
  unit_ids <- index$units$unit_id
  endpoint_tables <- list()
  endpoint_values <- list()

  for (component in basis$components) {
    current_bands <- .ngeo_validate_projection_bands(component, bands)
    projected <- .ngeo_project_component(
      x, component, selected_maps, center, scale, chunk_rows, chunk_maps
    )
    for (layer in index$layers) {
      layer_rows <- which(index$map_index$layer_id == layer)
      layer_units <- match(index$map_index$unit_id[layer_rows], unit_ids)
      layer_maps <- layer_rows
      add_endpoint <- function(metadata, values) {
        matrix_values <- matrix(NA_real_, length(unit_ids), nrow(metadata))
        matrix_values[layer_units, ] <- values
        endpoint_tables[[length(endpoint_tables) + 1L]] <<- metadata
        endpoint_values[[length(endpoint_values) + 1L]] <<- matrix_values
      }
      if ("coefficients" %in% summaries) {
        for (mode in seq_along(component$eigenvalues)) {
          metadata <- .ngeo_projection_endpoint(
            "coefficient", layer, component,
            sprintf("mode_%04d", mode), mode, basis
          )
          add_endpoint(
            metadata,
            matrix(projected$coefficients[mode, layer_maps], ncol = 1L)
          )
        }
      }
      for (band_name in names(current_bands)) {
        modes <- current_bands[[band_name]]
        band_energy <- colSums(projected$coefficients[modes, , drop = FALSE]^2)
        if ("absolute_energy" %in% summaries) {
          add_endpoint(
            .ngeo_projection_endpoint(
              "absolute_energy", layer, component, band_name, modes, basis,
              recommended = "log1p"
            ),
            matrix(band_energy[layer_maps], ncol = 1L)
          )
        }
        if ("relative_energy" %in% summaries) {
          add_endpoint(
            .ngeo_projection_endpoint(
              "relative_energy", layer, component, band_name, modes, basis,
              recommended = "logit"
            ),
            matrix(
              band_energy[layer_maps] /
                projected$retained_energy[layer_maps],
              ncol = 1L
            )
          )
        }
      }
      all_modes <- seq_along(component$eigenvalues)
      if ("roughness" %in% summaries) {
        add_endpoint(
          .ngeo_projection_endpoint(
            "roughness", layer, component, "retained", all_modes, basis,
            recommended = "log"
          ),
          matrix(projected$roughness[layer_maps], ncol = 1L)
        )
      }
      if ("retained_variance" %in% summaries) {
        add_endpoint(
          .ngeo_projection_endpoint(
            "retained_variance", layer, component, "retained", all_modes,
            basis, recommended = "logit"
          ),
          matrix(projected$retained_variance[layer_maps], ncol = 1L)
        )
      }
      if ("residual_energy" %in% summaries) {
        add_endpoint(
          .ngeo_projection_endpoint(
            "residual_energy", layer, component, "residual", all_modes,
            basis, recommended = "log1p"
          ),
          matrix(projected$residual_energy[layer_maps], ncol = 1L)
        )
      }
    }
  }
  endpoints <- do.call(rbind, endpoint_tables)
  rownames(endpoints) <- NULL
  values <- do.call(cbind, endpoint_values)
  colnames(values) <- endpoints$endpoint_id
  rownames(values) <- unit_ids
  result <- list(
    values = values,
    units = index$units,
    endpoints = endpoints,
    diagnostics = list(
      source_maps = length(selected_maps),
      units = length(unit_ids),
      endpoints = nrow(endpoints),
      center = isTRUE(center),
      scale = isTRUE(scale),
      chunk_rows = chunk_rows,
      chunk_maps = chunk_maps,
      missing_endpoints = sum(!is.finite(values))
    ),
    provenance = list(
      method = "support_weighted_fixed_basis_projection",
      domain_hash = ngeo_domain_hash(x),
      index_hash = index$index_hash,
      basis_hash = basis$basis_hash,
      operator_hash = basis$operator_hash,
      support_hash = basis$support$hash,
      source_map_id = x$maps$map_id[selected_maps],
      values_materialized = FALSE
    )
  )
  class(result) <- "ngeo_subject_features"
  result
}

#' @export
print.ngeo_subject_features <- function(x, ...) {
  cat(
    "<ngeo_subject_features>\n",
    "  units: ", nrow(x$units), "\n",
    "  endpoints: ", nrow(x$endpoints), "\n",
    "  finite cells: ", sum(is.finite(x$values)), "/", length(x$values), "\n",
    sep = ""
  )
  invisible(x)
}
