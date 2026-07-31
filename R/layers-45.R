.ngeo_layer_digest <- function(value) {
  digest::digest(value, algo = "sha256", serialize = TRUE)
}

.ngeo_layer_key <- function(data, columns, name = "unit") {
  if (!is.data.frame(data) || !length(columns) ||
      any(!columns %in% names(data))) {
    .ngeo_abort(
      sprintf("The `%s` key columns are missing.", name),
      "ngeo_error_layer_metadata"
    )
  }
  values <- lapply(data[columns], function(value) {
    value <- as.character(value)
    if (anyNA(value) || any(!nzchar(value))) {
      .ngeo_abort(
        sprintf("The `%s` key contains missing or empty values.", name),
        "ngeo_error_layer_metadata"
      )
    }
    value
  })
  if (length(values) == 1L) {
    return(values[[1L]])
  }
  do.call(
    paste,
    c(
      Map(function(column, value) paste0(column, "=", value), columns, values),
      sep = "\u001f"
    )
  )
}

.ngeo_layer_availability <- function(map_index, units, layers) {
  result <- matrix(
    FALSE,
    nrow = length(units),
    ncol = length(layers),
    dimnames = list(units, layers)
  )
  result[cbind(
    match(map_index$unit_id, units),
    match(map_index$layer_id, layers)
  )] <- TRUE
  result
}

.ngeo_layer_measure_consistency <- function(x, layer_id) {
  fields <- c(
    "value_type", "spatial_semantics", "units",
    "missing_policy", "default_aggregation"
  )
  output <- lapply(unique(layer_id), function(layer) {
    rows <- which(layer_id == layer)
    current <- unique(x$measures[rows, fields, drop = FALSE])
    data.frame(
      layer = layer,
      consistent = nrow(current) == 1L,
      variants = nrow(current),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, output)
}

#' Validate map columns as an independent-unit by layer index
#'
#' This function inspects map and measurement metadata only. It does not copy
#' or materialize the aligned values block. Duplicate unit-layer entries are
#' rejected rather than averaged implicitly.
#'
#' @param x An `ngeo` object.
#' @param unit One or more map-table columns forming the independent-unit key.
#' @param layer One map-table column identifying the feature/layer.
#' @param required_layers Optional layers required for every unit.
#' @param complete Whether incomplete units are reported or rejected.
#' @param require_consistent_measures Whether each layer must use one
#'   measurement contract across units.
#'
#' @return An `ngeo_layer_index` that references, but does not copy, maps.
#' @export
ngeo_validate_layers <- function(
    x,
    unit = "subject_id",
    layer = "feature",
    required_layers = NULL,
    complete = c("report", "error"),
    require_consistent_measures = TRUE) {
  ngeo_validate(x, "basic")
  complete <- match.arg(complete)
  if (!is.character(unit) || !length(unit) || anyNA(unit) ||
      any(!nzchar(unit)) || anyDuplicated(unit)) {
    .ngeo_abort("`unit` must name one or more unique columns.",
                "ngeo_error_argument")
  }
  if (!is.character(layer) || length(layer) != 1L || is.na(layer) ||
      !nzchar(layer)) {
    .ngeo_abort("`layer` must name one map-table column.",
                "ngeo_error_argument")
  }
  required_columns <- unique(c("map_id", "name", unit, layer))
  missing_columns <- setdiff(required_columns, names(x$maps))
  if (length(missing_columns)) {
    .ngeo_abort(
      sprintf(
        "Map metadata is missing required layer columns: %s.",
        paste(missing_columns, collapse = ", ")
      ),
      "ngeo_error_layer_metadata"
    )
  }

  unit_id <- .ngeo_layer_key(x$maps, unit, "unit")
  layer_id <- .ngeo_layer_key(x$maps, layer, "layer")
  combination <- paste(unit_id, layer_id, sep = "\u001e")
  duplicate <- duplicated(combination) | duplicated(combination, fromLast = TRUE)
  if (any(duplicate)) {
    examples <- unique(combination[duplicate])
    .ngeo_abort(
      paste0(
        "The layer index contains duplicate unit-layer combinations: ",
        paste(utils::head(examples, 5L), collapse = ", "),
        ". Select, aggregate, or contrast replicates explicitly."
      ),
      "ngeo_error_layer_duplicate"
    )
  }

  map_index <- data.frame(
    map_index = seq_len(nrow(x$maps)),
    map_id = as.character(x$maps$map_id),
    map_name = as.character(x$maps$name),
    unit_id = unit_id,
    layer_id = layer_id,
    stringsAsFactors = FALSE
  )
  for (column in unit) {
    map_index[[column]] <- x$maps[[column]]
  }

  unit_rows <- !duplicated(unit_id)
  units <- x$maps[unit_rows, unit, drop = FALSE]
  units$unit_id <- unit_id[unit_rows]
  units <- units[c("unit_id", unit)]
  layers <- unique(layer_id)
  if (!is.null(required_layers)) {
    if (!is.character(required_layers) || !length(required_layers) ||
        anyNA(required_layers) || any(!nzchar(required_layers)) ||
        anyDuplicated(required_layers)) {
      .ngeo_abort("`required_layers` must be unique non-empty names.",
                  "ngeo_error_argument")
    }
    globally_missing <- setdiff(required_layers, layers)
    if (length(globally_missing)) {
      .ngeo_abort(
        sprintf(
          "Required layers are absent from the map table: %s.",
          paste(globally_missing, collapse = ", ")
        ),
        "ngeo_error_layer_missing"
      )
    }
  }
  availability <- .ngeo_layer_availability(map_index, units$unit_id, layers)
  required <- required_layers %||% layers
  incomplete <- if (length(required)) {
    rownames(availability)[!apply(availability[, required, drop = FALSE], 1L, all)]
  } else {
    character()
  }
  if (identical(complete, "error") && length(incomplete)) {
    .ngeo_abort(
      sprintf(
        "Required layers are missing for units: %s.",
        paste(utils::head(incomplete, 10L), collapse = ", ")
      ),
      "ngeo_error_layer_missing"
    )
  }

  measure_consistency <- .ngeo_layer_measure_consistency(x, layer_id)
  if (isTRUE(require_consistent_measures) &&
      any(!measure_consistency$consistent)) {
    invalid <- measure_consistency$layer[!measure_consistency$consistent]
    .ngeo_abort(
      sprintf(
        "Measurement semantics are inconsistent within layers: %s.",
        paste(invalid, collapse = ", ")
      ),
      "ngeo_error_layer_measure"
    )
  }

  index_identity <- list(
    domain_hash = ngeo_domain_hash(x),
    unit_columns = unit,
    layer_column = layer,
    required_layers = required_layers,
    map_index = map_index[c("map_index", "map_id", "unit_id", "layer_id")],
    measures = x$measures
  )
  result <- list(
    map_index = map_index,
    units = units,
    layers = layers,
    availability = availability,
    duplicates = map_index[FALSE, , drop = FALSE],
    measure_consistency = measure_consistency,
    diagnostics = list(
      n_units = nrow(units),
      n_layers = length(layers),
      n_maps = nrow(map_index),
      incomplete_units = incomplete,
      complete = !length(incomplete),
      values_materialized = FALSE
    ),
    domain_hash = ngeo_domain_hash(x),
    index_hash = .ngeo_layer_digest(index_identity),
    unit_columns = unit,
    layer_column = layer,
    required_layers = required_layers
  )
  class(result) <- "ngeo_layer_index"
  result
}

#' @export
print.ngeo_layer_index <- function(x, ...) {
  cat(
    "<ngeo_layer_index>\n",
    "  units: ", nrow(x$units), "\n",
    "  layers: ", length(x$layers), "\n",
    "  maps: ", nrow(x$map_index), "\n",
    "  complete: ", x$diagnostics$complete, "\n",
    "  index hash: ", x$index_hash, "\n",
    sep = ""
  )
  invisible(x)
}

.ngeo_binding_sources <- function(dots, source_id) {
  if (length(dots) == 1L && is.list(dots[[1L]]) &&
      !inherits(dots[[1L]], "ngeo")) {
    nested <- dots[[1L]]
    if (is.null(names(dots)) || !nzchar(names(dots)[[1L]])) {
      dots <- nested
    }
  }
  if (!length(dots) || any(!vapply(dots, inherits, logical(1), "ngeo"))) {
    .ngeo_abort("Map binding requires a non-empty collection of `ngeo` objects.",
                "ngeo_error_argument")
  }
  ids <- source_id
  if (is.null(ids)) {
    ids <- names(dots)
    if (is.null(ids)) ids <- rep("", length(dots))
    empty <- is.na(ids) | !nzchar(ids)
    ids[empty] <- sprintf("source_%03d", which(empty))
  }
  if (!is.character(ids) || length(ids) != length(dots) || anyNA(ids) ||
      any(!nzchar(ids)) || anyDuplicated(ids)) {
    .ngeo_abort("`source_id` must uniquely identify every bound object.",
                "ngeo_error_argument")
  }
  names(dots) <- ids
  dots
}

.ngeo_binding_source_hash <- function(x) {
  values_identity <- if (inherits(x$values, "ngeo_delayed_values")) {
    list(
      class = class(x$values),
      dim = dim(x$values),
      names = colnames(x$values),
      source = x$values$source
    )
  } else {
    digest::digest(x$values, algo = "sha256", serialize = TRUE)
  }
  .ngeo_layer_digest(list(
    domain_hash = ngeo_domain_hash(x),
    maps = x$maps,
    measures = x$measures,
    labels = x$labels,
    values = values_identity
  ))
}

.ngeo_merge_bound_labels <- function(sources, source_ids, conflicts, maps) {
  output <- list()
  for (i in seq_along(sources)) {
    labels <- sources[[i]]$labels
    if (!length(labels)) next
    current_names <- names(labels)
    if (is.null(current_names) || any(!nzchar(current_names))) {
      if (length(labels) != nrow(sources[[i]]$maps)) {
        .ngeo_abort(
          "Unkeyed labels do not align with the source map table.",
          "ngeo_error_labels"
        )
      }
      current_names <- sources[[i]]$maps$map_id
    }
    if (identical(conflicts, "prefix")) {
      current_names <- paste0(source_ids[[i]], "::", current_names)
    }
    for (j in seq_along(labels)) {
      key <- current_names[[j]]
      if (!is.null(output[[key]]) &&
          !identical(output[[key]], labels[[j]])) {
        .ngeo_abort(
          sprintf("Conflicting label metadata for `%s`.", key),
          "ngeo_error_labels"
        )
      }
      output[[key]] <- labels[[j]]
    }
  }
  output
}

#' Bind aligned map columns without changing their spatial domain
#'
#' Inputs must have exactly the same ordered domain. No registration,
#' resampling, nearest-neighbour matching, or name inference is performed.
#'
#' @param ... Named `ngeo` objects, or one named list of them.
#' @param metadata Optional output-map metadata in exact column order.
#' @param source_id Optional deterministic source identifiers.
#' @param conflicts Whether conflicting map identifiers fail or are prefixed.
#' @param storage Automatic, delayed, or in-memory output values.
#' @param budget Hard resource limits checked before materialization.
#'
#' @return An ordinary `ngeo` object with one wider aligned values block.
#' @export
ngeo_bind_maps <- function(
    ...,
    metadata = NULL,
    source_id = NULL,
    conflicts = c("error", "prefix"),
    storage = c("auto", "delayed", "memory"),
    budget = ngeo_resource_budget()) {
  conflicts <- match.arg(conflicts)
  storage <- match.arg(storage)
  sources <- .ngeo_binding_sources(list(...), source_id)
  source_ids <- names(sources)
  lapply(sources, ngeo_validate, level = "basic")
  if (any(vapply(sources, function(x) is.null(x$values), logical(1)))) {
    .ngeo_abort("Every bound source must contain an aligned values block.",
                "ngeo_error_values")
  }

  reference <- sources[[1L]]
  reference_hash <- ngeo_domain_hash(reference)
  reference_elements <- reference$domain$elements$element_id
  reference_space <- ngeo_space_hash(reference$domain$space)
  for (i in seq_along(sources)[-1L]) {
    current <- sources[[i]]
    exact <- identical(ngeo_domain_hash(current), reference_hash) &&
      identical(current$domain$elements$element_id, reference_elements) &&
      identical(ngeo_space_hash(current$domain$space), reference_space) &&
      identical(current$domain$type, reference$domain$type)
    if (!exact) {
      .ngeo_abort(
        paste(
          "The maps do not share the same ordered domain.",
          "No registration or resampling was attempted."
        ),
        "ngeo_error_domain_mismatch"
      )
    }
  }

  maps <- lapply(sources, `[[`, "maps")
  measures <- lapply(sources, `[[`, "measures")
  all_ids <- unlist(lapply(maps, `[[`, "map_id"), use.names = FALSE)
  all_names <- unlist(lapply(maps, `[[`, "name"), use.names = FALSE)
  has_conflict <- anyDuplicated(all_ids) || anyDuplicated(all_names)
  if (has_conflict && identical(conflicts, "error")) {
    .ngeo_abort(
      "Bound map IDs or names conflict; use `conflicts = \"prefix\"` explicitly.",
      "ngeo_error_map_conflict"
    )
  }
  if (identical(conflicts, "prefix")) {
    for (i in seq_along(maps)) {
      maps[[i]]$map_id <- paste0(source_ids[[i]], "::", maps[[i]]$map_id)
      maps[[i]]$name <- paste0(source_ids[[i]], "::", maps[[i]]$name)
      measures[[i]]$map_id <- maps[[i]]$map_id
    }
  }
  output_maps <- do.call(rbind, maps)
  rownames(output_maps) <- NULL
  output_measures <- do.call(rbind, measures)
  rownames(output_measures) <- NULL

  if (!is.null(metadata)) {
    if (!is.data.frame(metadata) || nrow(metadata) != nrow(output_maps)) {
      .ngeo_abort(
        "`metadata` must have one row per output map in exact order.",
        "ngeo_error_alignment"
      )
    }
    overlap <- intersect(names(metadata), names(output_maps))
    for (column in overlap) {
      if (!identical(as.character(metadata[[column]]),
                     as.character(output_maps[[column]]))) {
        .ngeo_abort(
          sprintf("Metadata column `%s` conflicts with bound maps.", column),
          "ngeo_error_alignment"
        )
      }
    }
    additions <- setdiff(names(metadata), names(output_maps))
    output_maps[additions] <- metadata[additions]
  }

  if (identical(storage, "auto")) {
    storage <- if (any(vapply(
      sources,
      function(x) inherits(x$values, "ngeo_delayed_values"),
      logical(1)
    ))) "delayed" else "memory"
  }
  n_element <- nrow(reference$domain$elements)
  column_counts <- vapply(sources, function(x) ncol(x$values), integer(1))
  n_map <- sum(column_counts)
  if (identical(storage, "memory")) {
    cells <- as.double(n_element) * n_map
    .ngeo_budget_assert(budget, "materialized_elements", cells)
    .ngeo_budget_assert(budget, "memory_bytes", cells * 8)
    values <- do.call(cbind, lapply(sources, function(x) {
      as.matrix(x$values)
    }))
    colnames(values) <- output_maps$name
  } else {
    global_source <- rep(seq_along(sources), column_counts)
    global_local <- unlist(lapply(column_counts, seq_len), use.names = FALSE)
    reader <- function(rows, columns) {
      cells <- as.double(length(rows)) * length(columns)
      .ngeo_budget_assert(budget, "materialized_elements", cells)
      .ngeo_budget_assert(budget, "memory_bytes", cells * 8)
      result <- matrix(NA_real_, length(rows), length(columns))
      selected_sources <- unique(global_source[columns])
      for (source in selected_sources) {
        positions <- which(global_source[columns] == source)
        local <- global_local[columns[positions]]
        result[, positions] <- sources[[source]]$values[
          rows, local, drop = FALSE
        ]
      }
      result
    }
    values <- .ngeo_delayed_values(
      reader,
      c(n_element, n_map),
      map_names = output_maps$name,
      source = list(
        method = "composite_delayed_map_binding",
        sources = vapply(sources, .ngeo_binding_source_hash, character(1))
      )
    )
  }

  result <- reference
  result$values <- values
  result$maps <- output_maps
  result$measures <- output_measures
  result$labels <- .ngeo_merge_bound_labels(
    sources, source_ids, conflicts, output_maps
  )
  result$provenance$map_binding <- list(
    method = "exact_ordered_domain_column_binding",
    storage = storage,
    domain_hash = reference_hash,
    space_hash = reference_space,
    sources = lapply(seq_along(sources), function(i) list(
      source_id = source_ids[[i]],
      source_hash = .ngeo_binding_source_hash(sources[[i]]),
      source_domain_hash = ngeo_domain_hash(sources[[i]]),
      original_map_id = as.character(sources[[i]]$maps$map_id),
      output_map_id = as.character(maps[[i]]$map_id)
    )),
    conflicts = conflicts,
    implicit_resampling = FALSE
  )
  ngeo_validate(result, "strict")
  result
}
