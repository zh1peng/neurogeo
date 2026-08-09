.ngeo_migration_report <- function(status,
                                   base_type = NA_character_,
                                   issues = character(),
                                   field_map = NULL) {
  if (is.null(field_map)) {
    field_map <- data.frame(
      legacy = character(),
      current = character(),
      stringsAsFactors = FALSE
    )
  }
  structure(
    list(
      status = status,
      source_schema = "neurogeo 5.x",
      target_schema = "NGCS 6.0",
      base_type = base_type,
      issues = unique(as.character(issues)),
      field_map = field_map,
      migrated = NULL
    ),
    class = "ngeo_migration_report"
  )
}

.ngeo_legacy_space <- function(space) {
  if (!is.list(space)) {
    stop("The legacy domain does not contain a readable `space`.", call. = FALSE)
  }
  unit <- space$units %||% space$unit
  required <- list(space_id = space$space_id, kind = space$kind, unit = unit)
  invalid <- names(required)[vapply(required, function(value) {
    !is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)
  }, logical(1))]
  if (length(invalid)) {
    stop(
      sprintf("Legacy coordinate-space fields are missing or invalid: %s.",
              paste(invalid, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!required$kind %in% c("unknown", "surface", "volume", "hybrid")) {
    stop("The legacy coordinate-space `kind` is not supported.", call. = FALSE)
  }
  ngeo_coordinate_space(
    space_id = required$space_id,
    kind = required$kind,
    unit = required$unit,
    structure = space$structure,
    template = space$template,
    density = space$density,
    resolution = space$resolution,
    source_metadata = space$source_metadata %||% list()
  )
}

.ngeo_legacy_layers_measures <- function(maps, measures, n_layer) {
  if (!is.data.frame(maps) || nrow(maps) != n_layer ||
      !all(c("map_id", "name") %in% names(maps)) ||
      anyNA(maps$map_id) || any(!nzchar(as.character(maps$map_id))) ||
      anyDuplicated(maps$map_id)) {
    stop("Legacy `maps` must align with values and contain unique `map_id` values.",
         call. = FALSE)
  }
  if (!is.data.frame(measures) || nrow(measures) != n_layer) {
    stop("Legacy `measures` must have one row per map.", call. = FALSE)
  }
  required <- c(
    "value_type", "spatial_semantics", "units",
    "missing_policy", "default_aggregation"
  )
  missing <- setdiff(required, names(measures))
  if (length(missing)) {
    stop(
      sprintf("Legacy `measures` is missing: %s.", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  if ("map_id" %in% names(measures)) {
    rows <- match(as.character(maps$map_id), as.character(measures$map_id))
    if (anyNA(rows) || anyDuplicated(measures$map_id)) {
      stop("Legacy measure-to-map references are ambiguous.", call. = FALSE)
    }
    measures <- measures[rows, , drop = FALSE]
  }

  semantic <- measures[setdiff(names(measures), "map_id")]
  names(semantic)[names(semantic) == "spatial_semantics"] <- "support_behavior"
  names(semantic)[names(semantic) == "units"] <- "unit"
  names(semantic)[names(semantic) == "default_aggregation"] <- "aggregation"
  keys <- vapply(seq_len(nrow(semantic)), function(i) {
    digest::digest(as.list(semantic[i, , drop = FALSE]), algo = "xxhash64")
  }, character(1))
  first <- !duplicated(keys)
  migrated_measures <- semantic[first, , drop = FALSE]
  migrated_measures$measure_id <- paste0("legacy_measure_", substr(keys[first], 1L, 12L))
  migrated_measures <- migrated_measures[
    c("measure_id", setdiff(names(migrated_measures), "measure_id"))
  ]
  layers <- maps
  names(layers)[names(layers) == "map_id"] <- "layer_id"
  layers$measure_id <- migrated_measures$measure_id[match(keys, keys[first])]
  list(layers = layers, measures = migrated_measures)
}

.ngeo_legacy_base <- function(domain, coordinate_space) {
  type_map <- c(
    points = "point",
    surface = "surface",
    volume = "volume",
    regions = "parcellation",
    grayordinates = "grayordinate"
  )
  type <- unname(type_map[[domain$type]])
  if (is.null(type)) {
    stop("The legacy domain type is not supported.", call. = FALSE)
  }
  if (!is.data.frame(domain$elements) || !nrow(domain$elements)) {
    stop("The legacy domain has no ordered element table.", call. = FALSE)
  }

  if (identical(type, "point")) {
    geometry <- list(
      coordinates = domain$coordinates,
      uncertainty = domain$uncertainty
    )
  } else if (identical(type, "surface")) {
    meta <- domain$coordinate_meta
    if (!is.data.frame(meta)) {
      stop("The legacy surface has no coordinate metadata.", call. = FALSE)
    }
    names(meta)[names(meta) == "units"] <- "unit"
    geometry <- list(
      coordinates = domain$coordinates,
      coordinate_meta = meta,
      active_coordinates = domain$active_coordinates,
      faces = domain$faces,
      face_source_index_base = domain$face_source_index_base,
      mask = domain$mask
    )
  } else if (identical(type, "volume")) {
    geometry <- list(
      dim = domain$dim,
      affine = domain$affine,
      voxel_index = domain$voxel_index,
      source_voxel_index = domain$source_voxel_index,
      source_index_base = domain$source_index_base,
      header_transforms = domain$header_transforms %||% list(),
      mask = domain$mask
    )
  } else if (identical(type, "parcellation")) {
    geometry <- list(
      source_base_hash = domain$base_domain_hash,
      membership = domain$membership,
      centroid = domain$centroid,
      support_size = domain$support_size
    )
  } else {
    components <- domain$components
    if (!is.list(components) || !length(components)) {
      stop("The legacy grayordinate domain has no components.", call. = FALSE)
    }
    for (i in seq_along(components)) {
      attached <- components[[i]]$geometry
      if (!is.null(attached) && is.list(attached) && "domain" %in% names(attached)) {
        migrated <- ngeo_migrate_5x(attached)
        if (inherits(migrated, "ngeo_migration_report")) {
          stop("An attached legacy surface geometry requires reconstruction.",
               call. = FALSE)
        }
        components[[i]]$geometry <- migrated
      } else if (!is.null(attached) && !inherits(attached, "ngeo_surface")) {
        stop("An attached surface geometry has an unsupported representation.",
             call. = FALSE)
      }
    }
    geometry <- list(components = components)
  }

  topology <- if (identical(type, "parcellation") &&
      !is.null(domain$adjacency)) {
    list(adjacency = domain$adjacency)
  } else {
    NULL
  }
  class_name <- paste0("ngeo_", type, "_base")
  base::structure(
    list(
      type = type,
      elements = domain$elements,
      geometry = geometry,
      coordinate_space = coordinate_space,
      topology = topology
    ),
    class = c(class_name, "ngeo_base")
  )
}

#' Migrate an in-memory neurogeo 5.x object to NGCS 6.0
#'
#' The migration is conservative: it converts only complete, unambiguous
#' in-memory objects. Unsupported value backends, incomplete geometry, or
#' ambiguous measure references return an `ngeo_migration_report` with no
#' partially migrated object. Source files should then be re-read with a 6.x
#' reader.
#'
#' @param x A serialized or in-memory neurogeo 5.x object.
#' @param strict If `TRUE`, validate the migrated object at the strict level;
#'   otherwise use basic validation.
#'
#' @return A current `ngeo` object on success, or an
#'   `ngeo_migration_report` when reconstruction is required.
#' @templateVar example_call ngeo_migrate_5x(legacy_object)
#' @template stable-governance-core
#' @export
ngeo_migrate_5x <- function(x, strict = TRUE) {
  type <- if (is.list(x) && is.list(x$domain)) {
    x$domain$type %||% NA_character_
  } else {
    NA_character_
  }
  field_map <- data.frame(
    legacy = c(
      "domain", "domain$space", "maps", "maps$map_id", "provenance",
      "measures$spatial_semantics", "measures$units",
      "measures$default_aggregation"
    ),
    current = c(
      "base", "base$coordinate_space", "layers", "layers$layer_id", "history",
      "measures$support_behavior", "measures$unit", "measures$aggregation"
    ),
    stringsAsFactors = FALSE
  )
  required_top <- c("domain", "values", "maps", "measures", "labels", "provenance")
  if (!is.list(x) || any(!required_top %in% names(x))) {
    return(.ngeo_migration_report(
      "reconstruction_required", type,
      sprintf("Missing legacy top-level fields: %s.",
              paste(setdiff(required_top, names(x)), collapse = ", ")),
      field_map
    ))
  }
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    .ngeo_abort("`strict` must be TRUE or FALSE.", "ngeo_error_argument")
  }
  supported_types <- c("points", "surface", "volume", "regions", "grayordinates")
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !type %in% supported_types) {
    return(.ngeo_migration_report(
      "reconstruction_required", type,
      "The legacy domain type is missing or unsupported.", field_map
    ))
  }
  value_is_supported <- is.null(x$values) || is.matrix(x$values) ||
    inherits(x$values, "Matrix") || is.data.frame(x$values) ||
    (is.atomic(x$values) && is.null(dim(x$values)))
  if (!value_is_supported) {
    return(.ngeo_migration_report(
      "reconstruction_required", type,
      "The legacy values use a delayed or file-backed representation; re-read the source file.",
      field_map
    ))
  }

  result <- tryCatch({
    coordinate_space <- .ngeo_legacy_space(x$domain$space)
    n_layer <- if (is.null(x$values)) 0L else ncol(.values(x$values, nrow(x$domain$elements)))
    metadata <- .ngeo_legacy_layers_measures(x$maps, x$measures, n_layer)
    base <- .ngeo_legacy_base(x$domain, coordinate_space)
    history <- list(
      source_provenance = x$provenance,
      operations = list(.ngeo_operation(
        "ngeo_migrate_5x",
        list(
          source_schema = "neurogeo 5.x",
          target_schema = "NGCS 6.0",
          legacy_type = type,
          field_map = field_map
        )
      ))
    )
    current_class <- c(
      points = "ngeo_point", surface = "ngeo_surface", volume = "ngeo_volume",
      regions = "ngeo_parcellation", grayordinates = "ngeo_grayordinate"
    )[[type]]
    migrated <- .new_ngeo(
      base = base,
      values = x$values,
      layers = metadata$layers,
      measures = metadata$measures,
      labels = x$labels,
      history = history,
      class = current_class
    )
    ngeo_validate(migrated, if (isTRUE(strict)) "strict" else "basic")
    migrated
  }, error = function(condition) {
    .ngeo_migration_report(
      "reconstruction_required", type, conditionMessage(condition), field_map
    )
  })
  result
}

#' @export
print.ngeo_migration_report <- function(x, ...) {
  cat("<ngeo_migration_report> ", x$status, "\n", sep = "")
  cat("  source: ", x$source_schema, "\n", sep = "")
  cat("  target: ", x$target_schema, "\n", sep = "")
  if (!is.na(x$base_type)) cat("  base type: ", x$base_type, "\n", sep = "")
  if (length(x$issues)) {
    cat("  action: ", paste(x$issues, collapse = " "), "\n", sep = "")
  }
  invisible(x)
}
