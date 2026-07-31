.ngeo_qc_check <- function(check, status, value = NA_real_, message) {
  data.frame(
    check = check,
    status = status,
    value = as.numeric(value),
    message = message,
    stringsAsFactors = FALSE
  )
}

.ngeo_qc_values <- function(x, max_value_cells) {
  if (is.null(x$values)) {
    return(list(
      check = .ngeo_qc_check(
        "values_scan",
        "not_applicable",
        message = "The object has no values block."
      ),
      maps = NULL
    ))
  }
  if (ncol(x$values) == 0L) {
    return(list(
      check = .ngeo_qc_check(
        "values_scan",
        "not_applicable",
        message = "The values block contains no maps."
      ),
      maps = NULL
    ))
  }
  cells <- as.double(nrow(x$values)) * as.double(ncol(x$values))
  if (cells > max_value_cells) {
    return(list(
      check = .ngeo_qc_check(
        "values_scan",
        "not_evaluated",
        cells,
        sprintf(
          "The values block has %s cells and exceeds `max_value_cells`.",
          format(cells, scientific = FALSE, big.mark = ",")
        )
      ),
      maps = NULL
    ))
  }
  values <- as.matrix(x$values)
  maps <- lapply(seq_len(ncol(values)), function(i) {
    current <- values[, i]
    missing <- is.na(current)
    nonfinite <- !missing & !is.finite(current)
    finite <- is.finite(current)
    data.frame(
      map_id = x$maps$map_id[[i]],
      name = x$maps$name[[i]],
      missing_fraction = mean(missing),
      nonfinite_fraction = mean(nonfinite),
      finite_count = sum(finite),
      constant = any(finite) &&
        length(unique(current[finite])) == 1L,
      stringsAsFactors = FALSE
    )
  })
  maps <- do.call(rbind, maps)
  checks <- rbind(
    .ngeo_qc_check(
      "values_scan",
      "pass",
      cells,
      "The aligned values block was scanned within the declared budget."
    ),
    .ngeo_qc_check(
      "missing_value_fraction",
      if (any(maps$missing_fraction > 0)) "warning" else "pass",
      if (nrow(maps)) max(maps$missing_fraction) else 0,
      "Maximum map-wise NA fraction."
    ),
    .ngeo_qc_check(
      "nonfinite_value_fraction",
      if (any(maps$nonfinite_fraction > 0)) "warning" else "pass",
      if (nrow(maps)) max(maps$nonfinite_fraction) else 0,
      "Maximum map-wise non-finite, non-NA fraction."
    ),
    .ngeo_qc_check(
      "constant_maps",
      if (any(maps$constant)) "warning" else "pass",
      sum(maps$constant),
      "Number of maps with one unique finite value."
    )
  )
  list(check = checks, maps = maps)
}

.ngeo_qc_topology <- function(x, tolerance) {
  capabilities <- ngeo_capabilities(x)
  if (!isTRUE(capabilities[["adjacency"]])) {
    return(list(
      check = .ngeo_qc_check(
        "topology",
        "not_applicable",
        message = "The domain has no declared adjacency capability."
      ),
      summary = NULL
    ))
  }
  maximum <- getOption("neurogeo.max_qc_elements", 1000000L)
  if (nrow(x$domain$elements) > maximum) {
    return(list(
      check = .ngeo_qc_check(
        "topology",
        "not_evaluated",
        nrow(x$domain$elements),
        "The domain exceeds `getOption(\"neurogeo.max_qc_elements\")`."
      ),
      summary = NULL
    ))
  }
  adjacency <- ngeo_adjacency(x)
  degree <- Matrix::rowSums(abs(adjacency))
  components <- ngeo_components(adjacency)
  n_component <- max(components, 0L)
  isolates <- sum(degree <= tolerance)
  disconnected_surface <- identical(x$domain$type, "surface") &&
    n_component > 1L
  status <- if (isolates > 0L || disconnected_surface) {
    "warning"
  } else {
    "pass"
  }
  summary <- data.frame(
    elements = nrow(adjacency),
    nonzero = length(adjacency@x),
    components = n_component,
    isolates = isolates,
    stringsAsFactors = FALSE
  )
  list(
    check = .ngeo_qc_check(
      "topology",
      status,
      n_component,
      sprintf(
        "Sparse adjacency has %d component(s) and %d isolate(s).",
        n_component,
        isolates
      )
    ),
    summary = summary
  )
}

.ngeo_qc_charts <- function(x, chart) {
  available <- names(x$domain$charts %||% list())
  if (!is.null(chart)) {
    .ngeo_assert_scalar_character(chart, "chart")
    if (!chart %in% available) {
      .ngeo_abort(
        sprintf("Chart `%s` is not available.", chart),
        "ngeo_error_alignment"
      )
    }
    available <- chart
  }
  if (!length(available)) {
    return(list(
      check = .ngeo_qc_check(
        "cortical_chart",
        "not_applicable",
        message = "The object has no cortical chart."
      ),
      summary = NULL
    ))
  }
  n_face <- nrow(x$domain$faces)
  summary <- do.call(rbind, lapply(available, function(name) {
    metadata <- x$domain$charts[[name]]
    source_face <- metadata$invariants$source_face_in_chart %||%
      seq_len(n_face)
    distortion <- metadata$distortion_summary %||% list()
    charted_faces <- distortion$charted_faces %||% length(source_face)
    folded_faces <- distortion$folded_faces %||% NA_integer_
    data.frame(
      chart = name,
      method = metadata$method %||% "external",
      kind = metadata$kind %||% "chart",
      charted_faces = charted_faces,
      folded_faces = folded_faces,
      charted_fraction = if (n_face) charted_faces / n_face else NA_real_,
      finite_area_ratio = distortion$finite_area_ratio %||% NA,
      maximum_angle_error =
        distortion$maximum_angle_error %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  known_folds <- !is.na(summary$folded_faces)
  status <- if (any(summary$folded_faces[known_folds] > 0L) ||
      any(!summary$finite_area_ratio[!is.na(summary$finite_area_ratio)])) {
    "warning"
  } else if (all(!known_folds)) {
    "info"
  } else {
    "pass"
  }
  list(
    check = .ngeo_qc_check(
      "cortical_chart",
      status,
      sum(summary$folded_faces, na.rm = TRUE),
      sprintf(
        "%d chart(s) inspected; %d folded face(s) recorded.",
        nrow(summary),
        sum(summary$folded_faces, na.rm = TRUE)
      )
    ),
    summary = summary
  )
}

#' Report bounded scientific quality-control checks
#'
#' `ngeo_qc()` complements [ngeo_validate()]. Validation enforces object
#' invariants; QC reports conditions that can be structurally valid but
#' require a scientific decision. The function never repairs or mutates its
#' inputs and does not materialize a values block beyond `max_value_cells`.
#'
#' @param x An `ngeo` object.
#' @param support_map Optional source-aligned `ngeo_support_map`.
#' @param chart Optional cortical chart name. By default all stored charts
#'   are inspected.
#' @param tolerance Positive numerical tolerance for sparse topology and
#'   support diagnostics.
#' @param max_value_cells Maximum values-block cells that QC may materialize
#'   for missingness and constant-map summaries.
#'
#' @return An `ngeo_qc` object containing a machine-readable `checks` table
#'   and bounded optional detail tables.
#' @examples
#' x <- ngeo_points(
#'   matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 3)),
#'   measures = ngeo_measure(spatial_semantics = "intensive")
#' )
#' qc <- ngeo_qc(x)
#' qc$checks
#' @export
ngeo_qc <- function(
    x,
    support_map = NULL,
    chart = NULL,
    tolerance = 1e-8,
    max_value_cells = 1000000L) {
  if (!inherits(x, "ngeo")) {
    .ngeo_abort("`x` must be an `ngeo` object.", "ngeo_error_argument")
  }
  ngeo_validate(x, "strict")
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0) {
    .ngeo_abort(
      "`tolerance` must be one positive finite number.",
      "ngeo_error_argument"
    )
  }
  max_value_cells <- .ngeo_as_integer(max_value_cells, "max_value_cells")
  if (length(max_value_cells) != 1L || max_value_cells < 0L) {
    .ngeo_abort(
      "`max_value_cells` must be one non-negative integer.",
      "ngeo_error_argument"
    )
  }

  checks <- list(
    .ngeo_qc_check(
      "object_alignment",
      "pass",
      nrow(x$domain$elements),
      sprintf(
        "%d element(s) and %d aligned map(s) passed strict validation.",
        nrow(x$domain$elements),
        nrow(x$maps)
      )
    )
  )
  space_known <- !identical(x$domain$space$space_id, "unknown")
  checks[[length(checks) + 1L]] <- .ngeo_qc_check(
    "space_known",
    if (space_known) "pass" else "warning",
    as.numeric(space_known),
    if (space_known) {
      sprintf("Coordinate space is `%s`.", x$domain$space$space_id)
    } else {
      "Coordinate space is unknown; no spatial equivalence may be assumed."
    }
  )
  unknown_semantics <- sum(x$measures$spatial_semantics == "unknown")
  checks[[length(checks) + 1L]] <- .ngeo_qc_check(
    "measurement_semantics",
    if (unknown_semantics) "warning" else "pass",
    unknown_semantics,
    sprintf("%d map(s) have unknown spatial measurement semantics.", unknown_semantics)
  )
  unknown_units <- sum(x$measures$units == "unknown")
  checks[[length(checks) + 1L]] <- .ngeo_qc_check(
    "measurement_units",
    if (unknown_units) "warning" else "pass",
    unknown_units,
    sprintf("%d map(s) have unknown units.", unknown_units)
  )

  values <- .ngeo_qc_values(x, max_value_cells)
  checks[[length(checks) + 1L]] <- values$check
  topology <- .ngeo_qc_topology(x, tolerance)
  checks[[length(checks) + 1L]] <- topology$check
  charts <- .ngeo_qc_charts(x, chart)
  checks[[length(checks) + 1L]] <- charts$check

  support <- NULL
  if (is.null(support_map)) {
    checks[[length(checks) + 1L]] <- .ngeo_qc_check(
      "support_map",
      "not_applicable",
      message = "No support map was supplied."
    )
  } else {
    ngeo_validate_support_map(support_map, tolerance)
    if (!identical(support_map$source_domain_hash, ngeo_domain_hash(x))) {
      .ngeo_abort(
        "`support_map` is not aligned with `x`.",
        "ngeo_error_alignment"
      )
    }
    support <- ngeo_support_diagnostics(
      support_map,
      tolerance = tolerance,
      condition = FALSE
    )
    checks[[length(checks) + 1L]] <- rbind(
      .ngeo_qc_check(
        "support_coverage",
        if (support$complete) "pass" else "warning",
        mean(support$source$covered),
        "Fraction of source elements covered by the support map."
      ),
      .ngeo_qc_check(
        "support_conservation",
        if (support$conservative) "pass" else "warning",
        as.numeric(support$conservative),
        "Mapped source memberships must sum to one within tolerance."
      )
    )
  }
  operations <- length(x$provenance$operations %||% list())
  checks[[length(checks) + 1L]] <- .ngeo_qc_check(
    "provenance",
    if (operations) "pass" else "warning",
    operations,
    sprintf("%d provenance operation(s) are recorded.", operations)
  )
  checks <- do.call(rbind, checks)
  rownames(checks) <- NULL
  result <- list(
    overall_status = if (any(checks$status == "warning")) {
      "warning"
    } else {
      "pass"
    },
    domain_type = x$domain$type,
    domain_hash = ngeo_domain_hash(x),
    checks = checks,
    map_summary = values$maps,
    topology_summary = topology$summary,
    chart_summary = charts$summary,
    support = support,
    tolerance = tolerance,
    max_value_cells = max_value_cells
  )
  class(result) <- "ngeo_qc"
  result
}

#' @export
print.ngeo_qc <- function(x, ...) {
  count <- table(factor(
    x$checks$status,
    levels = c(
      "pass", "info", "warning", "not_evaluated", "not_applicable"
    )
  ))
  cat(
    "<ngeo_qc>\n",
    "  domain: ", x$domain_type, "\n",
    "  overall: ", x$overall_status, "\n",
    "  checks: ", nrow(x$checks), "\n",
    "  warnings: ", unname(count[["warning"]]), "\n",
    "  not evaluated: ", unname(count[["not_evaluated"]]), "\n",
    sep = ""
  )
  invisible(x)
}

#' Plot a neurogeo quality-control summary
#'
#' @param x An `ngeo_qc` object.
#' @param ... Additional arguments passed to [graphics::barplot()].
#'
#' @return `x`, invisibly.
#' @export
plot.ngeo_qc <- function(x, ...) {
  if (!inherits(x, "ngeo_qc")) {
    .ngeo_abort("`x` must be an `ngeo_qc` object.", "ngeo_error_argument")
  }
  status <- c(
    "pass", "info", "warning", "not_evaluated", "not_applicable"
  )
  count <- table(factor(x$checks$status, levels = status))
  color <- c(
    pass = "#009E73",
    info = "#56B4E9",
    warning = "#D55E00",
    not_evaluated = "#E69F00",
    not_applicable = "#999999"
  )
  graphics::barplot(
    count,
    col = unname(color[status]),
    main = paste("neurogeo QC:", x$overall_status),
    ylab = "Checks",
    ...
  )
  invisible(x)
}
