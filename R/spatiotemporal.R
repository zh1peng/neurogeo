# Explicit temporal and spatiotemporal semantics.
.ngeo_time_units <- function() {
  c(
    "millisecond", "second", "minute", "hour",
    "day", "week", "year", "frame"
  )
}

.ngeo_temporal_semantics <- function() {
  c(
    "instantaneous", "interval_mean", "interval_total",
    "rate", "categorical"
  )
}

.ngeo_time_axis_payload <- function(x) {
  list(
    schema = "NGCS-time-axis-1",
    time = x$time,
    unit = x$unit,
    support = x$support,
    interval_start = x$interval_start,
    interval_end = x$interval_end,
    allow_overlap = x$allow_overlap,
    tolerance = x$tolerance
  )
}

#' Construct an explicit regular or irregular time axis
#'
#' @param time Explicit strictly increasing time coordinates. When `NULL`,
#' `start`, `step`, and `n` define a regular axis.
#' @param start,step,n Regular-axis parameters.
#' @param unit Canonical temporal unit.
#' @param interval_start,interval_end Optional aligned interval boundaries.
#' @param allow_overlap Whether interval supports may overlap.
#' @param tolerance Numeric regularity and boundary tolerance.
#' @return An `ngeo_time_axis`.
#' @export
ngeo_time_axis <- function(
    time = NULL,
    start = 0,
    step = 1,
    n = NULL,
    unit = "second",
    interval_start = NULL,
    interval_end = NULL,
    allow_overlap = FALSE,
    tolerance = 1e-10) {
  unit <- match.arg(unit, .ngeo_time_units())
  if (is.null(time)) {
    if (!is.numeric(start) || length(start) != 1L ||
        !is.finite(start) ||
        !is.numeric(step) || length(step) != 1L ||
        !is.finite(step) || step <= 0 ||
        !is.numeric(n) || length(n) != 1L ||
        is.na(n) || n < 1 || n != floor(n)) {
      .ngeo_abort(
        "A regular axis requires finite `start`, positive `step`, and positive integer `n`.",
        "ngeo_error_time_axis"
      )
    }
    time <- start + (seq_len(as.integer(n)) - 1L) * step
  }
  if (!is.numeric(time) || !length(time) || anyNA(time) ||
      any(!is.finite(time)) ||
      (length(time) > 1L && any(diff(time) <= 0))) {
    .ngeo_abort(
      "`time` must be finite, unique, and strictly increasing.",
      "ngeo_error_time_axis"
    )
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0 ||
      !is.logical(allow_overlap) || length(allow_overlap) != 1L ||
      is.na(allow_overlap)) {
    .ngeo_abort(
      "Time-axis tolerance or overlap policy is invalid.",
      "ngeo_error_time_axis"
    )
  }
  interval <- !is.null(interval_start) || !is.null(interval_end)
  if (interval) {
    if (!is.numeric(interval_start) ||
        !is.numeric(interval_end) ||
        length(interval_start) != length(time) ||
        length(interval_end) != length(time) ||
        anyNA(interval_start) || anyNA(interval_end) ||
        any(!is.finite(interval_start)) ||
        any(!is.finite(interval_end)) ||
        any(interval_end - interval_start <= tolerance) ||
        any(time < interval_start - tolerance) ||
        any(time > interval_end + tolerance)) {
      .ngeo_abort(
        "Interval boundaries must be finite, positive-width, aligned, and contain each time coordinate.",
        "ngeo_error_temporal_support"
      )
    }
    if (!allow_overlap && length(time) > 1L &&
        any(
          interval_start[-1L] <
            interval_end[-length(interval_end)] - tolerance
        )) {
      .ngeo_abort(
        "Temporal support intervals overlap without authorization.",
        "ngeo_error_temporal_support"
      )
    }
  } else {
    interval_start <- interval_end <- NULL
  }
  difference <- diff(time)
  regular <- length(difference) < 2L ||
    max(abs(difference - difference[[1L]])) <= tolerance
  duration <- if (interval) interval_end - interval_start else
    rep.int(0, length(time))
  result <- structure(
    list(
      time = as.numeric(time),
      unit = unit,
      support = if (interval) "interval" else "instant",
      interval_start = if (interval) as.numeric(interval_start) else NULL,
      interval_end = if (interval) as.numeric(interval_end) else NULL,
      duration = as.numeric(duration),
      regular = regular,
      step = if (regular && length(time) > 1L) {
        difference[[1L]]
      } else {
        NA_real_
      },
      allow_overlap = allow_overlap,
      tolerance = tolerance,
      axis_hash = NULL
    ),
    class = "ngeo_time_axis"
  )
  result$axis_hash <- digest::digest(
    .ngeo_time_axis_payload(result), algo = "sha256"
  )
  ngeo_validate_time_axis(result)
  result
}

#' Validate and identify an explicit time axis
#'
#' @param x An `ngeo_time_axis`.
#' @return The validator returns `x` invisibly; the hash function returns
#' its SHA-256 identity.
#' @name ngeo_time_axis_validation
NULL

#' @rdname ngeo_time_axis_validation
#' @templateVar example_call ngeo_time_axis_hash(time_axis)
#' @template stable-reproducibility
#' @export
ngeo_validate_time_axis <- function(x) {
  if (!inherits(x, "ngeo_time_axis")) {
    .ngeo_abort(
      "Time-axis coordinates, unit, support, or regularity are invalid.",
      "ngeo_error_time_axis"
    )
  }
  if (is.character(x$axis_hash) && length(x$axis_hash) == 1L &&
      !is.na(x$axis_hash) && nzchar(x$axis_hash) &&
      !identical(
        digest::digest(
          .ngeo_time_axis_payload(x), algo = "sha256"
        ),
        x$axis_hash
      )) {
    .ngeo_abort(
      "Time-axis identity changed after construction.",
      "ngeo_error_time_axis_mutation"
    )
  }
  tolerance_valid <- is.numeric(x$tolerance) &&
    length(x$tolerance) == 1L &&
    !is.na(x$tolerance) && is.finite(x$tolerance) &&
    x$tolerance > 0
  tolerance <- if (tolerance_valid) x$tolerance else 1e-10
  difference <- if (is.numeric(x$time)) diff(x$time) else numeric()
  expected_regular <- length(difference) < 2L ||
    max(abs(difference - difference[[1L]])) <= tolerance
  expected_step <- if (expected_regular && length(x$time) > 1L) {
    difference[[1L]]
  } else {
    NA_real_
  }
  if (!inherits(x, "ngeo_time_axis") ||
      !is.numeric(x$time) || !length(x$time) ||
      anyNA(x$time) || any(!is.finite(x$time)) ||
      (length(x$time) > 1L && any(diff(x$time) <= 0)) ||
      !is.character(x$unit) || length(x$unit) != 1L ||
      !x$unit %in% .ngeo_time_units() ||
      !is.character(x$support) || length(x$support) != 1L ||
      !x$support %in% c("instant", "interval") ||
      !is.logical(x$regular) || length(x$regular) != 1L ||
      is.na(x$regular) ||
      !identical(x$regular, expected_regular) ||
      !is.numeric(x$step) || length(x$step) != 1L ||
      !isTRUE(all.equal(
        x$step, expected_step, tolerance = tolerance,
        check.attributes = FALSE
      )) ||
      !is.logical(x$allow_overlap) ||
      length(x$allow_overlap) != 1L || is.na(x$allow_overlap) ||
      !tolerance_valid ||
      !is.numeric(x$duration) ||
      length(x$duration) != length(x$time) ||
      anyNA(x$duration) || any(!is.finite(x$duration)) ||
      any(x$duration < 0) ||
      !is.character(x$axis_hash) || length(x$axis_hash) != 1L ||
      is.na(x$axis_hash) || !nzchar(x$axis_hash)) {
    .ngeo_abort(
      "Time-axis coordinates, unit, support, or regularity are invalid.",
      "ngeo_error_time_axis"
    )
  }
  if (identical(x$support, "interval")) {
    if (!is.numeric(x$interval_start) ||
        !is.numeric(x$interval_end) ||
        length(x$interval_start) != length(x$time) ||
        length(x$interval_end) != length(x$time) ||
        anyNA(x$interval_start) || anyNA(x$interval_end) ||
        any(!is.finite(x$interval_start)) ||
        any(!is.finite(x$interval_end)) ||
        any(x$interval_end - x$interval_start <= x$tolerance) ||
        any(x$time < x$interval_start - x$tolerance) ||
        any(x$time > x$interval_end + x$tolerance) ||
        (!x$allow_overlap && length(x$time) > 1L &&
          any(
            x$interval_start[-1L] <
              x$interval_end[-length(x$interval_end)] - x$tolerance
          )) ||
        !isTRUE(all.equal(
          x$duration,
          x$interval_end - x$interval_start,
          tolerance = x$tolerance,
          check.attributes = FALSE
        ))) {
      .ngeo_abort(
        "Time-axis interval support is inconsistent.",
        "ngeo_error_temporal_support"
      )
    }
  } else if (!is.null(x$interval_start) ||
      !is.null(x$interval_end) || any(x$duration != 0)) {
    .ngeo_abort(
      "Instantaneous axes cannot carry interval support.",
      "ngeo_error_temporal_support"
    )
  }
  expected <- digest::digest(
    .ngeo_time_axis_payload(x), algo = "sha256"
  )
  if (!identical(expected, x$axis_hash)) {
    .ngeo_abort(
      "Time-axis identity changed after construction.",
      "ngeo_error_time_axis_mutation"
    )
  }
  invisible(x)
}

#' @rdname ngeo_time_axis_validation
#' @export
ngeo_time_axis_hash <- function(x) {
  ngeo_validate_time_axis(x)
  x$axis_hash
}

.ngeo_validate_temporal_semantics <- function(semantics, axis) {
  semantics <- as.character(semantics)
  if (!length(semantics) ||
      !length(semantics) %in% c(1L, length(axis$time)) ||
      anyNA(semantics) ||
      any(!semantics %in% .ngeo_temporal_semantics())) {
    .ngeo_abort(
      "Temporal semantics must align to every time map.",
      "ngeo_error_temporal_measure"
    )
  }
  if (length(semantics) == 1L) {
    semantics <- rep.int(semantics, length(axis$time))
  }
  interval_required <- semantics %in%
    c("interval_mean", "interval_total", "rate")
  if (any(interval_required) &&
      !identical(axis$support, "interval")) {
    .ngeo_abort(
      "Interval-mean, interval-total, and rate semantics require interval support.",
      "ngeo_error_temporal_support"
    )
  }
  if (any(semantics == "instantaneous") &&
      !identical(axis$support, "instant")) {
    .ngeo_abort(
      "Instantaneous semantics require an instantaneous time axis.",
      "ngeo_error_temporal_support"
    )
  }
  semantics
}

.ngeo_temporal_measure_template <- function(x, index = seq_len(nrow(x$layers))) {
  fields <- c("value_type", "support_behavior", "unit")
  measure <- .ngeo_measures_for_layers(x, index)[, fields, drop = FALSE]
  compatible <- vapply(
    measure,
    function(value) length(unique(value)) == 1L,
    logical(1)
  )
  if (!all(compatible)) {
    .ngeo_abort(
      paste(
        "Time-aligned layers require matching value type, spatial semantics,",
        "and measurement unit."
      ),
      "ngeo_error_temporal_measure"
    )
  }
  .ngeo_measures_for_layers(x, index[[1L]])
}

.ngeo_temporal_divide_unit <- function(unit, time_unit) {
  if (identical(unit, "unknown")) "unknown" else
    paste0(unit, "/", time_unit)
}

.ngeo_temporal_multiply_unit <- function(unit, time_unit) {
  suffix <- paste0("/", time_unit)
  if (identical(unit, "unknown")) {
    "unknown"
  } else if (endsWith(unit, suffix)) {
    substr(unit, 1L, nchar(unit) - nchar(suffix))
  } else {
    paste0(unit, "*", time_unit)
  }
}

#' Bind an explicit time axis and temporal semantics to map-aligned values
#'
#' @param x An `ngeo` object with one map per time coordinate.
#' @param axis An `ngeo_time_axis`.
#' @param temporal_semantics Scalar or map-aligned temporal semantics.
#' @return `x` with time metadata aligned to layers and measures.
#' @export
ngeo_set_time_axis <- function(
    x,
    axis,
    temporal_semantics = "instantaneous") {
  ngeo_validate(x, "strict")
  ngeo_validate_time_axis(axis)
  if (is.null(x$values) || nrow(x$layers) != length(axis$time)) {
    .ngeo_abort(
      "Time-axis length must equal the sole values block map count.",
      "ngeo_error_alignment"
    )
  }
  .ngeo_temporal_measure_template(x)
  temporal_semantics <- .ngeo_validate_temporal_semantics(
    temporal_semantics, axis
  )
  result <- x
  result$layers$time_index <- seq_along(axis$time)
  result$layers$time <- axis$time
  result$layers$time_unit <- rep.int(axis$unit, length(axis$time))
  result$layers$interval_start <- if (axis$support == "interval") {
    axis$interval_start
  } else {
    rep.int(NA_real_, length(axis$time))
  }
  result$layers$interval_end <- if (axis$support == "interval") {
    axis$interval_end
  } else {
    rep.int(NA_real_, length(axis$time))
  }
  result$layers$time_axis_hash <- rep.int(
    axis$axis_hash, length(axis$time)
  )
  layer_measures <- .ngeo_measures_for_layers(
    result,
    seq_len(nrow(result$layers))
  )
  original_measure_id <- layer_measures$measure_id
  semantic_key <- paste(original_measure_id, temporal_semantics, sep = "\u001f")
  keep <- !duplicated(semantic_key)
  new_measure_id <- original_measure_id
  split_ids <- original_measure_id %in%
    original_measure_id[duplicated(original_measure_id) &
      !duplicated(semantic_key)]
  new_measure_id[split_ids] <- paste0(
    original_measure_id[split_ids],
    "::",
    temporal_semantics[split_ids]
  )
  layer_measures$measure_id <- new_measure_id
  layer_measures$temporal_semantics <- temporal_semantics
  result$layers$measure_id <- new_measure_id
  result$measures <- layer_measures[keep, , drop = FALSE]
  rownames(result$measures) <- NULL
  result$history$time_axis <- axis
  result$history$operations <- c(
    result$history$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_set_time_axis",
      list(
        axis_hash = axis$axis_hash,
        regular = axis$regular,
        support = axis$support,
        temporal_semantics = temporal_semantics
      )
    ))
  )
  ngeo_validate(result, "strict")
  result
}

#' Retrieve and verify the map-aligned time axis
#'
#' @param x A time-aware `ngeo` object.
#' @return The verified `ngeo_time_axis`.
#' @export
ngeo_get_time_axis <- function(x) {
  ngeo_validate(x, "strict")
  axis <- x$history$time_axis %||% NULL
  if (!inherits(axis, "ngeo_time_axis")) {
    .ngeo_abort(
      "The object has no explicit time axis.",
      "ngeo_error_time_axis"
    )
  }
  ngeo_validate_time_axis(axis)
  required <- c(
    "time_index", "time", "time_unit", "interval_start",
    "interval_end", "time_axis_hash"
  )
  if (any(!required %in% names(x$layers)) ||
      !"temporal_semantics" %in% names(x$measures) ||
      nrow(x$layers) != length(axis$time) ||
      !identical(as.integer(x$layers$time_index), seq_along(axis$time)) ||
      !isTRUE(all.equal(
        as.numeric(x$layers$time), axis$time,
        tolerance = axis$tolerance,
        check.attributes = FALSE
      )) ||
      any(x$layers$time_unit != axis$unit) ||
      any(x$layers$time_axis_hash != axis$axis_hash)) {
    .ngeo_abort(
      "Map time metadata no longer matches the explicit axis.",
      "ngeo_error_time_axis_mutation"
    )
  }
  expected_start <- if (axis$support == "interval") {
    axis$interval_start
  } else {
    rep.int(NA_real_, length(axis$time))
  }
  expected_end <- if (axis$support == "interval") {
    axis$interval_end
  } else {
    rep.int(NA_real_, length(axis$time))
  }
  if (!identical(as.numeric(x$layers$interval_start), expected_start) ||
      !identical(as.numeric(x$layers$interval_end), expected_end)) {
    .ngeo_abort(
      "Map interval support no longer matches the explicit axis.",
      "ngeo_error_time_axis_mutation"
    )
  }
  .ngeo_validate_temporal_semantics(
    .ngeo_measures_for_layers(
      x,
      seq_len(nrow(x$layers))
    )$temporal_semantics,
    axis
  )
  axis
}

#' Slice a time-aware object without changing its spatial base
#'
#' @param x A time-aware `ngeo` object.
#' @param index Optional ordered one-based time indices.
#' @param range Optional inclusive numeric time range.
#' @return A time-aware subset with the same spatial base.
#' @export
ngeo_time_slice <- function(x, index = NULL, range = NULL) {
  axis <- ngeo_get_time_axis(x)
  if (!is.null(index) && !is.null(range)) {
    .ngeo_abort(
      "Choose `index` or `range`, not both.",
      "ngeo_error_argument"
    )
  }
  if (is.null(index)) {
    if (is.null(range)) {
      index <- seq_along(axis$time)
    } else {
      if (!is.numeric(range) || length(range) != 2L ||
          anyNA(range) || any(!is.finite(range)) ||
          range[[1L]] > range[[2L]]) {
        .ngeo_abort(
          "`range` must be two ordered finite times.",
          "ngeo_error_argument"
        )
      }
      index <- which(
        axis$time >= range[[1L]] & axis$time <= range[[2L]]
      )
    }
  } else {
    index <- .ngeo_as_integer(index, "index")
    if (any(index < 1L | index > length(axis$time)) ||
        anyDuplicated(index) ||
        (length(index) > 1L && any(diff(index) <= 0L))) {
      .ngeo_abort(
        "Time indices must be unique, increasing, and in range.",
        "ngeo_error_index"
      )
    }
  }
  if (!length(index)) {
    .ngeo_abort(
      "Time slicing cannot produce an empty values block.",
      "ngeo_error_index"
    )
  }
  selected_axis <- ngeo_time_axis(
    time = axis$time[index],
    unit = axis$unit,
    interval_start = if (axis$support == "interval") {
      axis$interval_start[index]
    } else {
      NULL
    },
    interval_end = if (axis$support == "interval") {
      axis$interval_end[index]
    } else {
      NULL
    },
    allow_overlap = axis$allow_overlap,
    tolerance = axis$tolerance
  )
  result <- x
  result$values <- x$values[, index, drop = FALSE]
  result$layers <- x$layers[index, , drop = FALSE]
  result$measures <- .ngeo_measures_for_layers(x, index, unique = TRUE)
  rownames(result$layers) <- NULL
  rownames(result$measures) <- NULL
  result$history$operations <- c(
    result$history$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_time_slice",
      list(
        source_axis_hash = axis$axis_hash,
        source_time_count = length(axis$time),
        selected_index = index
      )
    ))
  )
  ngeo_set_time_axis(
    result,
    selected_axis,
    temporal_semantics =
      .ngeo_measures_for_layers(x, index)$temporal_semantics
  )
}

.ngeo_temporal_weights_hash <- function(x) {
  digest::digest(
    list(
      axis_hash = x$axis_hash,
      matrix = x$matrix,
      raw_matrix = x$raw_matrix,
      method = x$method,
      normalization = x$normalization,
      directed = x$directed,
      parameters = x$parameters
    ),
    algo = "sha256"
  )
}

#' Construct sparse temporal spatial_weights
#'
#' @param axis An `ngeo_time_axis`.
#' @param method Consecutive adjacency, index lag, or time-distance band.
#' @param lag Positive index lag.
#' @param threshold Positive time-distance threshold.
#' @param directed Whether only later rows receive earlier neighbours.
#' @param style Binary, row-standardized, or unnormalized spatial_weights.
#' @return An `ngeo_temporal_weights`.
#' @export
ngeo_temporal_weights <- function(
    axis,
    method = c("adjacent", "lag", "distance"),
    lag = 1L,
    threshold = NULL,
    directed = FALSE,
    style = c("W", "B", "none")) {
  ngeo_validate_time_axis(axis)
  method <- match.arg(method)
  style <- match.arg(style)
  if (!is.logical(directed) || length(directed) != 1L ||
      is.na(directed)) {
    .ngeo_abort("`directed` must be TRUE or FALSE.",
                "ngeo_error_argument")
  }
  n <- length(axis$time)
  if (n < 2L) {
    .ngeo_abort(
      "Temporal spatial_weights require at least two time coordinates.",
      "ngeo_error_temporal_weights"
    )
  }
  if (method %in% c("adjacent", "lag")) {
    if (!is.numeric(lag) || length(lag) != 1L ||
        is.na(lag) || lag < 1 || lag >= n ||
        lag != floor(lag)) {
      .ngeo_abort(
        "`lag` must be an integer between 1 and n_time - 1.",
        "ngeo_error_argument"
      )
    }
    lag <- if (method == "adjacent") 1L else as.integer(lag)
    earlier <- seq_len(n - lag)
    later <- earlier + lag
  } else {
    if (!is.numeric(threshold) || length(threshold) != 1L ||
        is.na(threshold) || !is.finite(threshold) ||
        threshold <= 0) {
      .ngeo_abort(
        "Distance spatial_weights require a positive finite threshold.",
        "ngeo_error_argument"
      )
    }
    pairs <- utils::combn(seq_len(n), 2L)
    keep <- (
      axis$time[pairs[2L, ]] - axis$time[pairs[1L, ]]
    ) <= threshold + axis$tolerance
    earlier <- pairs[1L, keep]
    later <- pairs[2L, keep]
  }
  if (!length(earlier)) {
    .ngeo_abort(
      "No temporal neighbour pairs satisfy the request.",
      "ngeo_error_temporal_weights"
    )
  }
  i <- later
  j <- earlier
  if (!directed) {
    i <- c(i, earlier)
    j <- c(j, later)
  }
  raw <- Matrix::sparseMatrix(
    i = i, j = j, x = 1,
    dims = c(n, n), giveCsparse = TRUE
  )
  diag(raw) <- 0
  raw <- .ngeo_as_dgCMatrix(raw)
  matrix <- switch(
    style,
    W = .ngeo_row_standardize(raw),
    B = .ngeo_binary(raw),
    none = raw
  )
  result <- structure(
    list(
      matrix = matrix,
      raw_matrix = raw,
      axis_hash = axis$axis_hash,
      n_time = n,
      method = method,
      normalization = style,
      directed = directed,
      parameters = list(lag = lag, threshold = threshold),
      weights_hash = NULL
    ),
    class = "ngeo_temporal_weights"
  )
  result$weights_hash <- .ngeo_temporal_weights_hash(result)
  ngeo_validate_temporal_weights(result)
  result
}

#' Validate temporal spatial_weights
#'
#' @param x An `ngeo_temporal_weights`.
#' @return `x`, invisibly.
#' @export
ngeo_validate_temporal_weights <- function(x) {
  if (!inherits(x, "ngeo_temporal_weights") ||
      !inherits(x$matrix, "dgCMatrix") ||
      !inherits(x$raw_matrix, "dgCMatrix") ||
      !is.numeric(x$n_time) || length(x$n_time) != 1L ||
      is.na(x$n_time) || x$n_time < 2 ||
      x$n_time != floor(x$n_time) ||
      !identical(dim(x$matrix), c(x$n_time, x$n_time)) ||
      !identical(dim(x$raw_matrix), c(x$n_time, x$n_time)) ||
      any(Matrix::diag(x$matrix) != 0) ||
      any(Matrix::diag(x$raw_matrix) != 0) ||
      any(!is.finite(x$matrix@x)) ||
      any(!is.finite(x$raw_matrix@x)) ||
      any(x$matrix@x < 0) ||
      any(x$raw_matrix@x < 0) ||
      !is.character(x$method) || length(x$method) != 1L ||
      !x$method %in% c("adjacent", "lag", "distance") ||
      !is.character(x$normalization) ||
      length(x$normalization) != 1L ||
      !x$normalization %in% c("W", "B", "none") ||
      !is.logical(x$directed) || length(x$directed) != 1L ||
      is.na(x$directed) ||
      !is.character(x$axis_hash) ||
      length(x$axis_hash) != 1L ||
      !identical(
        x$weights_hash,
        .ngeo_temporal_weights_hash(x)
      )) {
    .ngeo_abort(
      "Temporal spatial_weights identity or sparse matrix is invalid.",
      "ngeo_error_temporal_weights"
    )
  }
  invisible(x)
}

#' Return temporal neighbours
#'
#' @inheritParams ngeo_temporal_weights
#' @return An ordered list of one-based neighbour indices per time row.
#' @export
ngeo_temporal_neighbors <- function(
    axis,
    method = c("adjacent", "lag", "distance"),
    lag = 1L,
    threshold = NULL,
    directed = FALSE) {
  spatial_weights <- ngeo_temporal_weights(
    axis, method, lag, threshold, directed, style = "B"
  )
  lapply(seq_len(spatial_weights$n_time), function(i) {
    which(spatial_weights$matrix[i, ] != 0)
  })
}

.ngeo_st_weights_hash <- function(x) {
  digest::digest(
    list(
      base_hash = x$base_hash,
      axis_hash = x$axis_hash,
      spatial = digest::digest(x$spatial$matrix, algo = "sha256"),
      temporal = x$temporal$weights_hash,
      combination = x$combination,
      spatial_scale = x$spatial_scale
    ),
    algo = "sha256"
  )
}

#' Construct separable matrix-free spatiotemporal spatial_weights
#'
#' @param spatial Matching `ngeo_spatial_weights`.
#' @param temporal Matching `ngeo_temporal_weights`.
#' @param combination Weighted Kronecker sum or Kronecker product.
#' @param spatial_scale Spatial share for the sum.
#' @return An `ngeo_spatiotemporal_weights` without a coordinate_space-by-time matrix.
#' @export
ngeo_spatiotemporal_weights <- function(
    spatial,
    temporal,
    combination = c("sum", "product"),
    spatial_scale = 0.5) {
  if (!inherits(spatial, "ngeo_spatial_weights") ||
      !inherits(spatial$matrix, "dgCMatrix")) {
    .ngeo_abort(
      "`spatial` must be sparse `ngeo_spatial_weights`.",
      "ngeo_error_spatiotemporal_weights"
    )
  }
  ngeo_validate_temporal_weights(temporal)
  combination <- match.arg(combination)
  if (!is.numeric(spatial_scale) ||
      length(spatial_scale) != 1L ||
      is.na(spatial_scale) || !is.finite(spatial_scale) ||
      spatial_scale < 0 || spatial_scale > 1) {
    .ngeo_abort(
      "`spatial_scale` must lie in [0, 1].",
      "ngeo_error_argument"
    )
  }
  if (combination == "product") spatial_scale <- NA_real_
  result <- structure(
    list(
      spatial = spatial,
      temporal = temporal,
      base_hash = spatial$base_hash,
      axis_hash = temporal$axis_hash,
      n_space = nrow(spatial$matrix),
      n_time = temporal$n_time,
      combination = combination,
      spatial_scale = spatial_scale,
      matrix_materialized = FALSE,
      weights_hash = NULL
    ),
    class = "ngeo_spatiotemporal_weights"
  )
  result$weights_hash <- .ngeo_st_weights_hash(result)
  ngeo_validate_spatiotemporal_weights(result)
  result
}

#' Validate separable spatiotemporal spatial_weights
#'
#' @param x An `ngeo_spatiotemporal_weights`.
#' @return `x`, invisibly.
#' @export
ngeo_validate_spatiotemporal_weights <- function(x) {
  if (!inherits(x, "ngeo_spatiotemporal_weights") ||
      !inherits(x$spatial, "ngeo_spatial_weights") ||
      !inherits(x$spatial$matrix, "dgCMatrix") ||
      any(!is.finite(x$spatial$matrix@x)) ||
      any(x$spatial$matrix@x < 0) ||
      any(Matrix::diag(x$spatial$matrix) != 0) ||
      !is.numeric(x$n_space) || length(x$n_space) != 1L ||
      is.na(x$n_space) || x$n_space < 1 ||
      !identical(nrow(x$spatial$matrix), x$n_space) ||
      !identical(ncol(x$spatial$matrix), x$n_space) ||
      !identical(x$spatial$base_hash, x$base_hash) ||
      !inherits(x$temporal, "ngeo_temporal_weights") ||
      !identical(x$temporal$n_time, x$n_time) ||
      !identical(x$temporal$axis_hash, x$axis_hash) ||
      !x$combination %in% c("sum", "product") ||
      (x$combination == "sum" &&
        (!is.numeric(x$spatial_scale) ||
          length(x$spatial_scale) != 1L ||
          is.na(x$spatial_scale) ||
          x$spatial_scale < 0 || x$spatial_scale > 1)) ||
      (x$combination == "product" &&
        !(length(x$spatial_scale) == 1L &&
          is.numeric(x$spatial_scale) &&
          is.na(x$spatial_scale))) ||
      !identical(x$matrix_materialized, FALSE) ||
      !identical(x$weights_hash, .ngeo_st_weights_hash(x))) {
    .ngeo_abort(
      "Spatiotemporal weight components or identity are invalid.",
      "ngeo_error_spatiotemporal_weights"
    )
  }
  ngeo_validate_temporal_weights(x$temporal)
  invisible(x)
}

.ngeo_st_nonzero_estimate <- function(spatial_weights) {
  spatial_nnz <- length(spatial_weights$spatial$matrix@x)
  temporal_nnz <- length(spatial_weights$temporal$matrix@x)
  if (spatial_weights$combination == "sum") {
    spatial_weights$n_time * spatial_nnz +
      spatial_weights$n_space * temporal_nnz
  } else {
    spatial_nnz * temporal_nnz
  }
}

#' Explicitly materialize a small spatiotemporal reference matrix
#'
#' @param spatial_weights Separable spatiotemporal spatial_weights.
#' @param max_observations Maximum permitted coordinate_space-time observations.
#' @param budget Resource budget for sparse nonzeros and bytes.
#' @return A sparse Kronecker reference matrix.
#' @export
ngeo_materialize_spatiotemporal_weights <- function(
    spatial_weights,
    max_observations = 10000L,
    budget = ngeo_resource_budget()) {
  ngeo_validate_spatiotemporal_weights(spatial_weights)
  n <- as.double(spatial_weights$n_space) * as.double(spatial_weights$n_time)
  if (!is.numeric(max_observations) ||
      length(max_observations) != 1L ||
      is.na(max_observations) || max_observations < 1 ||
      n > max_observations) {
    .ngeo_abort(
      "Spatiotemporal materialization exceeds the explicit observation limit.",
      "ngeo_error_resource"
    )
  }
  nonzero <- .ngeo_st_nonzero_estimate(spatial_weights)
  .ngeo_budget_assert(budget, "materialized_elements", nonzero)
  .ngeo_budget_assert(budget, "memory_bytes", 24 * nonzero)
  spatial <- spatial_weights$spatial$matrix
  temporal <- spatial_weights$temporal$matrix
  result <- if (spatial_weights$combination == "sum") {
    spatial_weights$spatial_scale *
      Matrix::kronecker(Matrix::Diagonal(spatial_weights$n_time), spatial) +
      (1 - spatial_weights$spatial_scale) *
        Matrix::kronecker(
          temporal, Matrix::Diagonal(spatial_weights$n_space)
        )
  } else {
    Matrix::kronecker(temporal, spatial)
  }
  .ngeo_as_dgCMatrix(result)
}

.ngeo_st_values <- function(
    x,
    spatial_weights,
    budget = ngeo_resource_budget()) {
  axis <- ngeo_get_time_axis(x)
  ngeo_validate_spatiotemporal_weights(spatial_weights)
  if (!identical(base_hash(x), spatial_weights$base_hash) ||
      !identical(axis$axis_hash, spatial_weights$axis_hash) ||
      nrow(x$base$elements) != spatial_weights$n_space ||
      nrow(x$layers) != spatial_weights$n_time) {
    .ngeo_abort(
      "Spatiotemporal spatial_weights do not align with the base and time axis.",
      "ngeo_error_alignment"
    )
  }
  n_value <- as.double(spatial_weights$n_space) * spatial_weights$n_time
  .ngeo_budget_assert(
    budget, "materialized_elements", n_value
  )
  .ngeo_budget_assert(
    budget, "memory_bytes", 8 * n_value
  )
  values <- as.matrix(x$values)
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Spatiotemporal methods require finite values.",
      "ngeo_error_missing"
    )
  }
  if (any(
    x$measures$temporal_semantics == "categorical" |
      x$measures$value_type %in% "label"
  )) {
    .ngeo_abort(
      "Categorical time layers do not support numeric spatiotemporal statistics.",
      "ngeo_error_temporal_measure"
    )
  }
  list(values = values, axis = axis)
}

#' Compute a matrix-free separable spatiotemporal lag
#'
#' @param x Time-aware `ngeo` data.
#' @param spatial_weights Matching separable spatiotemporal spatial_weights.
#' @param center Whether to subtract the global coordinate_space-time mean.
#' @param budget Resource limits for materializing the aligned values block.
#' @return An element-by-time lag matrix.
#' @export
ngeo_spatiotemporal_lag <- function(
    x,
    spatial_weights,
    center = FALSE,
    budget = ngeo_resource_budget()) {
  if (!is.logical(center) || length(center) != 1L || is.na(center)) {
    .ngeo_abort("`center` must be TRUE or FALSE.",
                "ngeo_error_argument")
  }
  input <- .ngeo_st_values(x, spatial_weights, budget)
  values <- input$values
  if (center) values <- values - mean(values)
  if (spatial_weights$combination == "sum") {
    spatial_weights$spatial_scale *
      as.matrix(spatial_weights$spatial$matrix %*% values) +
      (1 - spatial_weights$spatial_scale) *
        as.matrix(values %*% Matrix::t(spatial_weights$temporal$matrix))
  } else {
    as.matrix(
      spatial_weights$spatial$matrix %*% values %*%
        Matrix::t(spatial_weights$temporal$matrix)
    )
  }
}

.ngeo_st_s0 <- function(spatial_weights) {
  spatial_sum <- sum(spatial_weights$spatial$matrix)
  temporal_sum <- sum(spatial_weights$temporal$matrix)
  if (spatial_weights$combination == "sum") {
    spatial_weights$spatial_scale * spatial_weights$n_time * spatial_sum +
      (1 - spatial_weights$spatial_scale) *
        spatial_weights$n_space * temporal_sum
  } else {
    spatial_sum * temporal_sum
  }
}

.ngeo_st_moran_value <- function(values, spatial_weights) {
  centered <- values - mean(values)
  lag <- if (spatial_weights$combination == "sum") {
    spatial_weights$spatial_scale *
      as.matrix(spatial_weights$spatial$matrix %*% centered) +
      (1 - spatial_weights$spatial_scale) *
        as.matrix(centered %*% Matrix::t(spatial_weights$temporal$matrix))
  } else {
    as.matrix(
      spatial_weights$spatial$matrix %*% centered %*%
        Matrix::t(spatial_weights$temporal$matrix)
    )
  }
  denominator <- sum(centered^2)
  s0 <- .ngeo_st_s0(spatial_weights)
  if (denominator <= 0 || s0 <= 0) return(NA_real_)
  length(centered) / s0 *
    sum(centered * lag) / denominator
}

#' Per-element temporal Moran statistics
#'
#' @param x Time-aware `ngeo` data.
#' @param spatial_weights Matching temporal spatial_weights.
#' @param elements Optional spatial element selection.
#' @param budget Resource limits for materializing selected values.
#' @return An `ngeo_temporal_moran` data frame.
#' @examples
#' \dontrun{
#' ngeo_temporal_moran(time_data, temporal_weights)
#' }
#' @template stable-statistical-method
#' @export
ngeo_temporal_moran <- function(
    x,
    spatial_weights,
    elements = NULL,
    budget = ngeo_resource_budget()) {
  axis <- ngeo_get_time_axis(x)
  ngeo_validate_temporal_weights(spatial_weights)
  if (!identical(axis$axis_hash, spatial_weights$axis_hash)) {
    .ngeo_abort(
      "Temporal spatial_weights do not match the object time axis.",
      "ngeo_error_alignment"
    )
  }
  index <- .ngeo_element_selection(x, elements)
  n_value <- as.double(length(index)) * spatial_weights$n_time
  .ngeo_budget_assert(
    budget, "materialized_elements", n_value
  )
  .ngeo_budget_assert(budget, "memory_bytes", 8 * n_value)
  values <- as.matrix(x$values[index, , drop = FALSE])
  if (any(!is.finite(values)) ||
      any(x$measures$temporal_semantics == "categorical")) {
    .ngeo_abort(
      "Temporal Moran requires finite non-categorical values.",
      "ngeo_error_temporal_measure"
    )
  }
  estimate <- apply(values, 1L, function(value) {
    if (stats::var(value) == 0) NA_real_ else
      .ngeo_moran_value(value, spatial_weights$matrix)
  })
  result <- data.frame(
    element_id = x$base$elements$element_id[index],
    moran_i = as.numeric(estimate),
    expectation = rep.int(-1 / (spatial_weights$n_time - 1), length(index)),
    n_time = rep.int(spatial_weights$n_time, length(index)),
    stringsAsFactors = FALSE
  )
  attr(result, "axis_hash") <- axis$axis_hash
  attr(result, "weights_hash") <- spatial_weights$weights_hash
  class(result) <- c("ngeo_temporal_moran", "data.frame")
  result
}

#' Matrix-free global spatiotemporal Moran's I
#'
#' @param x Time-aware `ngeo` data.
#' @param spatial_weights Matching separable spatiotemporal spatial_weights.
#' @param permutations Number of deterministic Monte Carlo permutations.
#' @param null Permute whole spatial profiles or all observations.
#' @param seed Optional seed.
#' @param alternative Permutation-test alternative.
#' @param budget Resource limits for materializing the aligned values block.
#' @return An `ngeo_spatiotemporal_moran`.
#' @examples
#' \dontrun{
#' ngeo_spatiotemporal_moran(
#'   time_data, spatial_weights, permutations = 199, seed = 2026
#' )
#' }
#' @template stable-statistical-method
#' @export
ngeo_spatiotemporal_moran <- function(
    x,
    spatial_weights,
    permutations = 0L,
    null = c("spatial_profiles", "observations"),
    seed = NULL,
    alternative = c("two.sided", "greater", "less"),
    budget = ngeo_resource_budget()) {
  input <- .ngeo_st_values(x, spatial_weights, budget)
  null <- match.arg(null)
  alternative <- match.arg(alternative)
  permutations <- .ngeo_as_integer(permutations, "permutations")
  if (length(permutations) != 1L || permutations < 0L) {
    .ngeo_abort(
      "`permutations` must be one non-negative integer.",
      "ngeo_error_argument"
    )
  }
  observed <- .ngeo_st_moran_value(input$values, spatial_weights)
  expectation <- -1 / (length(input$values) - 1)
  simulated <- .ngeo_with_seed(seed, function() {
    if (!permutations) return(numeric())
    vapply(seq_len(permutations), function(...) {
      permuted <- if (null == "spatial_profiles") {
        input$values[
          sample.int(nrow(input$values)), , drop = FALSE
        ]
      } else {
        matrix(
          sample(as.numeric(input$values)),
          nrow = nrow(input$values),
          ncol = ncol(input$values)
        )
      }
      .ngeo_st_moran_value(permuted, spatial_weights)
    }, numeric(1))
  })
  result <- list(
    statistic = "Spatiotemporal Moran's I",
    estimate = observed,
    expectation = expectation,
    p.value = .ngeo_permutation_p(
      observed, simulated, expectation, alternative
    ),
    permutations = permutations,
    simulated = simulated,
    null = null,
    alternative = alternative,
    n_space = spatial_weights$n_space,
    n_time = spatial_weights$n_time,
    n_observation = spatial_weights$n_space * spatial_weights$n_time,
    base_hash = spatial_weights$base_hash,
    axis_hash = spatial_weights$axis_hash,
    weights_hash = spatial_weights$weights_hash,
    matrix_materialized = FALSE,
    seed = seed
  )
  class(result) <- "ngeo_spatiotemporal_moran"
  result
}

.ngeo_temporal_breaks <- function(distance, breaks, name) {
  boundaries <- if (length(breaks) == 1L) {
    if (!is.numeric(breaks) || is.na(breaks) ||
        breaks < 1 || breaks != floor(breaks)) {
      .ngeo_abort(
        sprintf("`%s` must be a positive integer or boundaries.", name),
        "ngeo_error_argument"
      )
    }
    upper <- max(distance)
    if (upper <= 0) {
      .ngeo_abort(
        "Variogram distances must contain a positive value.",
        "ngeo_error_statistic"
      )
    }
    seq(0, upper, length.out = as.integer(breaks) + 1L)
  } else {
    boundaries <- as.numeric(breaks)
    if (length(boundaries) < 2L ||
        anyNA(boundaries) || any(!is.finite(boundaries)) ||
        is.unsorted(boundaries, strictly = TRUE)) {
      .ngeo_abort(
        sprintf("`%s` boundaries must be strictly increasing.", name),
        "ngeo_error_argument"
      )
    }
    boundaries
  }
  if (boundaries[[1L]] > min(distance) ||
      boundaries[[length(boundaries)]] < max(distance)) {
    .ngeo_abort(
      sprintf("`%s` boundaries must cover every distance.", name),
      "ngeo_error_argument"
    )
  }
  boundaries
}

#' Empirical temporal semivariogram
#'
#' @param x Time-aware `ngeo` data.
#' @param elements Optional spatial element selection.
#' @param breaks Temporal lag bins or boundaries.
#' @param max_pairs Hard pair budget.
#' @return An `ngeo_temporal_variogram`.
#' @examples
#' \dontrun{
#' ngeo_temporal_variogram(time_data, breaks = 5, max_pairs = 10000)
#' }
#' @template stable-statistical-method
#' @export
ngeo_temporal_variogram <- function(
    x,
    elements = NULL,
    breaks = 10L,
    max_pairs = getOption("neurogeo.max_temporal_pairs", 1e6)) {
  axis <- ngeo_get_time_axis(x)
  index <- .ngeo_element_selection(x, elements)
  if (length(axis$time) < 2L) {
    .ngeo_abort(
      "Temporal variograms require at least two time layers.",
      "ngeo_error_statistic"
    )
  }
  if (any(x$measures$temporal_semantics == "categorical")) {
    .ngeo_abort(
      "Categorical time layers have no numeric semivariogram.",
      "ngeo_error_temporal_measure"
    )
  }
  pair_per_element <- length(axis$time) *
    (length(axis$time) - 1) / 2
  pair_count <- pair_per_element * length(index)
  if (!is.numeric(max_pairs) || length(max_pairs) != 1L ||
      is.na(max_pairs) || max_pairs < 1 ||
      pair_count > max_pairs) {
    .ngeo_abort(
      "Temporal variogram exceeds the declared pair budget.",
      "ngeo_error_resource"
    )
  }
  pairs <- utils::combn(seq_along(axis$time), 2L)
  distance_one <- axis$time[pairs[2L, ]] -
    axis$time[pairs[1L, ]]
  distance <- rep(distance_one, times = length(index))
  values <- as.matrix(x$values[index, , drop = FALSE])
  semivariance <- unlist(lapply(seq_len(nrow(values)), function(i) {
    0.5 * (
      values[i, pairs[2L, ]] - values[i, pairs[1L, ]]
    )^2
  }), use.names = FALSE)
  if (any(!is.finite(semivariance))) {
    .ngeo_abort(
      "Temporal variogram requires finite values.",
      "ngeo_error_missing"
    )
  }
  boundaries <- .ngeo_temporal_breaks(
    distance, breaks, "breaks"
  )
  bin <- cut(
    distance, boundaries, include.lowest = TRUE, right = TRUE
  )
  result <- do.call(rbind, lapply(levels(bin), function(level) {
    selected <- which(bin == level)
    if (!length(selected)) return(NULL)
    data.frame(
      bin = level,
      temporal_distance = mean(distance[selected]),
      semivariance = mean(semivariance[selected]),
      n_pairs = length(selected),
      stringsAsFactors = FALSE
    )
  }))
  rownames(result) <- NULL
  attr(result, "axis_hash") <- axis$axis_hash
  attr(result, "pair_count") <- pair_count
  class(result) <- c("ngeo_temporal_variogram", "data.frame")
  result
}

#' Bounded empirical coordinate_space-time semivariogram
#'
#' @param x Time-aware `ngeo` data.
#' @param spatial_distance Optional explicit square spatial distance matrix.
#' @param distance_method Metric used only when a small distance matrix is constructed.
#' @param spatial_breaks,temporal_breaks Bin counts or boundaries.
#' @param max_pairs Hard coordinate_space-time observation-pair budget.
#' @return An `ngeo_spatiotemporal_variogram`.
#' @examples
#' \dontrun{
#' ngeo_spatiotemporal_variogram(
#'   time_data, spatial_breaks = 5, temporal_breaks = 5,
#'   max_pairs = 10000
#' )
#' }
#' @template stable-statistical-method
#' @export
ngeo_spatiotemporal_variogram <- function(
    x,
    spatial_distance = NULL,
    distance_method = NULL,
    spatial_breaks = 5L,
    temporal_breaks = 5L,
    max_pairs = getOption("neurogeo.max_spatiotemporal_pairs", 1e6)) {
  axis <- ngeo_get_time_axis(x)
  if (any(x$measures$temporal_semantics == "categorical")) {
    .ngeo_abort(
      "Space-time variograms require finite non-categorical values.",
      "ngeo_error_temporal_measure"
    )
  }
  n_space <- nrow(x$base$elements)
  n_time <- nrow(x$layers)
  n_observation <- as.double(n_space) * as.double(n_time)
  pair_count <- n_observation * (n_observation - 1) / 2
  if (!is.numeric(max_pairs) || length(max_pairs) != 1L ||
      is.na(max_pairs) || max_pairs < 1 ||
      pair_count > max_pairs) {
    .ngeo_abort(
      "Space-time variogram exceeds the declared pair budget.",
      "ngeo_error_resource"
    )
  }
  if (n_observation < 2) {
    .ngeo_abort(
      "Space-time variograms require at least two observations.",
      "ngeo_error_statistic"
    )
  }
  values <- as.matrix(x$values)
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Space-time variograms require finite non-categorical values.",
      "ngeo_error_temporal_measure"
    )
  }
  if (is.null(spatial_distance)) {
    if (as.double(n_space)^2 > max_pairs) {
      .ngeo_abort(
        "Supply an explicit bounded spatial distance matrix.",
        "ngeo_error_dense_distance"
      )
    }
    spatial_distance <- as.matrix(ngeo_distance(
      x,
      from = seq_len(n_space),
      to = seq_len(n_space),
      distance_method = distance_method
    ))
  }
  if (!is.matrix(spatial_distance) ||
      !identical(dim(spatial_distance), c(n_space, n_space)) ||
      anyNA(spatial_distance) ||
      any(!is.finite(spatial_distance)) ||
      any(spatial_distance < 0)) {
    .ngeo_abort(
      "`spatial_distance` must be a finite non-negative square matrix.",
      "ngeo_error_distance"
    )
  }
  observation_pairs <- utils::combn(
    seq_len(as.integer(n_observation)), 2L
  )
  element_a <- (observation_pairs[1L, ] - 1L) %% n_space + 1L
  element_b <- (observation_pairs[2L, ] - 1L) %% n_space + 1L
  time_a <- (observation_pairs[1L, ] - 1L) %/% n_space + 1L
  time_b <- (observation_pairs[2L, ] - 1L) %/% n_space + 1L
  spatial <- spatial_distance[cbind(element_a, element_b)]
  temporal <- abs(axis$time[time_a] - axis$time[time_b])
  flat <- as.numeric(values)
  semivariance <- 0.5 * (
    flat[observation_pairs[1L, ]] -
      flat[observation_pairs[2L, ]]
  )^2
  spatial_boundaries <- .ngeo_temporal_breaks(
    spatial, spatial_breaks, "spatial_breaks"
  )
  temporal_boundaries <- .ngeo_temporal_breaks(
    temporal, temporal_breaks, "temporal_breaks"
  )
  spatial_bin <- cut(
    spatial, spatial_boundaries,
    include.lowest = TRUE, right = TRUE
  )
  temporal_bin <- cut(
    temporal, temporal_boundaries,
    include.lowest = TRUE, right = TRUE
  )
  key <- interaction(spatial_bin, temporal_bin, drop = TRUE)
  result <- do.call(rbind, lapply(levels(key), function(level) {
    selected <- which(key == level)
    if (!length(selected)) return(NULL)
    data.frame(
      spatial_bin = as.character(spatial_bin[selected[[1L]]]),
      temporal_bin = as.character(temporal_bin[selected[[1L]]]),
      spatial_distance = mean(spatial[selected]),
      temporal_distance = mean(temporal[selected]),
      semivariance = mean(semivariance[selected]),
      n_pairs = length(selected),
      stringsAsFactors = FALSE
    )
  }))
  rownames(result) <- NULL
  attr(result, "base_hash") <- base_hash(x)
  attr(result, "axis_hash") <- axis$axis_hash
  attr(result, "pair_count") <- pair_count
  class(result) <- c(
    "ngeo_spatiotemporal_variogram", "data.frame"
  )
  result
}

.ngeo_temporal_derived <- function(
    x, values, names, operation, parameters, measures = NULL) {
  layers <- data.frame(name = names, stringsAsFactors = FALSE)
  if (is.null(measures)) {
    template <- .ngeo_temporal_measure_template(x)
    measures <- template[rep.int(1L, length(names)), , drop = FALSE]
  }
  if (!is.data.frame(measures) || nrow(measures) != length(names)) {
    .ngeo_abort(
      "Derived temporal measures must align with output layers.",
      "ngeo_error_temporal_measure"
    )
  }
  measures$measure_id <- NULL
  measures$temporal_semantics <- "derived"
  rownames(measures) <- NULL
  .new_ngeo(
    base = x$base,
    values = values,
    layers = layers,
    measures = measures,
    labels = .ngeo_subset_labels(
      x$base$labels %||% list(),
      seq_len(nrow(x$base$elements)),
      nrow(x$base$elements),
      character()
    ),
    history = list(
      spec_version = "6.0",
      source_dataset = list(
        base_hash = base_hash(x),
        axis_hash = ngeo_get_time_axis(x)$axis_hash
      ),
      operations = list(.ngeo_operation(operation, parameters))
    ),
    class = class(x)[[1L]]
  )
}

#' Compute longitudinal change between two time layers
#'
#' @param x Time-aware `ngeo` data.
#' @param from,to One time-map selector each.
#' @param scale Difference, rate per time unit, or percent change.
#' @param name Output map name.
#' @param budget Resource limits for two input layers and one output layer.
#' @return One target-base `ngeo` map.
#' @export
ngeo_longitudinal_change <- function(
    x,
    from = 1L,
    to = nrow(x$layers),
    scale = c("difference", "rate", "percent"),
    name = "change",
    budget = ngeo_resource_budget()) {
  axis <- ngeo_get_time_axis(x)
  scale <- match.arg(scale)
  from <- .ngeo_layer_selection(x, from)
  to <- .ngeo_layer_selection(x, to)
  if (length(from) != 1L || length(to) != 1L ||
      from == to || axis$time[[to]] <= axis$time[[from]]) {
    .ngeo_abort(
      "`from` and `to` must select two increasing time layers.",
      "ngeo_error_time_axis"
    )
  }
  semantics <- .ngeo_measures_for_layers(
    x,
    c(from, to)
  )$temporal_semantics
  if (any(semantics == "categorical") ||
      length(unique(semantics)) != 1L) {
    .ngeo_abort(
      "Longitudinal change requires matching numeric temporal semantics.",
      "ngeo_error_temporal_measure"
    )
  }
  n_value <- 3 * as.double(nrow(x$base$elements))
  .ngeo_budget_assert(
    budget, "materialized_elements", n_value
  )
  .ngeo_budget_assert(budget, "memory_bytes", 8 * n_value)
  before <- as.numeric(x$values[, from])
  after <- as.numeric(x$values[, to])
  change <- after - before
  measure <- .ngeo_measures_for_layers(x, from)
  if (scale == "rate") {
    change <- change / (axis$time[[to]] - axis$time[[from]])
    measure$unit <- .ngeo_temporal_divide_unit(
      measure$unit[[1L]], axis$unit
    )
  } else if (scale == "percent") {
    change <- 100 * change / before
    change[before == 0] <- NA_real_
    measure$value_type <- "continuous"
    measure$support_behavior <- "intensive"
    measure$unit <- "percent"
    measure$aggregation <- "support_weighted_mean"
  }
  .ngeo_temporal_derived(
    x,
    matrix(change, ncol = 1L),
    name,
    "ngeo_longitudinal_change",
    list(
      from = from, to = to, scale = scale,
      axis_hash = axis$axis_hash
    ),
    measures = measure
  )
}

#' Fit per-element linear temporal trends
#'
#' @param x Time-aware `ngeo` data.
#' @param budget Resource limits for complete input and two-map output.
#' @return Two layers: intercept and slope per time unit.
#' @export
ngeo_temporal_trend <- function(
    x,
    budget = ngeo_resource_budget()) {
  axis <- ngeo_get_time_axis(x)
  if (length(axis$time) < 2L ||
      any(x$measures$temporal_semantics == "categorical")) {
    .ngeo_abort(
      "Temporal trend requires at least two numeric time layers.",
      "ngeo_error_temporal_measure"
    )
  }
  semantics <- unique(x$measures$temporal_semantics)
  if (length(semantics) != 1L) {
    .ngeo_abort(
      "Temporal trend requires one consistent temporal semantics.",
      "ngeo_error_temporal_measure"
    )
  }
  if (semantics == "interval_total" &&
      length(unique(round(axis$duration, 12L))) != 1L) {
    .ngeo_abort(
      "Trend of interval totals requires equal temporal support durations.",
      "ngeo_error_temporal_support"
    )
  }
  n_value <- as.double(nrow(x$base$elements)) * nrow(x$layers)
  .ngeo_budget_assert(
    budget, "materialized_elements",
    n_value + 2 * nrow(x$base$elements)
  )
  .ngeo_budget_assert(
    budget, "memory_bytes", 8 * (
      n_value + 2 * nrow(x$base$elements)
    )
  )
  values <- as.matrix(x$values)
  centered_time <- axis$time - mean(axis$time)
  denominator <- sum(centered_time^2)
  slope <- as.numeric(values %*% centered_time / denominator)
  intercept <- rowMeans(values) - slope * mean(axis$time)
  intercept_measure <- .ngeo_temporal_measure_template(x)
  slope_measure <- intercept_measure
  slope_measure$value_type <- "continuous"
  slope_measure$unit <- .ngeo_temporal_divide_unit(
    slope_measure$unit[[1L]], axis$unit
  )
  .ngeo_temporal_derived(
    x,
    cbind(intercept = intercept, slope = slope),
    c("intercept", "slope"),
    "ngeo_temporal_trend",
    list(axis_hash = axis$axis_hash, method = "ordinary_least_squares"),
    measures = rbind(intercept_measure, slope_measure)
  )
}

#' Compute a temporally support-aware contrast
#'
#' @param x Time-aware `ngeo` data.
#' @param operation Linear contrast, semantic mean, interval-total sum, or
#' interval integral.
#' @param coefficients Explicit coefficients for `operation = "linear"`.
#' @param name Output map name.
#' @param budget Resource limits for complete input and one-map output.
#' @return One target-base `ngeo` map.
#' @export
ngeo_temporal_contrast <- function(
    x,
    operation = c("linear", "mean", "sum", "integral"),
    coefficients = NULL,
    name = "temporal_contrast",
    budget = ngeo_resource_budget()) {
  axis <- ngeo_get_time_axis(x)
  operation <- match.arg(operation)
  semantics <- unique(x$measures$temporal_semantics)
  if (length(semantics) != 1L ||
      semantics == "categorical") {
    .ngeo_abort(
      "Temporal contrast requires one consistent numeric temporal semantics.",
      "ngeo_error_temporal_measure"
    )
  }
  semantics <- semantics[[1L]]
  n <- length(axis$time)
  n_value <- as.double(nrow(x$base$elements)) * n
  .ngeo_budget_assert(
    budget, "materialized_elements",
    n_value + nrow(x$base$elements)
  )
  .ngeo_budget_assert(
    budget, "memory_bytes",
    8 * (n_value + nrow(x$base$elements))
  )
  coefficient <- switch(
    operation,
    linear = {
      if (!is.numeric(coefficients) || length(coefficients) != n ||
          anyNA(coefficients) || any(!is.finite(coefficients))) {
        .ngeo_abort(
          "A linear temporal contrast requires one finite coefficient per map.",
          "ngeo_error_argument"
        )
      }
      as.numeric(coefficients)
    },
    mean = {
      if (semantics == "interval_total") {
        .ngeo_abort(
          "A mean of interval totals has no default support interpretation.",
          "ngeo_error_temporal_support"
        )
      }
      if (axis$support == "interval") {
        axis$duration / sum(axis$duration)
      } else {
        rep.int(1 / n, n)
      }
    },
    sum = {
      if (semantics != "interval_total") {
        .ngeo_abort(
          "Temporal sum is defined only for interval-total semantics.",
          "ngeo_error_temporal_measure"
        )
      }
      rep.int(1, n)
    },
    integral = {
      if (!semantics %in% c("rate", "interval_mean") ||
          axis$support != "interval") {
        .ngeo_abort(
          "Temporal integral requires rate or interval-mean semantics with interval support.",
          "ngeo_error_temporal_support"
        )
      }
      axis$duration
    }
  )
  output <- as.numeric(as.matrix(x$values) %*% coefficient)
  measure <- .ngeo_temporal_measure_template(x)
  if (operation == "integral") {
    measure$unit <- .ngeo_temporal_multiply_unit(
      measure$unit[[1L]], axis$unit
    )
  }
  .ngeo_temporal_derived(
    x,
    matrix(output, ncol = 1L),
    name,
    "ngeo_temporal_contrast",
    list(
      operation = operation,
      coefficient = coefficient,
      semantics = semantics,
      axis_hash = axis$axis_hash
    ),
    measures = measure
  )
}

#' @export
print.ngeo_time_axis <- function(x, ...) {
  cat(
    "<ngeo_time_axis>\n  coordinates: ", length(x$time),
    "\n  unit: ", x$unit,
    "\n  support: ", x$support,
    "\n  regular: ", x$regular, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_temporal_weights <- function(x, ...) {
  cat(
    "<ngeo_temporal_weights>\n  times: ", x$n_time,
    "\n  method: ", x$method,
    "\n  nonzero: ", length(x$matrix@x),
    "\n  normalization: ", x$normalization, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_spatiotemporal_weights <- function(x, ...) {
  cat(
    "<ngeo_spatiotemporal_weights>\n  coordinate_space: ", x$n_space,
    "\n  time: ", x$n_time,
    "\n  combination: ", x$combination,
    "\n  matrix materialized: ", x$matrix_materialized, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_spatiotemporal_moran <- function(x, ...) {
  cat(
    "<ngeo_spatiotemporal_moran>\n  estimate: ",
    format(x$estimate, digits = 6L),
    "\n  observations: ", x$n_observation,
    "\n  permutations: ", x$permutations,
    "\n  matrix materialized: ", x$matrix_materialized, "\n",
    sep = ""
  )
  invisible(x)
}
