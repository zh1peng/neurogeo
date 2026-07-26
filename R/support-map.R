.ngeo_support_vector <- function(x, support = NULL, name = "support") {
  if (is.null(support)) {
    support <- ngeo_support_size(x)
  }
  if (!is.numeric(support) ||
      length(support) != nrow(x$domain$elements) ||
      anyNA(support) || any(!is.finite(support)) ||
      any(support <= 0)) {
    .ngeo_abort(
      sprintf(
        "`%s` must be positive, finite, and aligned with source elements.",
        name
      ),
      "ngeo_error_support"
    )
  }
  as.numeric(support)
}

.ngeo_support_operator <- function(source, target, operator) {
  n_source <- nrow(source$domain$elements)
  n_target <- nrow(target$domain$elements)
  if (is.atomic(operator) && is.null(dim(operator))) {
    if (length(operator) != n_source) {
      .ngeo_abort(
        "Support membership must align with source elements.",
        "ngeo_error_alignment"
      )
    }
    target_id <- if ("region_id" %in% names(target$domain$elements)) {
      as.character(target$domain$elements$region_id)
    } else {
      target$domain$elements$element_id
    }
    row <- match(as.character(operator), target_id)
    keep <- !is.na(operator)
    if (anyNA(row[keep])) {
      .ngeo_abort(
        "Support membership contains an unknown target identifier.",
        "ngeo_error_support_map"
      )
    }
    return(Matrix::sparseMatrix(
      i = row[keep],
      j = which(keep),
      x = 1,
      dims = c(n_target, n_source)
    ))
  }
  if (!(is.matrix(operator) || inherits(operator, "Matrix")) ||
      !identical(dim(operator), c(n_target, n_source))) {
    .ngeo_abort(
      sprintf(
        "Support operator must be target by source (%d by %d).",
        n_target,
        n_source
      ),
      "ngeo_error_alignment"
    )
  }
  methods::as(operator, "dgCMatrix")
}

.ngeo_support_map_structure <- function(operator,
                                        type,
                                        source_hash,
                                        target_hash,
                                        source_id,
                                        target_id,
                                        source_support,
                                        target_support,
                                        weight_variance,
                                        coverage,
                                        provenance) {
  result <- structure(
    list(
      operator = methods::as(operator, "dgCMatrix"),
      type = type,
      direction = "target_by_source",
      source_domain_hash = source_hash,
      target_domain_hash = target_hash,
      source_element_id = source_id,
      target_element_id = target_id,
      source_support = source_support,
      target_support = target_support,
      weight_variance = weight_variance,
      coverage = coverage,
      provenance = provenance,
      spec_version = "2.0"
    ),
    class = "ngeo_support_map"
  )
  ngeo_validate_support_map(result)
  result
}

#' Construct an NGCS 2.0 sparse support map
#'
#' The operator orientation is always target by source. Its non-negative
#' entries describe how source support contributes to target support.
#'
#' @param source Source `ngeo` dataset.
#' @param target Target `ngeo` dataset.
#' @param operator Target-by-source sparse/dense matrix, or crisp membership.
#' @param type Crisp, probabilistic, or overlapping mapping.
#' @param source_support Optional positive source support sizes.
#' @param weight_variance Optional target-by-source independent weight
#'   variances.
#' @param coverage Require complete source coverage or allow partial coverage.
#' @param provenance Optional mapping provenance.
#'
#' @return An `ngeo_support_map`.
#' @export
ngeo_support_map <- function(
    source,
    target,
    operator,
    type = c("crisp", "probabilistic", "overlapping"),
    source_support = NULL,
    weight_variance = NULL,
    coverage = c("complete", "partial"),
    provenance = list()) {
  ngeo_validate(source, "strict")
  ngeo_validate(target, "strict")
  type <- match.arg(type)
  coverage <- match.arg(coverage)
  operator <- .ngeo_support_operator(source, target, operator)
  if (is.null(source_support)) {
    candidate <- ngeo_support_size(source)
    source_support <- if (
      is.numeric(candidate) && length(candidate) == ncol(operator) &&
      all(is.finite(candidate)) && all(candidate > 0)
    ) {
      as.numeric(candidate)
    } else {
      NULL
    }
  } else {
    source_support <- .ngeo_support_vector(
      source,
      source_support,
      "source_support"
    )
  }
  if (!is.null(weight_variance)) {
    if (!(is.matrix(weight_variance) ||
        inherits(weight_variance, "Matrix")) ||
        !identical(dim(weight_variance), dim(operator))) {
      .ngeo_abort(
        "`weight_variance` must align with the support operator.",
        "ngeo_error_alignment"
      )
    }
    weight_variance <- methods::as(weight_variance, "dgCMatrix")
  }
  target_support <- if (is.null(source_support)) {
    NULL
  } else {
    as.numeric(operator %*% source_support)
  }
  provenance$operations <- c(
    provenance$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_support_map",
      list(type = type, coverage = coverage)
    ))
  )
  result <- .ngeo_support_map_structure(
    operator,
    type,
    ngeo_domain_hash(source),
    ngeo_domain_hash(target),
    source$domain$elements$element_id,
    target$domain$elements$element_id,
    source_support,
    target_support,
    weight_variance,
    coverage,
    provenance
  )
  declared_target_support <- ngeo_support_size(target)
  if (!is.null(target_support) &&
      is.numeric(declared_target_support) &&
      length(declared_target_support) == length(target_support) &&
      all(is.finite(declared_target_support)) &&
      !isTRUE(all.equal(
        declared_target_support,
        target_support,
        tolerance = 1e-8,
        check.attributes = FALSE
      ))) {
    .ngeo_abort(
      "Target domain support sizes do not match mapped source support.",
      "ngeo_error_support"
    )
  }
  result
}

#' Convert a crisp partition to an NGCS 2.0 support map
#'
#' @param source Source `ngeo` dataset.
#' @param partition Matching `ngeo_partition`.
#' @param target Target regions template with the partition's region order.
#'
#' @return A crisp `ngeo_support_map`.
#' @export
ngeo_support_map_from_partition <- function(source, partition, target) {
  .ngeo_validate_partition(partition, source)
  ngeo_validate(target, "strict")
  target_id <- if ("region_id" %in% names(target$domain$elements)) {
    as.character(target$domain$elements$region_id)
  } else {
    target$domain$elements$element_id
  }
  if (!identical(
    target_id,
    as.character(partition$regions$region_id)
  )) {
    .ngeo_abort(
      "Target region order must match the crisp partition.",
      "ngeo_error_alignment"
    )
  }
  ngeo_support_map(
    source,
    target,
    partition$membership,
    type = "crisp",
    coverage = if (anyNA(partition$membership)) "partial" else "complete",
    provenance = list(
      partition = partition$provenance,
      migration = "NGCS 1.x ngeo_partition"
    )
  )
}

#' Validate an NGCS 2.0 support map
#'
#' @param x An `ngeo_support_map`.
#' @param tolerance Numeric invariant tolerance.
#'
#' @return `x`, invisibly.
#' @export
ngeo_validate_support_map <- function(x, tolerance = 1e-10) {
  if (!inherits(x, "ngeo_support_map")) {
    .ngeo_abort(
      "`x` must be an `ngeo_support_map`.",
      "ngeo_error_support_map"
    )
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance < 0) {
    .ngeo_abort(
      "`tolerance` must be one finite non-negative number.",
      "ngeo_error_argument"
    )
  }
  operator_valid <- inherits(x$operator, "dgCMatrix") &&
    isTRUE(tryCatch(
      methods::validObject(x$operator, test = TRUE) == TRUE,
      error = function(...) FALSE
    ))
  if (!operator_valid ||
      length(x$source_element_id) != ncol(x$operator) ||
      length(x$target_element_id) != nrow(x$operator) ||
      !is.character(x$source_element_id) ||
      !is.character(x$target_element_id) ||
      anyNA(x$source_element_id) ||
      anyNA(x$target_element_id) ||
      anyDuplicated(x$source_element_id) ||
      anyDuplicated(x$target_element_id)) {
    .ngeo_abort(
      "Support-map dimensions and element IDs are inconsistent.",
      "ngeo_error_support_map"
    )
  }
  if (!is.character(x$source_domain_hash) ||
      length(x$source_domain_hash) != 1L ||
      is.na(x$source_domain_hash) ||
      !nzchar(x$source_domain_hash) ||
      !is.character(x$target_domain_hash) ||
      length(x$target_domain_hash) != 1L ||
      is.na(x$target_domain_hash) ||
      !nzchar(x$target_domain_hash) ||
      !identical(x$direction, "target_by_source") ||
      !is.list(x$provenance)) {
    .ngeo_abort(
      "Support-map identity, direction, or provenance is invalid.",
      "ngeo_error_support_map"
    )
  }
  if (length(x$operator@x) &&
      (any(!is.finite(x$operator@x)) || any(x$operator@x < 0))) {
    .ngeo_abort(
      "Support operator entries must be finite and non-negative.",
      "ngeo_error_support_map"
    )
  }
  column_sum <- Matrix::colSums(x$operator)
  column_nnz <- diff(x$operator@p)
  if (identical(x$type, "crisp") &&
      (any(abs(x$operator@x - 1) > tolerance) ||
        any(column_nnz > 1L))) {
    .ngeo_abort(
      "A crisp support map permits at most one unit entry per source.",
      "ngeo_error_support_map"
    )
  }
  if (identical(x$type, "probabilistic") &&
      any(column_sum > 1 + tolerance)) {
    .ngeo_abort(
      "Probabilistic source memberships cannot sum above one.",
      "ngeo_error_support_map"
    )
  }
  if (!x$type %in% c("crisp", "probabilistic", "overlapping") ||
      !x$coverage %in% c("complete", "partial")) {
    .ngeo_abort(
      "Support-map type or coverage policy is invalid.",
      "ngeo_error_support_map"
    )
  }
  if (identical(x$coverage, "complete")) {
    valid <- if (identical(x$type, "overlapping")) {
      column_sum > tolerance
    } else {
      abs(column_sum - 1) <= tolerance
    }
    if (!all(valid)) {
      .ngeo_abort(
        "Complete support maps must cover every source element.",
        "ngeo_error_support_map"
      )
    }
  }
  if (!is.null(x$source_support) &&
      (length(x$source_support) != ncol(x$operator) ||
        any(!is.finite(x$source_support)) ||
        any(x$source_support <= 0))) {
    .ngeo_abort(
      "Stored source supports are invalid.",
      "ngeo_error_support"
    )
  }
  if (!is.null(x$target_support) &&
      (length(x$target_support) != nrow(x$operator) ||
        any(!is.finite(x$target_support)) ||
        any(x$target_support < 0))) {
    .ngeo_abort(
      "Stored target supports are invalid.",
      "ngeo_error_support"
    )
  }
  if (!is.null(x$source_support) && !is.null(x$target_support) &&
      !isTRUE(all.equal(
        as.numeric(x$operator %*% x$source_support),
        as.numeric(x$target_support),
        tolerance = max(tolerance, sqrt(.Machine$double.eps)),
        check.attributes = FALSE
      ))) {
    .ngeo_abort(
      "Stored target support is inconsistent with the sparse operator.",
      "ngeo_error_support"
    )
  }
  if (!is.null(x$weight_variance)) {
    variance_valid <- inherits(x$weight_variance, "dgCMatrix") &&
      isTRUE(tryCatch(
        methods::validObject(x$weight_variance, test = TRUE) == TRUE,
        error = function(...) FALSE
      ))
    if (!variance_valid ||
        !identical(dim(x$weight_variance), dim(x$operator)) ||
        any(!is.finite(x$weight_variance@x)) ||
        any(x$weight_variance@x < 0)) {
      .ngeo_abort(
        "Support weight variances are invalid.",
        "ngeo_error_uncertainty"
      )
    }
  }
  invisible(x)
}

#' Hash an NGCS support map
#'
#' @param x An `ngeo_support_map`.
#'
#' @return An xxHash64 string.
#' @export
ngeo_support_map_hash <- function(x) {
  ngeo_validate_support_map(x)
  digest::digest(
    list(
      operator = x$operator,
      type = x$type,
      source = x$source_domain_hash,
      target = x$target_domain_hash,
      coverage = x$coverage
    ),
    algo = "xxhash64",
    serialize = TRUE
  )
}

.ngeo_validate_support_domains <- function(x, target, support_map) {
  ngeo_validate(x, "strict")
  ngeo_validate(target, "strict")
  ngeo_validate_support_map(support_map)
  if (!identical(
    ngeo_domain_hash(x),
    support_map$source_domain_hash
  ) || !identical(
    ngeo_domain_hash(target),
    support_map$target_domain_hash
  )) {
    .ngeo_abort(
      "Support-map source or target domain hash does not match.",
      "ngeo_error_domain_mismatch"
    )
  }
  invisible(TRUE)
}

.ngeo_allocation_operator <- function(support_map,
                                      allocation,
                                      unmapped,
                                      tolerance = 1e-10) {
  operator <- support_map$operator
  column_sum <- Matrix::colSums(operator)
  if (any(column_sum <= tolerance) && identical(unmapped, "error")) {
    .ngeo_abort(
      "Support map leaves source elements unmapped.",
      "ngeo_error_support_map"
    )
  }
  if (any(abs(column_sum[column_sum > tolerance] - 1) > tolerance)) {
    if (identical(allocation, "error")) {
      .ngeo_abort(
        "Conservative allocation requires unit column sums.",
        "ngeo_error_conservation"
      )
    }
    inverse <- numeric(length(column_sum))
    inverse[column_sum > tolerance] <- 1 / column_sum[column_sum > tolerance]
    operator <- operator %*% Matrix::Diagonal(x = inverse)
  }
  methods::as(operator, "dgCMatrix")
}

.ngeo_weighted_mode_operator <- function(values, operator, support) {
  categories <- sort(unique(values))
  scores <- vapply(categories, function(category) {
    as.numeric(operator %*% (support * (values == category)))
  }, numeric(nrow(operator)))
  scores <- matrix(
    scores,
    nrow = nrow(operator),
    ncol = length(categories)
  )
  categories[max.col(scores, ties.method = "first")]
}

#' Change values from one explicit support to another
#'
#' Intensive maps use support-normalized weighted means. Extensive and count
#' maps use conservative allocation; overlapping columns require explicit
#' normalization. Categorical maps use support-weighted modes.
#'
#' @param x Source `ngeo` dataset.
#' @param target Target-domain `ngeo` template.
#' @param support_map Matching `ngeo_support_map`.
#' @param maps Optional source map selection.
#' @param allocation Normalize non-unit columns or reject them.
#' @param unmapped Reject or explicitly drop unmapped source support.
#' @param unknown Interpretation for maps with unknown spatial semantics.
#'
#' @return A new `ngeo` dataset on the target domain.
#' @export
ngeo_change_support <- function(
    x,
    target,
    support_map,
    maps = NULL,
    allocation = c("error", "normalize"),
    unmapped = c("error", "drop"),
    unknown = c("error", "intensive", "extensive")) {
  if (inherits(support_map, "ngeo_block_support_map")) {
    return(ngeo_change_support_block(
      x = x,
      target = target,
      support_map = support_map,
      maps = maps,
      allocation = allocation,
      unmapped = unmapped,
      unknown = unknown
    ))
  }
  .ngeo_validate_support_domains(x, target, support_map)
  allocation <- match.arg(allocation)
  unmapped <- match.arg(unmapped)
  unknown <- match.arg(unknown)
  if (is.null(x$values)) {
    .ngeo_abort(
      "Change of support requires loaded values.",
      "ngeo_error_values"
    )
  }
  map_index <- .ngeo_map_selection(x, maps)
  source_support <- support_map$source_support %||%
    .ngeo_support_vector(x)
  operator <- support_map$operator
  target_support <- as.numeric(operator %*% source_support)
  allocation_operator <- NULL
  output <- vector("list", length(map_index))
  semantics_out <- x$measures$spatial_semantics[map_index]
  for (i in seq_along(map_index)) {
    map <- map_index[[i]]
    values <- as.numeric(x$values[, map])
    if (any(!is.finite(values))) {
      .ngeo_abort(
        "Change of support currently requires finite source values.",
        "ngeo_error_missing"
      )
    }
    semantics <- semantics_out[[i]]
    if (identical(semantics, "unknown")) {
      if (identical(unknown, "error")) {
        .ngeo_abort(
          "Declare how unknown measurement semantics change support.",
          "ngeo_error_measure"
        )
      }
      semantics <- unknown
      semantics_out[[i]] <- semantics
    }
    output[[i]] <- switch(
      semantics,
      intensive = {
        numerator <- as.numeric(operator %*% (source_support * values))
        result <- numerator / target_support
        result[target_support == 0] <- NA_real_
        result
      },
      extensive = ,
      count = {
        allocation_operator <- allocation_operator %||%
          .ngeo_allocation_operator(
            support_map,
            allocation,
            unmapped
          )
        as.numeric(allocation_operator %*% values)
      },
      categorical = .ngeo_weighted_mode_operator(
        values,
        operator,
        source_support
      ),
      .ngeo_abort(
        sprintf("Unsupported spatial semantics `%s`.", semantics),
        "ngeo_error_measure"
      )
    )
  }
  output <- do.call(cbind, output)
  maps_out <- x$maps[map_index, , drop = FALSE]
  measures_out <- x$measures[map_index, , drop = FALSE]
  measures_out$spatial_semantics <- semantics_out
  colnames(output) <- maps_out$name
  result <- .new_ngeo(
    domain = target$domain,
    values = output,
    maps = maps_out,
    measures = measures_out,
    labels = x$labels,
    provenance = list(
      spec_version = "2.0",
      source_dataset = list(
        domain_hash = ngeo_domain_hash(x),
        provenance = x$provenance
      ),
      target_template = list(
        domain_hash = ngeo_domain_hash(target),
        provenance = target$provenance
      ),
      support_map_hash = ngeo_support_map_hash(support_map),
      operations = list(.ngeo_operation(
        "ngeo_change_support",
        list(
          allocation = allocation,
          unmapped = unmapped,
          source_support_total = sum(source_support),
          target_support_total = sum(target_support)
        )
      ))
    ),
    class = class(target)[[1L]]
  )
  ngeo_validate(result, "strict")
  result
}

#' Propagate independent value and support-weight uncertainty
#'
#' @inheritParams ngeo_change_support
#' @param value_variance Source element-by-map independent variances.
#'
#' @return A target element-by-map variance matrix.
#' @export
ngeo_support_variance <- function(
    x,
    target,
    support_map,
    value_variance,
    maps = NULL,
    allocation = c("error", "normalize"),
    unmapped = c("error", "drop"),
    unknown = c("error", "intensive", "extensive")) {
  .ngeo_validate_support_domains(x, target, support_map)
  allocation <- match.arg(allocation)
  unmapped <- match.arg(unmapped)
  unknown <- match.arg(unknown)
  map_index <- .ngeo_map_selection(x, maps)
  if (is.atomic(value_variance) && is.null(dim(value_variance))) {
    value_variance <- matrix(value_variance, ncol = 1L)
  }
  if (!is.matrix(value_variance) ||
      nrow(value_variance) != nrow(x$domain$elements) ||
      !ncol(value_variance) %in% c(1L, length(map_index)) ||
      anyNA(value_variance) || any(!is.finite(value_variance)) ||
      any(value_variance < 0)) {
    .ngeo_abort(
      "`value_variance` must be non-negative and source-by-selected-map.",
      "ngeo_error_uncertainty"
    )
  }
  if (ncol(value_variance) == 1L && length(map_index) > 1L) {
    value_variance <- value_variance[
      , rep.int(1L, length(map_index)),
      drop = FALSE
    ]
  }
  source_support <- support_map$source_support %||%
    .ngeo_support_vector(x)
  operator <- support_map$operator
  operator_variance <- support_map$weight_variance
  result <- matrix(
    NA_real_,
    nrow = nrow(operator),
    ncol = length(map_index)
  )
  for (i in seq_along(map_index)) {
    map <- map_index[[i]]
    semantics <- x$measures$spatial_semantics[[map]]
    if (identical(semantics, "unknown")) {
      if (identical(unknown, "error")) {
        .ngeo_abort(
          "Declare unknown measurement semantics before uncertainty propagation.",
          "ngeo_error_measure"
        )
      }
      semantics <- unknown
    }
    if (identical(semantics, "categorical")) {
      .ngeo_abort(
        "Categorical mode uncertainty is not represented as a variance.",
        "ngeo_error_uncertainty"
      )
    }
    values <- as.numeric(x$values[, map])
    if (identical(semantics, "intensive")) {
      denominator <- as.numeric(operator %*% source_support)
      derivative <- operator %*% Matrix::Diagonal(
        x = source_support
      )
      derivative <- Matrix::Diagonal(x = 1 / denominator) %*% derivative
      variance <- as.numeric((derivative^2) %*% value_variance[, i])
      if (!is.null(operator_variance)) {
        estimate <- as.numeric(
          operator %*% (source_support * values) / denominator
        )
        entries <- Matrix::summary(operator_variance)
        contribution <- entries$x *
          (
            source_support[entries$j] *
              (values[entries$j] - estimate[entries$i]) /
              denominator[entries$i]
          )^2
        variance <- variance + as.numeric(Matrix::sparseMatrix(
          i = entries$i,
          j = rep.int(1L, nrow(entries)),
          x = contribution,
          dims = c(nrow(operator), 1L)
        ))
      }
      variance[!is.finite(variance)] <- NA_real_
      result[, i] <- variance
    } else {
      column_sum <- Matrix::colSums(support_map$operator)
      if (!is.null(operator_variance) &&
          any(abs(column_sum[column_sum > 0] - 1) > 1e-10)) {
        .ngeo_abort(
          "Uncertain overlap normalization requires a covariance model.",
          "ngeo_error_uncertainty"
        )
      }
      allocation_operator <- .ngeo_allocation_operator(
        support_map,
        allocation,
        unmapped
      )
      variance <- as.numeric(
        (allocation_operator^2) %*% value_variance[, i]
      )
      if (!is.null(operator_variance)) {
        variance <- variance + as.numeric(
          operator_variance %*% (values^2)
        )
      }
      result[, i] <- variance
    }
  }
  colnames(result) <- x$maps$name[map_index]
  result
}

#' Compose compatible sparse support maps
#'
#' @param first Source-to-intermediate support map.
#' @param second Intermediate-to-target support map.
#'
#' @return A composed `ngeo_support_map`.
#' @export
ngeo_compose_support_map <- function(first, second) {
  ngeo_validate_support_map(first)
  ngeo_validate_support_map(second)
  if (!is.null(first$weight_variance) ||
      !is.null(second$weight_variance)) {
    .ngeo_abort(
      "Composition of uncertain operators requires an explicit covariance model.",
      "ngeo_error_uncertainty"
    )
  }
  if (!identical(
    first$target_domain_hash,
    second$source_domain_hash
  ) || !identical(
    first$target_element_id,
    second$source_element_id
  )) {
    .ngeo_abort(
      "Support-map composition requires identical intermediate domains.",
      "ngeo_error_domain_mismatch"
    )
  }
  operator <- methods::as(
    second$operator %*% first$operator,
    "dgCMatrix"
  )
  column_sum <- Matrix::colSums(operator)
  column_nnz <- diff(operator@p)
  type <- if (
    all(abs(operator@x - 1) <= 1e-10) &&
      all(column_nnz <= 1L)
  ) {
    "crisp"
  } else if (all(column_sum <= 1 + 1e-10)) {
    "probabilistic"
  } else {
    "overlapping"
  }
  .ngeo_support_map_structure(
    operator,
    type,
    first$source_domain_hash,
    second$target_domain_hash,
    first$source_element_id,
    second$target_element_id,
    first$source_support,
    if (is.null(first$source_support)) {
      NULL
    } else {
      as.numeric(operator %*% first$source_support)
    },
    NULL,
    if (
      identical(first$coverage, "complete") &&
        identical(second$coverage, "complete")
    ) "complete" else "partial",
    list(operations = list(.ngeo_operation(
      "ngeo_compose_support_map",
      list(
        first = ngeo_support_map_hash(first),
        second = ngeo_support_map_hash(second)
      )
    )))
  )
}

#' @export
print.ngeo_support_map <- function(x, ...) {
  cat(
    "<ngeo_support_map>\n",
    "  type: ", x$type, "\n",
    "  direction: ", x$direction, "\n",
    "  dimensions: ", nrow(x$operator), " target x ",
    ncol(x$operator), " source\n",
    "  coverage: ", x$coverage, "\n",
    sep = ""
  )
  invisible(x)
}
