#' Partition one logical sparse support operator into validated blocks
#'
#' @param x An `ngeo_support_map`.
#' @param row_block_size,source_block_size Positive block sizes.
#'
#' @return An `ngeo_block_support_map`.
#' @export
ngeo_block_support_map <- function(
    x,
    row_block_size = 10000L,
    source_block_size = 100000L) {
  ngeo_validate_support_map(x)
  row_block_size <- .ngeo_as_integer(row_block_size, "row_block_size")
  source_block_size <- .ngeo_as_integer(
    source_block_size, "source_block_size"
  )
  if (length(row_block_size) != 1L || row_block_size < 1L ||
      length(source_block_size) != 1L || source_block_size < 1L) {
    .ngeo_abort("Block sizes must be positive.", "ngeo_error_argument")
  }
  row_group <- split(
    seq_len(nrow(x$operator)),
    ceiling(seq_len(nrow(x$operator)) / row_block_size)
  )
  column_group <- split(
    seq_len(ncol(x$operator)),
    ceiling(seq_len(ncol(x$operator)) / source_block_size)
  )
  blocks <- lapply(row_group, function(rows) {
    lapply(column_group, function(columns) {
      methods::as(x$operator[rows, columns, drop = FALSE], "dgCMatrix")
    })
  })
  variance_blocks <- if (is.null(x$weight_variance)) NULL else
    lapply(row_group, function(rows) {
      lapply(column_group, function(columns) {
        methods::as(
          x$weight_variance[rows, columns, drop = FALSE],
          "dgCMatrix"
        )
      })
    })
  result <- list(
    blocks = blocks,
    variance_blocks = variance_blocks,
    row_groups = row_group,
    column_groups = column_group,
    dim = dim(x$operator),
    source_element_id = x$source_element_id,
    target_element_id = x$target_element_id,
    metadata = x[setdiff(
      names(x),
      c(
        "operator", "weight_variance",
        "source_element_id", "target_element_id"
      )
    )],
    logical_hash = ngeo_support_map_hash(x),
    orientation = "target_by_source"
  )
  result$block_hash <- .ngeo_block_content_hash(result)
  class(result) <- "ngeo_block_support_map"
  ngeo_validate_block_support_map(result)
  result
}

.ngeo_block_content_hash <- function(x) {
  digest::digest(
    list(
      dim = x$dim,
      row_groups = x$row_groups,
      column_groups = x$column_groups,
      blocks = lapply(x$blocks, function(row) {
        lapply(row, function(block) {
          entries <- Matrix::summary(block)
          list(dim = dim(block), i = entries$i, j = entries$j, x = entries$x)
        })
      }),
      variance_blocks = if (is.null(x$variance_blocks)) NULL else
        lapply(x$variance_blocks, function(row) {
          lapply(row, function(block) {
            entries <- Matrix::summary(block)
            list(
              dim = dim(block),
              i = entries$i,
              j = entries$j,
              x = entries$x
            )
          })
        })
    ),
    algo = "sha256"
  )
}

.ngeo_block_operator <- function(x) {
  row <- lapply(x$blocks, function(current) {
    do.call(cbind, current)
  })
  methods::as(do.call(rbind, row), "dgCMatrix")
}

.ngeo_block_variance <- function(x) {
  if (is.null(x$variance_blocks)) return(NULL)
  row <- lapply(x$variance_blocks, function(current) do.call(cbind, current))
  methods::as(do.call(rbind, row), "dgCMatrix")
}

#' Validate a block support map
#'
#' @param x An `ngeo_block_support_map`.
#' @return `x`, invisibly.
#' @export
ngeo_validate_block_support_map <- function(x) {
  if (!inherits(x, "ngeo_block_support_map") ||
      !identical(x$orientation, "target_by_source") ||
      !identical(unname(unlist(x$row_groups)), seq_len(x$dim[[1L]])) ||
      !identical(unname(unlist(x$column_groups)), seq_len(x$dim[[2L]])) ||
      length(x$blocks) != length(x$row_groups) ||
      any(vapply(
        x$blocks,
        length,
        integer(1)
      ) != length(x$column_groups)) ||
      (!is.null(x$variance_blocks) &&
        (length(x$variance_blocks) != length(x$row_groups) ||
          any(vapply(
            x$variance_blocks,
            length,
            integer(1)
          ) != length(x$column_groups))))) {
    .ngeo_abort("Invalid logical block support map.", "ngeo_error_support_map")
  }
  for (i in seq_along(x$row_groups)) {
    for (j in seq_along(x$column_groups)) {
      block <- x$blocks[[i]][[j]]
      if (!inherits(block, "Matrix") ||
          !identical(
            dim(block),
            c(length(x$row_groups[[i]]), length(x$column_groups[[j]]))
          ) ||
          any(!is.finite(block@x)) || any(block@x < 0)) {
        .ngeo_abort("A support block is invalid.", "ngeo_error_support_map")
      }
      if (!is.null(x$variance_blocks)) {
        variance <- x$variance_blocks[[i]][[j]]
        if (!inherits(variance, "Matrix") ||
            !identical(dim(variance), dim(block)) ||
            any(!is.finite(variance@x)) || any(variance@x < 0)) {
          .ngeo_abort(
            "A support variance block is invalid.",
            "ngeo_error_uncertainty"
          )
        }
      }
    }
  }
  if (!identical(.ngeo_block_content_hash(x), x$block_hash)) {
    .ngeo_abort(
      "Block logical hash verification failed.",
      c("ngeo_error_io", "ngeo_error_support_map")
    )
  }
  invisible(x)
}

#' Materialize the logical sparse support map
#'
#' @param x An `ngeo_block_support_map`.
#' @param budget Resource budget.
#' @return An `ngeo_support_map`.
#' @export
ngeo_materialize_support_map <- function(
    x,
    budget = ngeo_resource_budget()) {
  ngeo_validate_block_support_map(x)
  .ngeo_budget_assert(
    budget,
    "materialized_elements",
    prod(as.double(x$dim))
  )
  result <- x$metadata
  result$source_element_id <- x$source_element_id
  result$target_element_id <- x$target_element_id
  result$operator <- .ngeo_block_operator(x)
  result$weight_variance <- .ngeo_block_variance(x)
  class(result) <- "ngeo_support_map"
  ngeo_validate_support_map(result)
  if (!identical(ngeo_support_map_hash(result), x$logical_hash)) {
    .ngeo_abort("Materialized logical hash verification failed.",
                "ngeo_error_io")
  }
  result
}

#' Block-aware change of support
#'
#' @inheritParams ngeo_change_support
#' @param support_map An `ngeo_block_support_map`.
#' @param budget Resource budget.
#'
#' @return An `ngeo` dataset on `target`.
#' @export
ngeo_change_support_block <- function(
    x,
    target,
    support_map,
    maps = NULL,
    allocation = c("error", "normalize"),
    unmapped = c("error", "drop"),
    unknown = c("error", "intensive", "extensive"),
    budget = ngeo_resource_budget()) {
  allocation <- match.arg(allocation)
  unmapped <- match.arg(unmapped)
  unknown <- match.arg(unknown)
  ngeo_validate_block_support_map(support_map)
  .ngeo_budget_assert(
    budget,
    "blocks",
    length(support_map$row_groups) * length(support_map$column_groups)
  )
  .ngeo_budget_assert(
    budget,
    "memory_bytes",
    8 * support_map$dim[[1L]] * max(3L, length(.ngeo_map_selection(x, maps)))
  )
  .ngeo_budget_assert(
    budget,
    "materialized_elements",
    support_map$dim[[1L]] * length(.ngeo_map_selection(x, maps))
  )
  ngeo_validate(x, "strict")
  ngeo_validate(target, "strict")
  if (!identical(ngeo_domain_hash(x),
                 support_map$metadata$source_domain_hash) ||
      !identical(ngeo_domain_hash(target),
                 support_map$metadata$target_domain_hash) ||
      !identical(x$domain$elements$element_id,
                 support_map$source_element_id) ||
      !identical(target$domain$elements$element_id,
                 support_map$target_element_id)) {
    .ngeo_abort("Block support domains do not align.",
                "ngeo_error_domain_mismatch")
  }
  if (is.null(x$values)) {
    .ngeo_abort("Change of support requires loaded values.",
                "ngeo_error_values")
  }
  map_index <- .ngeo_map_selection(x, maps)
  source_support <- support_map$metadata$source_support %||%
    .ngeo_support_vector(x)
  target_support <- numeric(support_map$dim[[1L]])
  column_sum <- numeric(support_map$dim[[2L]])
  for (i in seq_along(support_map$row_groups)) {
    rows <- support_map$row_groups[[i]]
    for (j in seq_along(support_map$column_groups)) {
      columns <- support_map$column_groups[[j]]
      block <- support_map$blocks[[i]][[j]]
      target_support[rows] <- target_support[rows] +
        as.numeric(block %*% source_support[columns])
      column_sum[columns] <- column_sum[columns] +
        Matrix::colSums(block)
    }
  }
  if (any(column_sum <= 1e-10) && unmapped == "error") {
    .ngeo_abort("Support map leaves source elements unmapped.",
                "ngeo_error_support_map")
  }
  allocation_scale <- rep.int(1, length(column_sum))
  nonunit <- column_sum > 1e-10 & abs(column_sum - 1) > 1e-10
  if (any(nonunit)) {
    if (allocation == "error") {
      allocation_scale[nonunit] <- NA_real_
    } else {
      allocation_scale[column_sum > 1e-10] <-
        1 / column_sum[column_sum > 1e-10]
    }
  }
  output <- matrix(
    NA_real_,
    nrow = support_map$dim[[1L]],
    ncol = length(map_index)
  )
  semantics_out <- x$measures$spatial_semantics[map_index]
  for (m in seq_along(map_index)) {
    semantics <- semantics_out[[m]]
    if (semantics == "unknown") {
      if (unknown == "error") {
        .ngeo_abort("Declare unknown measurement semantics.",
                    "ngeo_error_measure")
      }
      semantics <- unknown
      semantics_out[[m]] <- semantics
    }
    if (semantics %in% c("extensive", "count") &&
        any(nonunit) && allocation == "error") {
      .ngeo_abort("Conservative allocation requires unit column sums.",
                  "ngeo_error_conservation")
    }
    values_by_column <- lapply(
      support_map$column_groups,
      function(columns) {
        value <- as.numeric(x$values[columns, map_index[[m]]])
        if (any(!is.finite(value))) {
          .ngeo_abort("Change of support requires finite values.",
                      "ngeo_error_missing")
        }
        value
      }
    )
    if (semantics == "categorical") {
      categories <- sort(unique(unlist(values_by_column, use.names = FALSE)))
      score <- matrix(
        0,
        nrow = support_map$dim[[1L]],
        ncol = length(categories)
      )
      for (i in seq_along(support_map$row_groups)) {
        rows <- support_map$row_groups[[i]]
        for (j in seq_along(support_map$column_groups)) {
          columns <- support_map$column_groups[[j]]
          block <- support_map$blocks[[i]][[j]]
          for (category in seq_along(categories)) {
            score[rows, category] <- score[rows, category] +
              as.numeric(block %*% (
                source_support[columns] *
                  (values_by_column[[j]] == categories[[category]])
              ))
          }
        }
      }
      output[, m] <- categories[max.col(score, ties.method = "first")]
    } else {
      accumulated <- numeric(support_map$dim[[1L]])
      for (i in seq_along(support_map$row_groups)) {
        rows <- support_map$row_groups[[i]]
        for (j in seq_along(support_map$column_groups)) {
          columns <- support_map$column_groups[[j]]
          block <- support_map$blocks[[i]][[j]]
          source_value <- values_by_column[[j]]
          if (semantics == "intensive") {
            source_value <- source_support[columns] * source_value
          } else {
            source_value <- allocation_scale[columns] * source_value
          }
          accumulated[rows] <- accumulated[rows] +
            as.numeric(block %*% source_value)
        }
      }
      if (semantics == "intensive") {
        accumulated <- accumulated / target_support
        accumulated[target_support == 0] <- NA_real_
      }
      output[, m] <- accumulated
    }
  }
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
      spec_version = "2.6",
      source_dataset = list(
        domain_hash = ngeo_domain_hash(x),
        provenance = x$provenance
      ),
      target_template = list(
        domain_hash = ngeo_domain_hash(target),
        provenance = target$provenance
      ),
      support_map_hash = support_map$logical_hash,
      block_hash = support_map$block_hash,
      operations = list(.ngeo_operation(
        "ngeo_change_support_block",
        list(
          blocks = length(support_map$row_groups) *
            length(support_map$column_groups),
          allocation = allocation,
          unmapped = unmapped,
          materialized_operator = FALSE
        )
      ))
    ),
    class = class(target)[[1L]]
  )
  ngeo_validate(result, "strict")
  result
}

#' Diagnose a logical support operator without materialization
#'
#' @param x An `ngeo_block_support_map`.
#' @param budget Resource budget.
#'
#' @return An `ngeo_block_support_diagnostics`.
#' @export
ngeo_block_diagnostics <- function(
    x,
    budget = ngeo_resource_budget()) {
  ngeo_validate_block_support_map(x)
  block_count <- length(x$row_groups) * length(x$column_groups)
  .ngeo_budget_assert(budget, "blocks", block_count)
  row_sum <- numeric(x$dim[[1L]])
  column_sum <- numeric(x$dim[[2L]])
  nonzero <- 0
  minimum <- Inf
  maximum <- -Inf
  for (i in seq_along(x$row_groups)) {
    rows <- x$row_groups[[i]]
    for (j in seq_along(x$column_groups)) {
      columns <- x$column_groups[[j]]
      block <- x$blocks[[i]][[j]]
      row_sum[rows] <- row_sum[rows] + Matrix::rowSums(block)
      column_sum[columns] <- column_sum[columns] + Matrix::colSums(block)
      nonzero <- nonzero + length(block@x)
      if (length(block@x)) {
        minimum <- min(minimum, block@x)
        maximum <- max(maximum, block@x)
      }
    }
  }
  result <- list(
    dimensions = x$dim,
    blocks = block_count,
    nonzero = nonzero,
    density = nonzero / prod(as.double(x$dim)),
    target_isolates = which(row_sum == 0),
    source_unmapped = which(column_sum == 0),
    column_sum_range = range(column_sum),
    row_sum_range = range(row_sum),
    weight_range = if (is.finite(minimum)) c(minimum, maximum) else c(NA, NA),
    logical_hash = x$logical_hash,
    block_hash = x$block_hash,
    materialized_operator = FALSE
  )
  class(result) <- "ngeo_block_support_diagnostics"
  result
}

#' Propagate independent variance blockwise
#'
#' @param x Source dataset.
#' @param support_map Block support map.
#' @param value_variance Source-by-map non-negative variance.
#' @inheritParams ngeo_support_variance
#' @param budget Resource budget.
#'
#' @return A target-by-map variance matrix.
#' @export
ngeo_block_variance <- function(
    x,
    target,
    support_map,
    value_variance,
    maps = NULL,
    allocation = c("error", "normalize"),
    unmapped = c("error", "drop"),
    unknown = c("error", "intensive", "extensive"),
    budget = ngeo_resource_budget()) {
  allocation <- match.arg(allocation)
  unmapped <- match.arg(unmapped)
  unknown <- match.arg(unknown)
  ngeo_validate_block_support_map(support_map)
  if (!is.null(support_map$variance_blocks)) {
    .ngeo_abort(
      paste(
        "Block variance with uncertain operator weights requires",
        "an explicit covariance model."
      ),
      "ngeo_error_uncertainty"
    )
  }
  map_index <- .ngeo_map_selection(x, maps)
  if (is.atomic(value_variance) && is.null(dim(value_variance))) {
    value_variance <- matrix(value_variance, ncol = 1L)
  }
  if (!is.matrix(value_variance) ||
      nrow(value_variance) != support_map$dim[[2L]] ||
      !ncol(value_variance) %in% c(1L, length(map_index)) ||
      any(!is.finite(value_variance)) || any(value_variance < 0)) {
    .ngeo_abort("Value variance does not align with block sources.",
                "ngeo_error_uncertainty")
  }
  if (ncol(value_variance) == 1L && length(map_index) > 1L) {
    value_variance <- value_variance[
      , rep.int(1L, length(map_index)), drop = FALSE
    ]
  }
  .ngeo_budget_assert(
    budget, "memory_bytes",
    8 * support_map$dim[[1L]] * (length(map_index) + 2L)
  )
  .ngeo_budget_assert(
    budget,
    "blocks",
    length(support_map$row_groups) * length(support_map$column_groups)
  )
  .ngeo_budget_assert(
    budget,
    "materialized_elements",
    support_map$dim[[1L]] * length(map_index)
  )
  source_support <- support_map$metadata$source_support %||%
    .ngeo_support_vector(x)
  target_support <- numeric(support_map$dim[[1L]])
  column_sum <- numeric(support_map$dim[[2L]])
  for (i in seq_along(support_map$row_groups)) {
    rows <- support_map$row_groups[[i]]
    for (j in seq_along(support_map$column_groups)) {
      columns <- support_map$column_groups[[j]]
      block <- support_map$blocks[[i]][[j]]
      target_support[rows] <- target_support[rows] +
        as.numeric(block %*% source_support[columns])
      column_sum[columns] <- column_sum[columns] + Matrix::colSums(block)
    }
  }
  scale <- rep.int(1, length(column_sum))
  nonunit <- column_sum > 1e-10 & abs(column_sum - 1) > 1e-10
  if (any(column_sum <= 1e-10) && unmapped == "error") {
    .ngeo_abort("Support map leaves source elements unmapped.",
                "ngeo_error_support_map")
  }
  if (any(nonunit)) {
    if (allocation == "error") {
      scale[nonunit] <- NA_real_
    } else {
      scale[column_sum > 1e-10] <- 1 / column_sum[column_sum > 1e-10]
    }
  }
  result <- matrix(0, support_map$dim[[1L]], length(map_index))
  for (m in seq_along(map_index)) {
    semantics <- x$measures$spatial_semantics[[map_index[[m]]]]
    if (semantics == "unknown") {
      if (unknown == "error") {
        .ngeo_abort("Declare unknown measurement semantics.",
                    "ngeo_error_measure")
      }
      semantics <- unknown
    }
    if (semantics == "categorical") {
      .ngeo_abort("Categorical uncertainty needs a probability model.",
                  "ngeo_error_uncertainty")
    }
    if (semantics != "intensive" && any(nonunit) && allocation == "error") {
      .ngeo_abort("Conservative allocation requires unit column sums.",
                  "ngeo_error_conservation")
    }
    for (i in seq_along(support_map$row_groups)) {
      rows <- support_map$row_groups[[i]]
      for (j in seq_along(support_map$column_groups)) {
        columns <- support_map$column_groups[[j]]
        block <- support_map$blocks[[i]][[j]]
        coefficient <- if (semantics == "intensive") {
          block %*% Matrix::Diagonal(x = source_support[columns])
        } else {
          block %*% Matrix::Diagonal(x = scale[columns])
        }
        if (semantics == "intensive") {
          coefficient <- Matrix::Diagonal(
            x = 1 / target_support[rows]
          ) %*% coefficient
        }
        result[rows, m] <- result[rows, m] +
          as.numeric((coefficient^2) %*% value_variance[columns, m])
      }
    }
  }
  result[!is.finite(result)] <- NA_real_
  colnames(result) <- x$maps$name[map_index]
  result
}

#' Compose compatible block support maps
#'
#' @param first Source-to-intermediate block map.
#' @param second Intermediate-to-target block map.
#' @param budget Resource budget.
#'
#' @return An `ngeo_block_support_map`.
#' @export
ngeo_compose_block_support_map <- function(
    first,
    second,
    budget = ngeo_resource_budget()) {
  ngeo_validate_block_support_map(first)
  ngeo_validate_block_support_map(second)
  if (!identical(first$metadata$target_domain_hash,
                 second$metadata$source_domain_hash) ||
      !identical(first$target_element_id, second$source_element_id) ||
      !identical(first$row_groups, second$column_groups)) {
    .ngeo_abort(
      "Block composition requires the same intermediate partition.",
      "ngeo_error_domain_mismatch"
    )
  }
  if (!is.null(first$variance_blocks) ||
      !is.null(second$variance_blocks)) {
    .ngeo_abort("Uncertain block composition needs covariance.",
                "ngeo_error_uncertainty")
  }
  count <- length(second$row_groups) * length(first$column_groups)
  .ngeo_budget_assert(budget, "blocks", count)
  .ngeo_budget_assert(
    budget,
    "materialized_elements",
    prod(as.double(c(second$dim[[1L]], first$dim[[2L]])))
  )
  blocks <- lapply(seq_along(second$row_groups), function(i) {
    lapply(seq_along(first$column_groups), function(j) {
      accumulated <- NULL
      for (k in seq_along(first$row_groups)) {
        product <- second$blocks[[i]][[k]] %*% first$blocks[[k]][[j]]
        accumulated <- if (is.null(accumulated)) product else
          accumulated + product
      }
      methods::as(accumulated, "dgCMatrix")
    })
  })
  operator <- methods::as(
    do.call(rbind, lapply(blocks, function(row) do.call(cbind, row))),
    "dgCMatrix"
  )
  column_sum <- Matrix::colSums(operator)
  column_nnz <- diff(operator@p)
  map <- .ngeo_support_map_structure(
    operator = operator,
    type = if (
      all(abs(operator@x - 1) <= 1e-10) &&
        all(column_nnz <= 1L)
    ) {
      "crisp"
    } else if (all(column_sum <= 1 + 1e-10)) {
      "probabilistic"
    } else {
      "overlapping"
    },
    source_hash = first$metadata$source_domain_hash,
    target_hash = second$metadata$target_domain_hash,
    source_id = first$source_element_id,
    target_id = second$target_element_id,
    source_support = first$metadata$source_support,
    target_support = if (is.null(first$metadata$source_support)) NULL else
      as.numeric(operator %*% first$metadata$source_support),
    weight_variance = NULL,
    coverage = if (
      first$metadata$coverage == "complete" &&
        second$metadata$coverage == "complete"
    ) "complete" else "partial",
    provenance = list(operations = list(.ngeo_operation(
      "ngeo_compose_block_support_map",
      list(first = first$logical_hash, second = second$logical_hash)
    )))
  )
  result <- ngeo_block_support_map(
    map,
    row_block_size = max(vapply(second$row_groups, length, integer(1))),
    source_block_size = max(vapply(first$column_groups, length, integer(1)))
  )
  result$blocks <- blocks
  result$block_hash <- .ngeo_block_content_hash(result)
  result
}

#' @export
print.ngeo_block_support_diagnostics <- function(x, ...) {
  cat("<ngeo_block_support_diagnostics>\n  blocks: ", x$blocks,
      "\n  nonzero: ", x$nonzero,
      "\n  materialized: ", x$materialized_operator, "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_block_support_map <- function(x, ...) {
  cat("<ngeo_block_support_map>\n  dimensions: ",
      paste(x$dim, collapse = " x "), "\n  blocks: ",
      length(x$row_groups) * length(x$column_groups),
      "\n  logical hash: ", x$logical_hash, "\n", sep = "")
  invisible(x)
}
