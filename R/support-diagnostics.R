.ngeo_support_entropy_raw <- function(x) {
  column_sum <- Matrix::colSums(x$operator)
  entries <- Matrix::summary(x$operator)
  result <- rep.int(NA_real_, ncol(x$operator))
  covered <- column_sum > 0
  result[covered] <- 0
  if (nrow(entries)) {
    probability <- entries$x / column_sum[entries$j]
    contribution <- -probability * log(probability)
    grouped <- rowsum(
      contribution,
      entries$j,
      reorder = FALSE
    )
    result[as.integer(rownames(grouped))] <- grouped[, 1L]
  }
  result
}

#' Compute source-wise support-map entropy
#'
#' Entropy describes ambiguity among target memberships without converting
#' the sparse operator to a dense matrix.
#'
#' @param x An `ngeo_support_map`.
#' @param normalized Divide by the maximum entropy over all target elements.
#'
#' @return A numeric vector aligned with source elements. Unmapped elements
#'   are `NA`.
#' @export
ngeo_support_entropy <- function(x, normalized = TRUE) {
  ngeo_validate_support_map(x)
  if (!is.logical(normalized) || length(normalized) != 1L ||
      is.na(normalized)) {
    .ngeo_abort(
      "`normalized` must be TRUE or FALSE.",
      "ngeo_error_argument"
    )
  }
  result <- .ngeo_support_entropy_raw(x)
  if (isTRUE(normalized)) {
    denominator <- log(nrow(x$operator))
    if (denominator > 0) {
      result <- result / denominator
    } else {
      result[!is.na(result)] <- 0
    }
  }
  pmin(1, pmax(0, result))
}

#' Diagnose a sparse support map
#'
#' @param x An `ngeo_support_map`.
#' @param tolerance Numerical conservation tolerance.
#' @param source_structure Optional source-aligned structure labels for
#'   coverage summaries.
#' @param condition Whether to compute bounded sparse conditioning.
#'
#' @return An `ngeo_support_diagnostics` object with sparse summaries and
#'   source/target tables.
#' @export
ngeo_support_diagnostics <- function(
    x,
    tolerance = 1e-10,
    source_structure = NULL,
    condition = TRUE) {
  ngeo_validate_support_map(x, tolerance)
  if (!is.null(source_structure) &&
      (!is.atomic(source_structure) ||
        length(source_structure) != ncol(x$operator) ||
        anyNA(source_structure))) {
    .ngeo_abort(
      "`source_structure` must be source-aligned and non-missing.",
      "ngeo_error_alignment"
    )
  }
  if (!is.logical(condition) || length(condition) != 1L ||
      is.na(condition)) {
    .ngeo_abort(
      "`condition` must be TRUE or FALSE.",
      "ngeo_error_argument"
    )
  }
  column_sum <- Matrix::colSums(x$operator)
  entries <- Matrix::summary(x$operator)
  source_nnz <- tabulate(entries$j, nbins = ncol(x$operator))
  target_nnz <- tabulate(entries$i, nbins = nrow(x$operator))
  target_weight <- Matrix::rowSums(x$operator)
  entropy_raw <- .ngeo_support_entropy_raw(x)
  entropy <- ngeo_support_entropy(x)
  covered <- column_sum > tolerance
  source_support <- x$source_support %||%
    rep.int(1, ncol(x$operator))
  support_total <- sum(source_support)
  mapped_support <- sum(source_support[covered])
  unit_error <- abs(column_sum - 1)
  mapped_error <- unit_error[covered]
  target_support <- x$target_support %||%
    as.numeric(x$operator %*% source_support)
  entropy_quantile <- if (any(covered)) {
    stats::quantile(
      entropy[covered],
      probs = c(0.25, 0.5, 0.75, 0.95),
      names = FALSE
    )
  } else {
    rep.int(NA_real_, 4L)
  }
  uncertainty_total <- if (is.null(x$weight_variance)) {
    0
  } else {
    sum(x$weight_variance@x)
  }
  source_table <- data.frame(
    element_id = x$source_element_id,
    membership_sum = as.numeric(column_sum),
    membership_count = source_nnz,
    covered = covered,
    entropy = entropy,
    effective_membership = exp(entropy_raw),
    support = source_support,
    stringsAsFactors = FALSE
  )
  target_table <- data.frame(
    element_id = x$target_element_id,
    membership_sum = as.numeric(target_weight),
    contributing_sources = target_nnz,
    covered = target_nnz > 0L,
    mapped_support = target_support,
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    metric = c(
      "source_elements",
      "target_elements",
      "nonzero",
      "density",
      "source_coverage_fraction",
      "source_support_coverage_fraction",
      "target_coverage_fraction",
      "maximum_unit_column_error",
      "mapped_unit_column_error",
      "mean_normalized_entropy",
      "maximum_normalized_entropy",
      "entropy_q25",
      "entropy_median",
      "entropy_q75",
      "entropy_q95",
      "mean_effective_target_count",
      "uncertain_nonzero",
      "operator_variance_total"
    ),
    value = c(
      ncol(x$operator),
      nrow(x$operator),
      length(x$operator@x),
      length(x$operator@x) /
        (as.double(nrow(x$operator)) * as.double(ncol(x$operator))),
      mean(covered),
      if (support_total > 0) mapped_support / support_total else NA_real_,
      mean(target_nnz > 0L),
      if (length(unit_error)) max(unit_error) else 0,
      if (length(mapped_error)) max(mapped_error) else NA_real_,
      if (any(covered)) mean(entropy[covered]) else NA_real_,
      if (any(covered)) max(entropy[covered]) else NA_real_,
      entropy_quantile,
      if (any(covered)) {
        mean(exp(entropy_raw[covered]))
      } else {
        NA_real_
      },
      if (is.null(x$weight_variance)) {
        0
      } else {
        length(x$weight_variance@x)
      },
      uncertainty_total
    ),
    stringsAsFactors = FALSE
  )
  coverage_by_structure <- NULL
  if (!is.null(source_structure)) {
    structure <- as.character(source_structure)
    split_rows <- split(
      seq_along(structure),
      factor(structure, levels = unique(structure))
    )
    coverage_by_structure <- do.call(rbind, lapply(
      names(split_rows),
      function(name) {
        rows <- split_rows[[name]]
        data.frame(
          structure = name,
          source_elements = length(rows),
          covered_elements = sum(covered[rows]),
          element_coverage_fraction = mean(covered[rows]),
          source_support = sum(source_support[rows]),
          covered_support = sum(source_support[rows][covered[rows]]),
          support_coverage_fraction =
            sum(source_support[rows][covered[rows]]) /
              sum(source_support[rows]),
          stringsAsFactors = FALSE
        )
      }
    ))
    rownames(coverage_by_structure) <- NULL
  }
  conditioning <- if (isTRUE(condition)) {
    ngeo_support_condition(x, tolerance)
  } else {
    NULL
  }
  result <- list(
    summary = summary,
    source = source_table,
    target = target_table,
    coverage_by_structure = coverage_by_structure,
    conditioning = conditioning,
    conservative = all(unit_error <= tolerance),
    complete = all(covered),
    tolerance = tolerance,
    type = x$type,
    coverage = x$coverage,
    support_map_hash = ngeo_support_map_hash(x)
  )
  class(result) <- "ngeo_support_diagnostics"
  result
}

#' @export
print.ngeo_support_diagnostics <- function(x, ...) {
  value <- stats::setNames(x$summary$value, x$summary$metric)
  cat(
    "<ngeo_support_diagnostics>\n",
    "  type: ", x$type, "\n",
    "  dimensions: ", format(value[["target_elements"]]), " target x ",
    format(value[["source_elements"]]), " source\n",
    "  nonzero: ", format(value[["nonzero"]], big.mark = ","), "\n",
    "  source coverage: ",
    format(100 * value[["source_coverage_fraction"]], digits = 4L),
    "%\n",
    "  conservative: ", x$conservative, "\n",
    sep = ""
  )
  invisible(x)
}

#' Plot support-map diagnostics
#'
#' @param x An `ngeo_support_diagnostics`.
#' @param type Membership-sum, entropy, or target-support plot.
#' @param ... Graphical parameters.
#'
#' @return `x`, invisibly.
#' @export
plot.ngeo_support_diagnostics <- function(
    x,
    type = c("membership", "entropy", "target_support"),
    ...) {
  type <- match.arg(type)
  if (!inherits(x, "ngeo_support_diagnostics")) {
    .ngeo_abort(
      "`x` must be `ngeo_support_diagnostics`.",
      "ngeo_error_argument"
    )
  }
  switch(
    type,
    membership = graphics::hist(
      x$source$membership_sum,
      main = "Source membership sums",
      xlab = "Column sum",
      ...
    ),
    entropy = graphics::hist(
      x$source$entropy[is.finite(x$source$entropy)],
      main = "Normalized support entropy",
      xlab = "Entropy",
      ...
    ),
    target_support = graphics::plot(
      seq_len(nrow(x$target)),
      x$target$mapped_support,
      type = "h",
      main = "Mapped target support",
      xlab = "Target element",
      ylab = "Support",
      ...
    )
  )
  invisible(x)
}

#' Plot a sparse support map
#'
#' This method plots bounded summaries and never draws the full operator.
#'
#' @param x An `ngeo_support_map`.
#' @param ... Passed to `plot.ngeo_support_diagnostics()`.
#'
#' @return `x`, invisibly.
#' @export
plot.ngeo_support_map <- function(x, ...) {
  diagnostics <- ngeo_support_diagnostics(x)
  plot(diagnostics, ...)
  invisible(x)
}

#' Sample uncertain support operators
#'
#' Independent Gaussian entry perturbations use the stored
#' `weight_variance`, are truncated at zero, and retain the original sparse
#' pattern. Column normalization is explicit.
#'
#' @param x An uncertain `ngeo_support_map`.
#' @param nsim Number of sampled operators.
#' @param seed Reproducible seed.
#' @param normalization Column-normalize each non-empty source membership or
#'   leave sampled weights unchanged.
#' @param tolerance Numerical mapping tolerance.
#'
#' @return An `ngeo_support_ensemble`.
#' @export
ngeo_support_monte_carlo <- function(
    x,
    nsim = 100L,
    seed = NULL,
    normalization = c("column", "none"),
    tolerance = 1e-10) {
  ngeo_validate_support_map(x, tolerance)
  normalization <- match.arg(normalization)
  nsim <- .ngeo_nsim(nsim)
  maximum <- getOption("neurogeo.max_support_draws", 1000L)
  if (nsim > maximum) {
    .ngeo_abort(
      "Requested support draws exceed the configured limit.",
      "ngeo_error_resource"
    )
  }
  if (is.null(x$weight_variance)) {
    .ngeo_abort(
      "Monte Carlo support sampling requires stored `weight_variance`.",
      "ngeo_error_uncertainty"
    )
  }
  entries <- Matrix::summary(x$operator)
  variance <- as.numeric(
    x$weight_variance[cbind(entries$i, entries$j)]
  )
  samples <- .ngeo_with_seed(seed, function() {
    lapply(seq_len(nsim), function(...) {
      sampled <- pmax(
        0,
        stats::rnorm(
          nrow(entries),
          mean = entries$x,
          sd = sqrt(variance)
        )
      )
      operator <- Matrix::sparseMatrix(
        i = entries$i,
        j = entries$j,
        x = sampled,
        dims = dim(x$operator)
      )
      column_sum <- Matrix::colSums(operator)
      if (identical(normalization, "column")) {
        inverse <- numeric(length(column_sum))
        inverse[column_sum > tolerance] <-
          1 / column_sum[column_sum > tolerance]
        operator <- methods::as(
          operator %*% Matrix::Diagonal(x = inverse),
          "dgCMatrix"
        )
        column_sum <- Matrix::colSums(operator)
      }
      type <- if (
        all(abs(operator@x - 1) <= tolerance) &&
          all(diff(operator@p) <= 1L)
      ) {
        "crisp"
      } else if (all(column_sum <= 1 + tolerance)) {
        "probabilistic"
      } else {
        "overlapping"
      }
      coverage <- if (identical(type, "overlapping")) {
        if (all(column_sum > tolerance)) "complete" else "partial"
      } else {
        if (all(abs(column_sum - 1) <= tolerance)) {
          "complete"
        } else {
          "partial"
        }
      }
      provenance <- x$provenance
      provenance$operations <- c(
        provenance$operations %||% list(),
        list(.ngeo_operation(
          "ngeo_support_monte_carlo",
          list(normalization = normalization)
        ))
      )
      .ngeo_support_map_structure(
        operator,
        type,
        x$source_domain_hash,
        x$target_domain_hash,
        x$source_element_id,
        x$target_element_id,
        x$source_support,
        if (is.null(x$source_support)) {
          NULL
        } else {
          as.numeric(operator %*% x$source_support)
        },
        NULL,
        coverage,
        provenance
      )
    })
  })
  result <- list(
    samples = samples,
    maps = samples,
    kind = "operator",
    weights = rep.int(1 / nsim, nsim),
    source_domain_hash = x$source_domain_hash,
    target_domain_hash = x$target_domain_hash,
    source_element_id = x$source_element_id,
    target_element_id = x$target_element_id,
    map_hashes = vapply(
      samples, ngeo_support_map_hash, character(1)
    ),
    source_support_map_hash = ngeo_support_map_hash(x),
    nsim = nsim,
    seed = .ngeo_seed(seed),
    normalization = normalization,
    assumptions = "independent Gaussian entry errors truncated at zero",
    provenance = list(operations = list(.ngeo_operation(
      "ngeo_support_monte_carlo",
      list(nsim = nsim, normalization = normalization)
    ))),
    spec_version = "2.2"
  )
  result$ensemble_hash <- .ngeo_support_ensemble_hash(
    samples, result$kind, result$weights
  )
  class(result) <- "ngeo_support_ensemble"
  ngeo_validate_support_ensemble(result)
  result
}

#' Compare results under alternative support operators
#'
#' @param x Source `ngeo` dataset.
#' @param target Shared target template or a list of identical target
#'   templates.
#' @param support_maps Two or more maps with common source and target domains.
#' @param maps Optional value-map selection.
#' @param reference Reference support-map index.
#' @param value_variance Optional source value variance propagated under each
#'   support map.
#' @param ... Passed to `ngeo_change_support()`.
#'
#' @return An `ngeo_support_sensitivity` object.
#' @export
ngeo_support_sensitivity <- function(
    x,
    target,
    support_maps,
    maps = NULL,
    reference = 1L,
    value_variance = NULL,
    ...) {
  ensemble <- NULL
  if (inherits(support_maps, "ngeo_support_ensemble")) {
    ngeo_validate_support_ensemble(support_maps)
    ensemble <- support_maps
    support_maps <- ensemble$maps %||% ensemble$samples
  }
  if (!is.list(support_maps) || length(support_maps) < 2L ||
      !all(vapply(
        support_maps,
        inherits,
        logical(1),
        what = "ngeo_support_map"
      ))) {
    .ngeo_abort(
      "`support_maps` must contain at least two support maps.",
      "ngeo_error_argument"
    )
  }
  if (inherits(target, "ngeo")) {
    targets <- rep(list(target), length(support_maps))
  } else {
    targets <- target
  }
  if (!is.list(targets) || length(targets) != length(support_maps)) {
    .ngeo_abort(
      "`target` must be shared or align with `support_maps`.",
      "ngeo_error_alignment"
    )
  }
  reference <- .ngeo_as_integer(reference, "reference")
  if (length(reference) != 1L ||
      reference < 1L || reference > length(support_maps)) {
    .ngeo_abort("`reference` is out of range.", "ngeo_error_argument")
  }
  source_hash <- vapply(
    support_maps,
    function(map) map$source_domain_hash,
    character(1)
  )
  target_hash <- vapply(
    support_maps,
    function(map) map$target_domain_hash,
    character(1)
  )
  if (length(unique(source_hash)) != 1L ||
      length(unique(target_hash)) != 1L) {
    .ngeo_abort(
      "Sensitivity comparison requires common source and target domains.",
      "ngeo_error_domain_mismatch"
    )
  }
  changed <- lapply(seq_along(support_maps), function(i) {
    ngeo_change_support(
      x,
      targets[[i]],
      support_maps[[i]],
      maps = maps,
      ...
    )
  })
  reference_values <- changed[[reference]]$values
  summaries <- do.call(rbind, lapply(seq_along(changed), function(i) {
    difference <- changed[[i]]$values - reference_values
    data.frame(
      support_map = i,
      map = colnames(difference),
      mean_difference = colMeans(difference, na.rm = TRUE),
      rmse = sqrt(colMeans(difference^2, na.rm = TRUE)),
      maximum_absolute_difference = apply(
        abs(difference), 2L, max, na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }))
  variance <- NULL
  if (!is.null(value_variance)) {
    variance <- lapply(seq_along(support_maps), function(i) {
      ngeo_support_variance(
        x,
        targets[[i]],
        support_maps[[i]],
        value_variance = value_variance,
        maps = maps,
        ...
      )
    })
  }
  ensemble_weights <- if (is.null(ensemble)) {
    rep.int(1 / length(support_maps), length(support_maps))
  } else {
    ensemble$weights
  }
  distribution <- do.call(rbind, lapply(
    seq_len(ncol(reference_values)),
    function(map_column) {
      value_matrix <- do.call(cbind, lapply(
        changed,
        function(dataset) dataset$values[, map_column]
      ))
      weighted_mean <- as.numeric(value_matrix %*% ensemble_weights)
      centered <- sweep(value_matrix, 1L, weighted_mean, "-")
      between <- as.numeric((centered^2) %*% ensemble_weights)
      lower <- apply(
        value_matrix, 1L, stats::quantile,
        probs = 0.025, names = FALSE
      )
      upper <- apply(
        value_matrix, 1L, stats::quantile,
        probs = 0.975, names = FALSE
      )
      within <- if (is.null(variance)) {
        rep.int(NA_real_, nrow(value_matrix))
      } else {
        variance_matrix <- do.call(cbind, lapply(
          variance,
          function(value) value[, map_column]
        ))
        as.numeric(variance_matrix %*% ensemble_weights)
      }
      data.frame(
        target_element_id =
          support_maps[[1L]]$target_element_id,
        map = colnames(reference_values)[[map_column]],
        ensemble_mean = weighted_mean,
        between_operator_variance = between,
        mean_within_variance = within,
        total_variance = between + ifelse(is.na(within), 0, within),
        lower = lower,
        upper = upper,
        stringsAsFactors = FALSE
      )
    }
  ))
  result <- list(
    summary = summaries,
    values = lapply(changed, `[[`, "values"),
    variance = variance,
    distribution = distribution,
    reference = reference,
    support_map_hashes = vapply(
      support_maps,
      ngeo_support_map_hash,
      character(1)
    ),
    source_domain_hash = source_hash[[1L]],
    target_domain_hash = target_hash[[1L]],
    ensemble_hash = if (is.null(ensemble)) {
      NULL
    } else {
      ensemble$ensemble_hash
    },
    ensemble_kind = if (is.null(ensemble)) {
      "alternative_maps"
    } else {
      ensemble$kind
    }
  )
  class(result) <- "ngeo_support_sensitivity"
  result
}

#' @export
print.ngeo_support_sensitivity <- function(x, ...) {
  cat(
    "<ngeo_support_sensitivity>\n",
    "  operators: ", length(x$support_map_hashes), "\n",
    "  reference: ", x$reference, "\n",
    "  maximum difference: ",
    format(max(x$summary$maximum_absolute_difference), digits = 5L),
    "\n",
    sep = ""
  )
  invisible(x)
}
