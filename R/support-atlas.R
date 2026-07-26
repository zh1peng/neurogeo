.ngeo_support_source <- function(first, second, source_support = NULL) {
  ngeo_validate_support_map(first)
  ngeo_validate_support_map(second)
  if (!identical(
    first$source_domain_hash,
    second$source_domain_hash
  ) || !identical(
    first$source_element_id,
    second$source_element_id
  )) {
    .ngeo_abort(
      "Atlas comparison requires a common source domain.",
      "ngeo_error_domain_mismatch"
    )
  }
  support <- source_support %||% first$source_support %||%
    second$source_support
  if (!is.numeric(support) ||
      length(support) != ncol(first$operator) ||
      anyNA(support) || any(!is.finite(support)) ||
      any(support <= 0)) {
    .ngeo_abort(
      "Atlas comparison requires positive common source support.",
      "ngeo_error_support"
    )
  }
  as.numeric(support)
}

#' Compute cross-atlas support overlap
#'
#' @param first First base-to-atlas support map.
#' @param second Second base-to-atlas support map.
#' @param source_support Optional common base support.
#' @param metric Intersection, Jaccard, or Dice support.
#'
#' @return A sparse first-atlas by second-atlas matrix.
#' @export
ngeo_atlas_overlap <- function(
    first,
    second,
    source_support = NULL,
    metric = c("intersection", "jaccard", "dice")) {
  metric <- match.arg(metric)
  support <- .ngeo_support_source(first, second, source_support)
  intersection <- methods::as(
    first$operator %*% Matrix::Diagonal(x = support) %*%
      Matrix::t(second$operator),
    "dgCMatrix"
  )
  dimnames(intersection) <- list(
    first$target_element_id,
    second$target_element_id
  )
  if (identical(metric, "intersection")) {
    return(intersection)
  }
  first_size <- as.numeric(first$operator %*% support)
  second_size <- as.numeric(second$operator %*% support)
  entries <- Matrix::summary(intersection)
  denominator <- if (identical(metric, "jaccard")) {
    first_size[entries$i] + second_size[entries$j] - entries$x
  } else {
    first_size[entries$i] + second_size[entries$j]
  }
  entries$x <- if (identical(metric, "jaccard")) {
    entries$x / denominator
  } else {
    2 * entries$x / denominator
  }
  Matrix::sparseMatrix(
    i = entries$i,
    j = entries$j,
    x = entries$x,
    dims = dim(intersection),
    dimnames = dimnames(intersection)
  )
}

#' Summarize best cross-atlas matches
#'
#' @inheritParams ngeo_atlas_overlap
#'
#' @return A data frame with best Dice and Jaccard matches.
#' @export
ngeo_atlas_compare <- function(first, second, source_support = NULL) {
  pair_count <- nrow(first$operator) * nrow(second$operator)
  maximum <- getOption("neurogeo.max_atlas_comparison_pairs", 1e6)
  if (pair_count > maximum) {
    .ngeo_abort(
      "Dense best-match comparison exceeds the configured atlas-pair limit.",
      "ngeo_error_resource"
    )
  }
  intersection <- ngeo_atlas_overlap(
    first, second, source_support, "intersection"
  )
  dice <- ngeo_atlas_overlap(first, second, source_support, "dice")
  jaccard <- ngeo_atlas_overlap(
    first, second, source_support, "jaccard"
  )
  dense_dice <- as.matrix(dice)
  best <- max.col(dense_dice, ties.method = "first")
  data.frame(
    source_region = first$target_element_id,
    target_region = second$target_element_id[best],
    intersection = as.numeric(
      intersection[cbind(seq_len(nrow(intersection)), best)]
    ),
    dice = dense_dice[cbind(seq_len(nrow(dense_dice)), best)],
    jaccard = as.numeric(
      jaccard[cbind(seq_len(nrow(jaccard)), best)]
    ),
    stringsAsFactors = FALSE
  )
}

#' Transfer parcel values under a declared piecewise-constant model
#'
#' This is a model-based atlas transfer, not an inverse reconstruction.
#'
#' @param values First-atlas region-by-map values.
#' @param first Base-to-first-atlas support map.
#' @param second Base-to-second-atlas support map.
#' @param semantics Intensive or extensive values.
#' @param value_variance Optional first-atlas independent variances.
#' @param source_support Optional common base support.
#' @param model Must be `"piecewise_constant"`.
#'
#' @return An `ngeo_cross_atlas` list with transfer operator and uncertainty.
#' @export
ngeo_cross_atlas <- function(
    values,
    first,
    second,
    semantics = c("intensive", "extensive"),
    value_variance = NULL,
    source_support = NULL,
    model = "piecewise_constant") {
  semantics <- match.arg(semantics)
  if (!identical(model, "piecewise_constant")) {
    .ngeo_abort(
      "Only the explicit piecewise-constant atlas model is implemented.",
      "ngeo_error_model"
    )
  }
  first_sum <- Matrix::colSums(first$operator)
  second_sum <- Matrix::colSums(second$operator)
  if (any(abs(first_sum - 1) > 1e-10) ||
      any(abs(second_sum - 1) > 1e-10)) {
    .ngeo_abort(
      "Piecewise-constant transfer requires complete unit-allocation atlases.",
      "ngeo_error_conservation"
    )
  }
  if (is.atomic(values) && is.null(dim(values))) {
    values <- matrix(values, ncol = 1L)
  }
  if (!is.matrix(values) || nrow(values) != nrow(first$operator) ||
      any(!is.finite(values))) {
    .ngeo_abort(
      "`values` must be a finite first-atlas region-by-map matrix.",
      "ngeo_error_alignment"
    )
  }
  intersection <- ngeo_atlas_overlap(
    first,
    second,
    source_support,
    "intersection"
  )
  transfer <- Matrix::t(intersection)
  totals <- if (identical(semantics, "intensive")) {
    Matrix::rowSums(transfer)
  } else {
    Matrix::colSums(transfer)
  }
  inverse <- numeric(length(totals))
  inverse[totals > 0] <- 1 / totals[totals > 0]
  transfer <- if (identical(semantics, "intensive")) {
    Matrix::Diagonal(x = inverse) %*% transfer
  } else {
    transfer %*% Matrix::Diagonal(x = inverse)
  }
  transferred <- as.matrix(transfer %*% values)
  variance <- NULL
  if (!is.null(value_variance)) {
    if (is.atomic(value_variance) && is.null(dim(value_variance))) {
      value_variance <- matrix(value_variance, ncol = 1L)
    }
    if (!is.matrix(value_variance) ||
        !identical(dim(value_variance), dim(values)) ||
        any(!is.finite(value_variance)) ||
        any(value_variance < 0)) {
      .ngeo_abort(
        "`value_variance` must align with atlas values.",
        "ngeo_error_uncertainty"
      )
    }
    variance <- as.matrix((transfer^2) %*% value_variance)
  }
  result <- list(
    values = transferred,
    variance = variance,
    operator = methods::as(transfer, "dgCMatrix"),
    model = model,
    semantics = semantics,
    source_atlas_hash = first$target_domain_hash,
    target_atlas_hash = second$target_domain_hash
  )
  class(result) <- "ngeo_cross_atlas"
  result
}

.ngeo_invariant_estimate <- function(values, support, semantics) {
  if (identical(semantics, "intensive")) {
    sum(values * support) / sum(support)
  } else {
    sum(values)
  }
}

#' Parcellation-invariant support inference
#'
#' Applies complete support maps and verifies that a global intensive mean or
#' extensive/count total is invariant. Confidence intervals use one shared
#' source-element bootstrap, independent of atlas.
#'
#' @param x Source `ngeo` dataset.
#' @param support_maps List of source-to-atlas support maps.
#' @param targets List of matching target templates.
#' @param map One map.
#' @param nsim Shared source bootstrap replicates.
#' @param seed Reproducible seed.
#' @param tolerance Maximum cross-parcellation deviation.
#' @param allocation Extensive overlap allocation policy.
#'
#' @return An `ngeo_parcellation_inference` object.
#' @export
ngeo_parcellation_inference <- function(
    x,
    support_maps,
    targets,
    map = 1L,
    nsim = 999L,
    seed = NULL,
    tolerance = 1e-10,
    allocation = c("error", "normalize")) {
  allocation <- match.arg(allocation)
  if (!is.list(support_maps) || !length(support_maps) ||
      !is.list(targets) ||
      length(targets) != length(support_maps)) {
    .ngeo_abort(
      "`support_maps` and `targets` must be aligned non-empty lists.",
      "ngeo_error_argument"
    )
  }
  map_index <- .ngeo_map_selection(x, map)
  if (length(map_index) != 1L || is.null(x$values)) {
    .ngeo_abort(
      "Inference requires one loaded map.",
      "ngeo_error_values"
    )
  }
  semantics <- x$measures$spatial_semantics[[map_index]]
  if (!semantics %in% c("intensive", "extensive", "count")) {
    .ngeo_abort(
      "Inference requires intensive, extensive, or count semantics.",
      "ngeo_error_measure"
    )
  }
  support <- support_maps[[1L]]$source_support %||%
    .ngeo_support_vector(x)
  for (current in support_maps) {
    ngeo_validate_support_map(current)
    if (!identical(current$coverage, "complete") ||
        !isTRUE(all.equal(
          current$source_support %||% support,
          support,
          tolerance = 1e-10,
          check.attributes = FALSE
        ))) {
      .ngeo_abort(
        "Invariant inference requires complete maps with common source support.",
        "ngeo_error_invariant"
      )
    }
  }
  source_values <- as.numeric(x$values[, map_index])
  source_estimate <- .ngeo_invariant_estimate(
    source_values,
    support,
    semantics
  )
  atlas_estimates <- vapply(seq_along(support_maps), function(i) {
    changed <- ngeo_change_support(
      x,
      targets[[i]],
      support_maps[[i]],
      maps = map_index,
      allocation = allocation
    )
    target_support <- as.numeric(
      support_maps[[i]]$operator %*% support
    )
    .ngeo_invariant_estimate(
      as.numeric(changed$values[, 1L]),
      target_support,
      semantics
    )
  }, numeric(1))
  deviation <- max(abs(atlas_estimates - source_estimate))
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || tolerance < 0 || deviation > tolerance) {
    .ngeo_abort(
      sprintf(
        "Parcellation invariant deviation %.6g exceeds tolerance %.6g.",
        deviation,
        tolerance
      ),
      "ngeo_error_invariant"
    )
  }
  bootstrap <- .ngeo_with_seed(seed, function() {
    vapply(seq_len(.ngeo_nsim(nsim)), function(...) {
      index <- sample(seq_along(source_values), replace = TRUE)
      .ngeo_invariant_estimate(
        source_values[index],
        support[index],
        semantics
      )
    }, numeric(1))
  })
  atlas_names <- names(support_maps)
  if (is.null(atlas_names) || any(!nzchar(atlas_names))) {
    atlas_names <- paste0("atlas_", seq_along(support_maps))
  }
  estimates <- data.frame(
    parcellation = c("source", atlas_names),
    estimate = c(source_estimate, atlas_estimates),
    stringsAsFactors = FALSE
  )
  result <- list(
    estimates = estimates,
    confidence_interval = stats::quantile(
      bootstrap,
      c(0.025, 0.975),
      names = FALSE
    ),
    bootstrap = bootstrap,
    statistic = if (identical(semantics, "intensive")) {
      "support_weighted_mean"
    } else {
      "total"
    },
    semantics = semantics,
    max_deviation = deviation,
    tolerance = tolerance,
    domain_hash = ngeo_domain_hash(x),
    support_map_hashes = vapply(
      support_maps,
      ngeo_support_map_hash,
      character(1)
    ),
    nsim = .ngeo_nsim(nsim),
    seed = .ngeo_seed(seed)
  )
  class(result) <- "ngeo_parcellation_inference"
  result
}

#' @export
print.ngeo_cross_atlas <- function(x, ...) {
  cat(
    "<ngeo_cross_atlas>\n",
    "  model: ", x$model, "\n",
    "  semantics: ", x$semantics, "\n",
    "  dimensions: ", nrow(x$values), " x ", ncol(x$values), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_parcellation_inference <- function(x, ...) {
  cat(
    "<ngeo_parcellation_inference>\n",
    "  statistic: ", x$statistic, "\n",
    "  parcellations: ", nrow(x$estimates) - 1L, "\n",
    "  max deviation: ", format(x$max_deviation, digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}
