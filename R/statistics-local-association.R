.ngeo_getis_matrix <- function(input, star) {
  if (!isTRUE(star)) {
    return(input$matrix)
  }
  matrix <- input$raw_matrix
  diag(matrix) <- 1
  switch(
    input$normalization,
    W = .ngeo_row_standardize(matrix),
    B = .ngeo_binary(matrix),
    none = methods::as(matrix, "dgCMatrix")
  )
}

.ngeo_getis_values <- function(values, matrix, star) {
  n <- length(values)
  sum_w <- Matrix::rowSums(matrix)
  sum_w2 <- Matrix::rowSums(matrix^2)
  lag <- as.numeric(matrix %*% values)
  total <- sum(values)
  if (isTRUE(star)) {
    average <- rep.int(mean(values), n)
    variance <- rep.int(sum((values - mean(values))^2) / n, n)
    statistic <- lag / total
    expectation <- sum_w * average / total
    variance_statistic <- variance *
      ((n * sum_w2 - sum_w^2) / (n - 1)) / total^2
  } else {
    average <- (total - values) / (n - 1)
    variance <- ((sum(values^2) - values^2) / (n - 1)) -
      average^2
    denominator_total <- total - values
    statistic <- lag / denominator_total
    expectation <- sum_w * average / denominator_total
    variance_statistic <- variance *
      (((n - 1) * sum_w2 - sum_w^2) / (n - 2)) /
      denominator_total^2
  }
  list(
    statistic = statistic,
    expectation = expectation,
    variance = variance_statistic,
    z = (statistic - expectation) / sqrt(variance_statistic)
  )
}

.ngeo_getis_z <- function(values, matrix, star) {
  .ngeo_getis_values(values, matrix, star)$z
}

.ngeo_normal_p <- function(z, alternative) {
  switch(
    alternative,
    greater = stats::pnorm(z, lower.tail = FALSE),
    less = stats::pnorm(z),
    two.sided = 2 * stats::pnorm(-abs(z))
  )
}

.ngeo_extreme <- function(simulated, observed, alternative) {
  switch(
    alternative,
    greater = simulated >= observed,
    less = simulated <= observed,
    two.sided = abs(simulated) >= abs(observed)
  )
}

#' Local Getis-Ord Gi or Gi-star statistics
#'
#' @inheritParams ngeo_moran
#' @param star Include the focal element to compute Gi-star.
#'
#' @return An `ngeo_getis` data frame aligned to analysed elements.
#' @export
ngeo_getis_ord <- function(
    x,
    weights,
    map = 1L,
    star = TRUE,
    permutations = 0L,
    alternative = c("two.sided", "greater", "less"),
    seed = NULL,
    na_action = c("fail", "omit"),
    zero_policy = FALSE,
    adjust = "none",
    control = NULL) {
  if (!is.logical(star) || length(star) != 1L || is.na(star)) {
    .ngeo_abort(
      "`star` must be one non-missing logical value.",
      "ngeo_error_argument"
    )
  }
  na_action <- match.arg(na_action)
  inference <- .ngeo_resolve_permutation(
    control,
    permutations,
    seed,
    alternative,
    adjust
  )
  input <- .ngeo_spatial_inputs(
    x,
    weights,
    map,
    na_action,
    zero_policy
  )
  matrix <- .ngeo_getis_matrix(input, star)
  getis <- .ngeo_getis_values(input$values, matrix, star)
  observed <- getis$z
  permutations <- inference$permutations
  exceed <- integer(length(observed))
  valid <- is.finite(observed)
  .ngeo_with_seed(inference$seed, function() {
    if (permutations) {
      for (i in seq_len(permutations)) {
        simulated <- .ngeo_getis_z(sample(input$values), matrix, star)
        extreme <- .ngeo_extreme(
          simulated,
          observed,
          inference$alternative
        )
        exceed[valid] <<- exceed[valid] + extreme[valid]
      }
    }
    invisible(NULL)
  })
  p_value <- if (permutations) {
    result <- rep.int(NA_real_, length(observed))
    result[valid] <- (exceed[valid] + 1) / (permutations + 1)
    result
  } else {
    .ngeo_normal_p(observed, inference$alternative)
  }
  result <- data.frame(
    element_id = input$element_id,
    value = input$values,
    getis_ord = getis$statistic,
    expectation = getis$expectation,
    variance = getis$variance,
    z_score = observed,
    p.value = p_value,
    p.adjusted = stats::p.adjust(p_value, method = inference$adjust),
    stringsAsFactors = FALSE
  )
  attr(result, "statistic") <- if (star) "Gi*" else "Gi"
  attr(result, "map_id") <- input$map_id
  attr(result, "map_name") <- input$map_name
  attr(result, "domain_hash") <- input$domain_hash
  attr(result, "weights_method") <- input$weights_method
  attr(result, "normalization") <- input$normalization
  attr(result, "permutations") <- permutations
  attr(result, "alternative") <- inference$alternative
  attr(result, "adjustment") <- inference$adjust
  attr(result, "seed") <- inference$seed
  class(result) <- c("ngeo_getis", "data.frame")
  result
}

.ngeo_validate_lags <- function(lags) {
  if (!is.numeric(lags) || !length(lags) || anyNA(lags) ||
      any(!is.finite(lags)) || any(lags < 1) ||
      any(lags != floor(lags))) {
    .ngeo_abort(
      "`lags` must contain positive integer orders.",
      "ngeo_error_argument"
    )
  }
  lags <- sort(unique(as.integer(lags)))
  maximum <- getOption("neurogeo.max_correlogram_lag", 50L)
  if (max(lags) > maximum) {
    .ngeo_abort(
      sprintf("Requested lag exceeds the configured limit of %d.", maximum),
      "ngeo_error_resource"
    )
  }
  lags
}

.ngeo_lag_matrices <- function(matrix, lags) {
  adjacency <- .ngeo_binary(matrix)
  n <- nrow(adjacency)
  seen <- .ngeo_sparse_directed(n, seq_len(n), seq_len(n))
  frontier <- adjacency
  requested <- max(lags)
  result <- vector("list", length(lags))
  names(result) <- as.character(lags)
  limit <- getOption("neurogeo.max_correlogram_edges", 5000000L)
  for (order in seq_len(requested)) {
    candidate <- .ngeo_binary(frontier)
    current <- Matrix::drop0(candidate - candidate * seen)
    diag(current) <- 0
    if (length(current@x) > limit) {
      .ngeo_abort(
        "Correlogram lag expansion exceeds the configured sparse edge limit.",
        "ngeo_error_resource"
      )
    }
    if (order %in% lags) {
      result[[as.character(order)]] <- .ngeo_row_standardize(current)
    }
    seen <- .ngeo_binary(seen + current)
    frontier <- current %*% adjacency
  }
  result
}

#' Spatial autocorrelation correlogram
#'
#' Computes Moran's I over exact-order sparse graph lags. Previously reached
#' element pairs are excluded from later orders.
#'
#' @inheritParams ngeo_moran
#' @param lags Positive graph-lag orders.
#'
#' @return An `ngeo_correlogram` data frame.
#' @export
ngeo_correlogram <- function(
    x,
    weights,
    map = 1L,
    lags = 1:10,
    permutations = 0L,
    alternative = c("two.sided", "greater", "less"),
    seed = NULL,
    na_action = c("fail", "omit"),
    zero_policy = TRUE,
    adjust = "none",
    control = NULL) {
  na_action <- match.arg(na_action)
  lags <- .ngeo_validate_lags(lags)
  inference <- .ngeo_resolve_permutation(
    control,
    permutations,
    seed,
    alternative,
    adjust
  )
  input <- .ngeo_spatial_inputs(
    x,
    weights,
    map,
    na_action,
    zero_policy
  )
  matrices <- .ngeo_lag_matrices(input$raw_matrix, lags)
  expectation <- -1 / (length(input$values) - 1)
  observed <- vapply(matrices, function(matrix) {
    if (sum(matrix) == 0) {
      return(NA_real_)
    }
    .ngeo_moran_value(input$values, matrix)
  }, numeric(1))
  exceed <- integer(length(observed))
  valid <- is.finite(observed)
  .ngeo_with_seed(inference$seed, function() {
    if (inference$permutations) {
      for (i in seq_len(inference$permutations)) {
        values <- sample(input$values)
        simulated <- vapply(matrices, function(matrix) {
          if (sum(matrix) == 0) {
            return(NA_real_)
          }
          .ngeo_moran_value(values, matrix)
        }, numeric(1))
        extreme <- .ngeo_extreme(
          simulated - expectation,
          observed - expectation,
          inference$alternative
        )
        exceed[valid] <<- exceed[valid] + extreme[valid]
      }
    }
    invisible(NULL)
  })
  p_value <- rep.int(NA_real_, length(observed))
  if (inference$permutations) {
    p_value[valid] <- (exceed[valid] + 1) /
      (inference$permutations + 1)
  }
  result <- data.frame(
    lag = lags,
    moran_i = observed,
    expectation = rep.int(expectation, length(lags)),
    p.value = p_value,
    p.adjusted = stats::p.adjust(p_value, method = inference$adjust),
    n_edges = vapply(
      matrices,
      function(matrix) length(matrix@x),
      integer(1)
    ),
    n_isolated = vapply(
      matrices,
      function(matrix) sum(Matrix::rowSums(abs(matrix)) == 0),
      integer(1)
    ),
    stringsAsFactors = FALSE
  )
  attr(result, "map_id") <- input$map_id
  attr(result, "map_name") <- input$map_name
  attr(result, "domain_hash") <- input$domain_hash
  attr(result, "permutations") <- inference$permutations
  attr(result, "alternative") <- inference$alternative
  attr(result, "adjustment") <- inference$adjust
  attr(result, "seed") <- inference$seed
  class(result) <- c("ngeo_correlogram", "data.frame")
  result
}

#' @export
print.ngeo_getis <- function(x, ...) {
  cat(
    "<ngeo_getis>\n",
    "  statistic: ", attr(x, "statistic"), "\n",
    "  observations: ", nrow(x), "\n",
    "  map: ", attr(x, "map_name"), "\n",
    "  permutations: ", attr(x, "permutations"), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_correlogram <- function(x, ...) {
  cat(
    "<ngeo_correlogram>\n",
    "  lags: ", nrow(x), "\n",
    "  map: ", attr(x, "map_name"), "\n",
    "  permutations: ", attr(x, "permutations"), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
plot.ngeo_getis <- function(x, ...) {
  graphics::plot(
    seq_len(nrow(x)),
    x$z_score,
    pch = 19,
    xlab = "Analysed element",
    ylab = paste(attr(x, "statistic"), "z-score"),
    main = paste(attr(x, "statistic"), "-", attr(x, "map_name")),
    ...
  )
  graphics::abline(h = 0, col = "grey70", lty = 2)
  invisible(x)
}

#' @export
plot.ngeo_correlogram <- function(x, ...) {
  graphics::plot(
    x$lag,
    x$moran_i,
    type = "b",
    xlab = "Graph lag",
    ylab = "Moran's I",
    main = paste("Spatial correlogram -", attr(x, "map_name")),
    ...
  )
  graphics::abline(h = x$expectation[[1L]], col = "grey70", lty = 2)
  invisible(x)
}
