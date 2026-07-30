.ngeo_spatial_inputs <- function(x,
                                 weights,
                                 map,
                                 na_action,
                                 zero_policy) {
  ngeo_validate(x, "basic")
  if (!inherits(weights, "ngeo_weights")) {
    .ngeo_abort(
      "`weights` must be an `ngeo_weights` object.",
      "ngeo_error_argument"
    )
  }
  if (!identical(weights$domain_hash, ngeo_domain_hash(x))) {
    .ngeo_abort(
      "Weights domain hash does not match the dataset.",
      "ngeo_error_domain_mismatch"
    )
  }
  if (is.null(x$values)) {
    .ngeo_abort(
      "Spatial statistics require loaded values.",
      "ngeo_error_values"
    )
  }
  map_index <- .ngeo_map_selection(x, map)
  if (length(map_index) != 1L) {
    .ngeo_abort(
      "`map` must select exactly one map.",
      "ngeo_error_argument"
    )
  }
  measure <- x$measures[map_index, , drop = FALSE]
  if (identical(measure$spatial_semantics[[1L]], "categorical")) {
    .ngeo_abort(
      "Categorical maps are not valid for this statistic.",
      "ngeo_error_measure"
    )
  }

  values <- as.numeric(x$values[, map_index])
  finite <- is.finite(values)
  if (identical(na_action, "fail") && !all(finite)) {
    .ngeo_abort(
      "Map values contain missing or non-finite values.",
      "ngeo_error_missing"
    )
  }
  index <- which(finite)
  if (length(index) < 3L) {
    .ngeo_abort(
      "At least three finite observations are required.",
      "ngeo_error_statistic"
    )
  }
  raw_matrix <- .ngeo_as_dgCMatrix(
    weights$raw_matrix[index, index, drop = FALSE]
  )
  matrix <- switch(
    weights$normalization,
    W = .ngeo_row_standardize(raw_matrix),
    B = .ngeo_binary(raw_matrix),
    none = raw_matrix
  )
  if (length(matrix@x) && any(!is.finite(matrix@x))) {
    .ngeo_abort(
      "Weights contain non-finite values.",
      "ngeo_error_weights"
    )
  }
  row_weight <- Matrix::rowSums(abs(matrix))
  isolated <- which(row_weight == 0)
  if (length(isolated) && !isTRUE(zero_policy)) {
    .ngeo_abort(
      "Weights contain isolated observations; set `zero_policy = TRUE` to retain them.",
      "ngeo_error_zero_policy"
    )
  }
  if (sum(matrix) == 0) {
    .ngeo_abort(
      "Weights have zero total weight.",
      "ngeo_error_weights"
    )
  }
  if (stats::var(values[index]) == 0) {
    .ngeo_abort(
      "The selected map has zero variance.",
      "ngeo_error_statistic"
    )
  }

  list(
    values = values[index],
    matrix = matrix,
    raw_matrix = raw_matrix,
    index = index,
    element_id = x$domain$elements$element_id[index],
    map_id = x$maps$map_id[[map_index]],
    map_name = x$maps$name[[map_index]],
    domain_hash = weights$domain_hash,
    weights_method = weights$method,
    normalization = weights$normalization,
    isolated = isolated
  )
}

.ngeo_permutations <- function(permutations) {
  if (!is.numeric(permutations) || length(permutations) != 1L ||
      is.na(permutations) || !is.finite(permutations) ||
      permutations < 0 || permutations != floor(permutations)) {
    .ngeo_abort(
      "`permutations` must be one non-negative integer.",
      "ngeo_error_argument"
    )
  }
  maximum <- getOption("neurogeo.max_permutations", 99999L)
  if (permutations > maximum) {
    .ngeo_abort(
      sprintf(
        "`permutations` exceeds the configured limit of %d.",
        maximum
      ),
      "ngeo_error_argument"
    )
  }
  as.integer(permutations)
}

.ngeo_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  if (!is.numeric(seed) || length(seed) != 1L ||
      is.na(seed) || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max ||
      seed != floor(seed)) {
    .ngeo_abort(
      "`seed` must be `NULL` or one non-negative integer.",
      "ngeo_error_argument"
    )
  }
  as.integer(seed)
}

.ngeo_adjustment <- function(adjust) {
  if (!is.character(adjust) || length(adjust) != 1L ||
      is.na(adjust) || !adjust %in% stats::p.adjust.methods) {
    .ngeo_abort(
      sprintf(
        "`adjust` must be one of: %s.",
        paste(stats::p.adjust.methods, collapse = ", ")
      ),
      "ngeo_error_argument"
    )
  }
  adjust
}

#' Configure permutation inference
#'
#' A control object provides one reproducible permutation, tail, and
#' multiple-testing policy across spatial statistics. When supplied to a
#' statistic, its fields override that function's corresponding scalar
#' arguments.
#'
#' @param permutations Number of Monte Carlo permutations.
#' @param seed Optional reproducible random seed.
#' @param alternative Test alternative.
#' @param adjust A method accepted by [stats::p.adjust()].
#'
#' @return An `ngeo_permutation_control` object.
#' @export
ngeo_permutation_control <- function(
    permutations = 999L,
    seed = NULL,
    alternative = c("two.sided", "greater", "less"),
    adjust = "none") {
  alternative <- match.arg(alternative)
  structure(
    list(
      permutations = .ngeo_permutations(permutations),
      seed = .ngeo_seed(seed),
      alternative = alternative,
      adjust = .ngeo_adjustment(adjust)
    ),
    class = "ngeo_permutation_control"
  )
}

.ngeo_resolve_permutation <- function(control,
                                      permutations,
                                      seed,
                                      alternative,
                                      adjust = "none") {
  if (!is.null(control)) {
    if (!inherits(control, "ngeo_permutation_control")) {
      .ngeo_abort(
        "`control` must be an `ngeo_permutation_control` object.",
        "ngeo_error_argument"
      )
    }
    return(control)
  }
  ngeo_permutation_control(
    permutations = permutations,
    seed = seed,
    alternative = alternative,
    adjust = adjust
  )
}

.ngeo_with_seed <- function(seed, code) {
  seed <- .ngeo_seed(seed)
  if (is.null(seed)) {
    return(code())
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  code()
}

.ngeo_permutation_p <- function(observed,
                                simulated,
                                expectation,
                                alternative) {
  if (!length(simulated)) {
    return(NA_real_)
  }
  extreme <- switch(
    alternative,
    greater = simulated >= observed,
    less = simulated <= observed,
    two.sided = abs(simulated - expectation) >=
      abs(observed - expectation)
  )
  (sum(extreme) + 1) / (length(simulated) + 1)
}

.ngeo_moran_value <- function(values, matrix) {
  centered <- values - mean(values)
  n <- length(centered)
  s0 <- sum(matrix)
  as.numeric(n / s0 * crossprod(centered, matrix %*% centered) /
    crossprod(centered))
}

.ngeo_geary_value <- function(values, matrix) {
  centered <- values - mean(values)
  entries <- Matrix::summary(matrix)
  numerator <- sum(
    entries$x * (values[entries$i] - values[entries$j])^2
  )
  (length(values) - 1) / (2 * sum(matrix)) *
    numerator / sum(centered^2)
}

.ngeo_global_statistic <- function(x,
                                   weights,
                                   map,
                                   permutations,
                                   alternative,
                                   seed,
                                   na_action,
                                   zero_policy,
                                   statistic,
                                   adjust,
                                   control) {
  na_action <- match.arg(na_action, c("fail", "omit"))
  inference <- .ngeo_resolve_permutation(
    control,
    permutations,
    seed,
    alternative,
    adjust
  )
  permutations <- inference$permutations
  alternative <- inference$alternative
  seed <- inference$seed
  input <- .ngeo_spatial_inputs(
    x,
    weights,
    map,
    na_action,
    zero_policy
  )
  value_function <- if (identical(statistic, "Moran's I")) {
    .ngeo_moran_value
  } else {
    .ngeo_geary_value
  }
  observed <- value_function(input$values, input$matrix)
  expectation <- if (identical(statistic, "Moran's I")) {
    -1 / (length(input$values) - 1)
  } else {
    1
  }
  simulated <- .ngeo_with_seed(seed, function() {
    if (!permutations) {
      return(numeric())
    }
    vapply(
      seq_len(permutations),
      function(...) {
        value_function(sample(input$values), input$matrix)
      },
      numeric(1)
    )
  })
  centered <- input$values - mean(input$values)
  p_value <- .ngeo_permutation_p(
    observed,
    simulated,
    expectation,
    alternative
  )
  result <- list(
    statistic = statistic,
    estimate = observed,
    expectation = expectation,
    p.value = p_value,
    p.adjusted = stats::p.adjust(p_value, method = inference$adjust),
    adjustment = inference$adjust,
    alternative = alternative,
    permutations = permutations,
    simulated = simulated,
    n = length(input$values),
    map_id = input$map_id,
    map_name = input$map_name,
    element_id = input$element_id,
    values = input$values,
    standardized = as.numeric(scale(input$values)),
    spatial_lag = as.numeric(input$matrix %*% centered),
    domain_hash = input$domain_hash,
    weights_method = input$weights_method,
    normalization = input$normalization,
    zero_policy = isTRUE(zero_policy),
    omitted = nrow(x$domain$elements) - length(input$values),
    seed = seed
  )
  class(result) <- "ngeo_global_stat"
  result
}

#' Global Moran's I
#'
#' @param x An `ngeo` dataset.
#' @param weights Matching `ngeo_weights`.
#' @param map One map name, ID, or index.
#' @param permutations Number of Monte Carlo permutations.
#' @param alternative Permutation-test alternative.
#' @param seed Optional reproducible random seed.
#' @param na_action Whether to fail or omit non-finite values. Omission
#'   rebuilds the declared normalization on the retained raw-weight subgraph.
#' @param zero_policy Whether to retain observations with no neighbours.
#' @param adjust A method accepted by [stats::p.adjust()].
#' @param control Optional [ngeo_permutation_control()] overriding permutation
#'   inference arguments.
#'
#' @return An `ngeo_global_stat` result.
#' @export
ngeo_moran <- function(x,
                       weights,
                       map = 1L,
                       permutations = 0L,
                       alternative = c("two.sided", "greater", "less"),
                       seed = NULL,
                       na_action = c("fail", "omit"),
                       zero_policy = FALSE,
                       adjust = "none",
                       control = NULL) {
  .ngeo_global_statistic(
    x,
    weights,
    map,
    permutations,
    alternative,
    seed,
    na_action,
    zero_policy,
    "Moran's I",
    adjust,
    control
  )
}

#' Global Geary's C
#'
#' @inheritParams ngeo_moran
#'
#' @return An `ngeo_global_stat` result.
#' @export
ngeo_geary <- function(x,
                       weights,
                       map = 1L,
                       permutations = 0L,
                       alternative = c("two.sided", "greater", "less"),
                       seed = NULL,
                       na_action = c("fail", "omit"),
                       zero_policy = FALSE,
                       adjust = "none",
                       control = NULL) {
  .ngeo_global_statistic(
    x,
    weights,
    map,
    permutations,
    alternative,
    seed,
    na_action,
    zero_policy,
    "Geary's C",
    adjust,
    control
  )
}

#' Local Moran statistics (LISA)
#'
#' @inheritParams ngeo_moran
#' @param null_model Conditional randomization with the focal value fixed, or
#'   total randomization of the complete centered vector.
#'
#' @return An `ngeo_lisa` data frame aligned to the analysed elements.
#' @export
ngeo_local_moran <- function(x,
                             weights,
                             map = 1L,
                             permutations = 0L,
                             alternative = c(
                               "two.sided", "greater", "less"
                             ),
                             seed = NULL,
                             na_action = c("fail", "omit"),
                             zero_policy = FALSE,
                             adjust = "none",
                             control = NULL,
                             null_model = c("conditional", "total")) {
  na_action <- match.arg(na_action)
  null_model <- match.arg(null_model)
  inference <- .ngeo_resolve_permutation(
    control,
    permutations,
    seed,
    alternative,
    adjust
  )
  permutations <- inference$permutations
  alternative <- inference$alternative
  seed <- inference$seed
  input <- .ngeo_spatial_inputs(
    x,
    weights,
    map,
    na_action,
    zero_policy
  )
  centered <- input$values - mean(input$values)
  m2 <- sum(centered^2) / length(centered)
  lag <- as.numeric(input$matrix %*% centered)
  observed <- centered * lag / m2
  row_weight <- Matrix::rowSums(input$matrix)
  expectation <- if (identical(null_model, "conditional")) {
    -(centered^2 / m2) * row_weight / (length(centered) - 1)
  } else {
    -row_weight / (length(centered) - 1)
  }
  exceed <- integer(length(observed))
  neighbor <- weight <- NULL
  if (permutations && identical(null_model, "conditional")) {
    neighbor <- lapply(seq_along(centered), function(i) {
      which(as.numeric(input$matrix[i, ]) != 0)
    })
    weight <- lapply(seq_along(centered), function(i) {
      as.numeric(input$matrix[i, neighbor[[i]], drop = TRUE])
    })
  }
  .ngeo_with_seed(seed, function() {
    if (permutations) {
      for (i in seq_len(permutations)) {
        simulated <- if (identical(null_model, "conditional")) {
          vapply(seq_along(centered), function(position) {
            current_neighbor <- neighbor[[position]]
            if (!length(current_neighbor)) return(0)
            pool <- centered[-position]
            sampled <- sample(
              pool, length(current_neighbor), replace = FALSE
            )
            centered[[position]] *
              sum(weight[[position]] * sampled) / m2
          }, numeric(1))
        } else {
          permuted <- sample(centered)
          permuted * as.numeric(input$matrix %*% permuted) / m2
        }
        extreme <- switch(
          alternative,
          greater = simulated >= observed,
          less = simulated <= observed,
          two.sided = abs(simulated - expectation) >=
            abs(observed - expectation)
        )
        exceed <<- exceed + extreme
      }
    }
    invisible(NULL)
  })
  p_value <- if (permutations) {
    (exceed + 1) / (permutations + 1)
  } else {
    rep.int(NA_real_, length(observed))
  }
  p_adjusted <- stats::p.adjust(p_value, method = inference$adjust)
  cluster <- ifelse(
    centered >= 0 & lag >= 0,
    "high-high",
    ifelse(
      centered < 0 & lag < 0,
      "low-low",
      ifelse(centered >= 0, "high-low", "low-high")
    )
  )
  result <- data.frame(
    element_id = input$element_id,
    value = input$values,
    centered = centered,
    spatial_lag = lag,
    local_i = observed,
    expectation = as.numeric(expectation),
    p.value = p_value,
    p.adjusted = p_adjusted,
    significant = if (permutations) {
      !is.na(p_adjusted) & p_adjusted <= 0.05
    } else {
      rep.int(NA, length(observed))
    },
    cluster = cluster,
    stringsAsFactors = FALSE
  )
  attr(result, "map_id") <- input$map_id
  attr(result, "map_name") <- input$map_name
  attr(result, "domain_hash") <- input$domain_hash
  attr(result, "weights_method") <- input$weights_method
  attr(result, "normalization") <- input$normalization
  attr(result, "permutations") <- permutations
  attr(result, "alternative") <- alternative
  attr(result, "adjustment") <- inference$adjust
  attr(result, "seed") <- seed
  attr(result, "null_model") <- null_model
  attr(result, "cluster_definition") <-
    "Moran quadrant; inspect `significant` for permutation evidence"
  class(result) <- c("ngeo_lisa", "data.frame")
  result
}

#' Empirical semivariogram
#'
#' @param x An `ngeo` dataset.
#' @param map One map name, ID, or index.
#' @param metric Explicit distance metric.
#' @param breaks Number of bins or a numeric vector of bin boundaries.
#' @param max_distance Optional maximum pair distance.
#' @param na_action Whether to fail or omit non-finite values.
#'
#' @return An `ngeo_variogram` data frame.
#' @export
ngeo_variogram <- function(x,
                           map = 1L,
                           metric = NULL,
                           breaks = 10L,
                           max_distance = Inf,
                           na_action = c("fail", "omit")) {
  ngeo_validate(x, "basic")
  na_action <- match.arg(na_action)
  if (is.null(x$values)) {
    .ngeo_abort(
      "A variogram requires loaded values.",
      "ngeo_error_values"
    )
  }
  map_index <- .ngeo_map_selection(x, map)
  if (length(map_index) != 1L) {
    .ngeo_abort(
      "`map` must select exactly one map.",
      "ngeo_error_argument"
    )
  }
  if (identical(
    x$measures$spatial_semantics[[map_index]],
    "categorical"
  )) {
    .ngeo_abort(
      "Categorical maps do not have a numeric semivariogram.",
      "ngeo_error_measure"
    )
  }
  values <- as.numeric(x$values[, map_index])
  finite <- is.finite(values)
  if (identical(na_action, "fail") && !all(finite)) {
    .ngeo_abort(
      "Map values contain missing or non-finite values.",
      "ngeo_error_missing"
    )
  }
  index <- which(finite)
  n <- length(index)
  if (n < 2L) {
    .ngeo_abort(
      "At least two finite observations are required.",
      "ngeo_error_statistic"
    )
  }
  pair_count <- n * (n - 1) / 2
  maximum <- getOption("neurogeo.max_variogram_pairs", 1e6)
  if (pair_count > maximum) {
    .ngeo_abort(
      sprintf(
        "Requested %s variogram pairs exceeds the configured limit of %s.",
        format(pair_count, big.mark = ","),
        format(maximum, big.mark = ",")
      ),
      "ngeo_error_dense_distance"
    )
  }
  if (!is.numeric(max_distance) || length(max_distance) != 1L ||
      is.na(max_distance) || max_distance <= 0) {
    .ngeo_abort(
      "`max_distance` must be one positive number.",
      "ngeo_error_argument"
    )
  }

  distance <- numeric(pair_count)
  semivariance <- numeric(pair_count)
  position <- 1L
  for (i in seq_len(n - 1L)) {
    targets <- index[seq.int(i + 1L, n)]
    count <- length(targets)
    range <- seq.int(position, length.out = count)
    distance[range] <- as.numeric(ngeo_distance(
      x,
      from = index[[i]],
      to = targets,
      metric = metric,
      max_distance = max_distance
    ))
    semivariance[range] <- 0.5 *
      (values[index[[i]]] - values[targets])^2
    position <- position + count
  }
  keep <- is.finite(distance) & distance <= max_distance
  distance <- distance[keep]
  semivariance <- semivariance[keep]
  if (!length(distance)) {
    .ngeo_abort(
      "No finite pairs remain within `max_distance`.",
      "ngeo_error_statistic"
    )
  }

  if (length(breaks) == 1L) {
    if (!is.numeric(breaks) || is.na(breaks) ||
        breaks < 1 || breaks != floor(breaks)) {
      .ngeo_abort(
        "`breaks` must be a positive integer or numeric boundaries.",
        "ngeo_error_argument"
      )
    }
    upper <- max(distance)
    if (upper == 0) {
      .ngeo_abort(
        "All analysed elements are colocated.",
        "ngeo_error_statistic"
      )
    }
    boundaries <- seq(0, upper, length.out = as.integer(breaks) + 1L)
  } else {
    boundaries <- as.numeric(breaks)
    if (anyNA(boundaries) || any(!is.finite(boundaries)) ||
        is.unsorted(boundaries, strictly = TRUE) ||
        boundaries[[1L]] > min(distance) ||
        boundaries[[length(boundaries)]] < max(distance)) {
      .ngeo_abort(
        paste(
          "`breaks` boundaries must be finite, strictly increasing,",
          "and cover every retained pair distance."
        ),
        "ngeo_error_argument"
      )
    }
  }
  bin <- cut(
    distance,
    boundaries,
    include.lowest = TRUE,
    right = TRUE
  )
  levels <- levels(bin)
  result <- do.call(
    rbind,
    lapply(seq_along(levels), function(i) {
      selected <- which(bin == levels[[i]])
      data.frame(
        bin = levels[[i]],
        distance = if (length(selected)) {
          mean(distance[selected])
        } else {
          NA_real_
        },
        semivariance = if (length(selected)) {
          mean(semivariance[selected])
        } else {
          NA_real_
        },
        n_pairs = length(selected),
        stringsAsFactors = FALSE
      )
    })
  )
  result <- result[result$n_pairs > 0L, , drop = FALSE]
  rownames(result) <- NULL
  attr(result, "map_id") <- x$maps$map_id[[map_index]]
  attr(result, "map_name") <- x$maps$name[[map_index]]
  attr(result, "domain_hash") <- ngeo_domain_hash(x)
  attr(result, "metric") <- .ngeo_metric_name(
    metric %||% switch(
      x$domain$type,
      surface = "edge_geodesic",
      volume = "world_euclidean",
      points = "euclidean",
      regions = "region_centroid",
      grayordinates = "edge_geodesic"
    )
  )
  attr(result, "pair_count") <- length(distance)
  class(result) <- c("ngeo_variogram", "data.frame")
  result
}

#' @export
print.ngeo_global_stat <- function(x, ...) {
  cat(
    "<ngeo_global_stat>\n",
    "  statistic: ", x$statistic, "\n",
    "  estimate: ", format(x$estimate, digits = 6L), "\n",
    "  expectation: ", format(x$expectation, digits = 6L), "\n",
    "  observations: ", x$n, "\n",
    "  permutations: ", x$permutations, "\n",
    if (x$permutations) {
      paste0("  p-value: ", format(x$p.value, digits = 6L), "\n")
    } else {
      ""
    },
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_lisa <- function(x, ...) {
  cat(
    "<ngeo_lisa>\n",
    "  observations: ", nrow(x), "\n",
    "  map: ", attr(x, "map_name"), "\n",
    "  permutations: ", attr(x, "permutations"), "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_variogram <- function(x, ...) {
  cat(
    "<ngeo_variogram>\n",
    "  bins: ", nrow(x), "\n",
    "  pairs: ", attr(x, "pair_count"), "\n",
    "  metric: ", attr(x, "metric"), "\n",
    sep = ""
  )
  invisible(x)
}

#' Plot neurogeo spatial diagnostics
#'
#' @param x A global statistic, LISA, or variogram result.
#' @param ... Additional arguments passed to base plotting functions.
#'
#' @return `x`, invisibly.
#' @export
plot.ngeo_global_stat <- function(x, ...) {
  graphics::plot(
    x$standardized,
    x$spatial_lag,
    xlab = "Standardized value",
    ylab = "Spatial lag",
    main = paste(x$statistic, "-", x$map_name),
    ...
  )
  graphics::abline(h = 0, v = 0, col = "grey70", lty = 2)
  graphics::abline(stats::lm(x$spatial_lag ~ x$standardized), col = 2)
  invisible(x)
}

#' @rdname plot.ngeo_global_stat
#' @export
plot.ngeo_lisa <- function(x, ...) {
  graphics::plot(
    x$centered,
    x$spatial_lag,
    xlab = "Centered value",
    ylab = "Spatial lag",
    main = paste("Local Moran -", attr(x, "map_name")),
    ...
  )
  graphics::abline(h = 0, v = 0, col = "grey70", lty = 2)
  invisible(x)
}

#' @rdname plot.ngeo_global_stat
#' @export
plot.ngeo_variogram <- function(x, ...) {
  graphics::plot(
    x$distance,
    x$semivariance,
    type = "b",
    xlab = paste("Distance (", attr(x, "metric"), ")", sep = ""),
    ylab = "Semivariance",
    main = paste("Empirical variogram -", attr(x, "map_name")),
    ...
  )
  invisible(x)
}
