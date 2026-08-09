.ngeo_spatial_inputs <- function(x,
                                 spatial_weights,
                                 layer,
                                 na_action,
                                 zero_policy) {
  ngeo_validate(x, "basic")
  if (!inherits(spatial_weights, "ngeo_spatial_weights")) {
    .ngeo_abort(
      "`spatial_weights` must be an `ngeo_spatial_weights` object.",
      "ngeo_error_argument"
    )
  }
  if (!identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort(
      "Weights base hash does not match the dataset.",
      "ngeo_error_base_mismatch"
    )
  }
  if (is.null(x$values)) {
    .ngeo_abort(
      "Spatial statistics require loaded values.",
      "ngeo_error_values"
    )
  }
  layer_index <- .ngeo_layer_selection(x, layer)
  if (length(layer_index) != 1L) {
    .ngeo_abort(
      "`layer` must select exactly one layer.",
      "ngeo_error_argument"
    )
  }
  measure <- .ngeo_measures_for_layers(x, layer_index)
  if (identical(measure$support_behavior[[1L]], "categorical")) {
    .ngeo_abort(
      "Categorical layers are not valid for this statistic.",
      "ngeo_error_measure"
    )
  }

  values <- as.numeric(x$values[, layer_index])
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
    spatial_weights$raw_matrix[index, index, drop = FALSE]
  )
  matrix <- switch(
    spatial_weights$normalization,
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
      "The selected layer has zero variance.",
      "ngeo_error_statistic"
    )
  }

  list(
    values = values[index],
    matrix = matrix,
    raw_matrix = raw_matrix,
    index = index,
    element_id = x$base$elements$element_id[index],
    layer_id = x$layers$layer_id[[layer_index]],
    layer_name = x$layers$name[[layer_index]],
    base_hash = spatial_weights$base_hash,
    weights_method = spatial_weights$method,
    normalization = spatial_weights$normalization,
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
#' @examples
#' control <- ngeo_permutation_control(
#'   permutations = 99, seed = 42, alternative = "two.sided"
#' )
#' control
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
                                   spatial_weights,
                                   layer,
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
    spatial_weights,
    layer,
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
    layer_id = input$layer_id,
    layer_name = input$layer_name,
    element_id = input$element_id,
    values = input$values,
    standardized = as.numeric(scale(input$values)),
    spatial_lag = as.numeric(input$matrix %*% centered),
    base_hash = input$base_hash,
    weights_method = input$weights_method,
    normalization = input$normalization,
    zero_policy = isTRUE(zero_policy),
    omitted = nrow(x$base$elements) - length(input$values),
    seed = seed
  )
  class(result) <- "ngeo_global_stat"
  result
}

#' Global Moran's I
#'
#' @param x An `ngeo` dataset.
#' @param spatial_weights Matching `ngeo_spatial_weights`.
#' @param layer One layer name, ID, or index.
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
#' @section When to use and when not to use:
#' Use Moran's I to quantify global autocorrelation for one declared layer and
#' weight matrix. Do not interpret it as causation or population inference,
#' and do not use permutations whose exchangeability null is scientifically
#' inappropriate.
#' @section Units and assumptions:
#' Moran's I is dimensionless. Its estimand depends on the layer's measurement
#' semantics, retained observations, zero policy, and exact weight
#' normalization.
#' @section Validation:
#' `ngeo_inference_contract()` reports estimand, sampling unit, null, metric,
#' support, and uncertainty target. Reference calculations and null fixtures
#' are registered in the 6.0 scientific validation corpus.
#' @return An `ngeo_global_stat` result.
#' @seealso [ngeo_local_moran()], [ngeo_spatial_weights()],
#'   [ngeo_inference_contract()]
#' @references Moran, P. A. P. (1950). Notes on continuous stochastic
#'   phenomena. Biometrika, 37, 17-23.
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 3))
#' )
#' spatial_weights <- ngeo_spatial_weights(point, method = "knn", k = 2)
#' ngeo_moran(point, spatial_weights, "signal")
#' ngeo_moran(
#'   point, spatial_weights, "signal",
#'   control = ngeo_permutation_control(19, seed = 42)
#' )
#' @export
ngeo_moran <- function(x,
                       spatial_weights,
                       layer = 1L,
                       permutations = 0L,
                       alternative = c("two.sided", "greater", "less"),
                       seed = NULL,
                       na_action = c("fail", "omit"),
                       zero_policy = FALSE,
                       adjust = "none",
                       control = NULL) {
  .ngeo_global_statistic(
    x,
    spatial_weights,
    layer,
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
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 3))
#' )
#' spatial_weights <- ngeo_spatial_weights(point, method = "knn", k = 2)
#' ngeo_geary(point, spatial_weights, "signal")
#' @template stable-statistical-method
#' @export
ngeo_geary <- function(x,
                       spatial_weights,
                       layer = 1L,
                       permutations = 0L,
                       alternative = c("two.sided", "greater", "less"),
                       seed = NULL,
                       na_action = c("fail", "omit"),
                       zero_policy = FALSE,
                       adjust = "none",
                       control = NULL) {
  .ngeo_global_statistic(
    x,
    spatial_weights,
    layer,
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
#' @section When to use and when not to use:
#' Use local Moran statistics to describe element-level local association under
#' an explicit conditional or total randomization null. Do not interpret
#' unadjusted local p-values as a familywise-confirmatory result.
#' @section Units and assumptions:
#' Local statistics are dimensionless but depend on layer semantics, weight
#' normalization, missing-data handling, and the chosen local null.
#' @section Validation:
#' The returned inference contract and columns record the null and adjustment.
#' Local-reference fixtures validate ordering and exact graph-lag calculations.
#' @return An `ngeo_lisa` data frame aligned to the analysed elements.
#' @seealso [ngeo_moran()], [ngeo_spatial_weights()], [stats::p.adjust()]
#' @references Anselin, L. (1995). Local indicators of spatial association.
#'   Geographical Analysis, 27, 93-115.
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 3))
#' )
#' spatial_weights <- ngeo_spatial_weights(point, method = "knn", k = 2)
#' ngeo_local_moran(point, spatial_weights, "signal", permutations = 19, seed = 7)
#' @export
ngeo_local_moran <- function(x,
                             spatial_weights,
                             layer = 1L,
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
    spatial_weights,
    layer,
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
  attr(result, "layer_id") <- input$layer_id
  attr(result, "layer_name") <- input$layer_name
  attr(result, "base_hash") <- input$base_hash
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
#' @param layer One layer name, ID, or index.
#' @param distance_method Explicit distance method.
#' @param breaks Number of bins or a numeric vector of bin boundaries.
#' @param max_distance Optional maximum pair distance.
#' @param na_action Whether to fail or omit non-finite values.
#'
#' @return An `ngeo_variogram` data frame.
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 2, 0, 3, 0, 4, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 2.5, 4, 5))
#' )
#' ngeo_variogram(point, "signal", breaks = c(0, 1.5, 3, 5))
#' @template stable-statistical-method
#' @export
ngeo_variogram <- function(x,
                           layer = 1L,
                           distance_method = NULL,
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
  layer_index <- .ngeo_layer_selection(x, layer)
  if (length(layer_index) != 1L) {
    .ngeo_abort(
      "`layer` must select exactly one layer.",
      "ngeo_error_argument"
    )
  }
  if (identical(
    .ngeo_measures_for_layers(x, layer_index)$support_behavior[[1L]],
    "categorical"
  )) {
    .ngeo_abort(
      "Categorical layers do not have a numeric semivariogram.",
      "ngeo_error_measure"
    )
  }
  values <- as.numeric(x$values[, layer_index])
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
      distance_method = distance_method,
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
  attr(result, "layer_id") <- x$layers$layer_id[[layer_index]]
  attr(result, "layer_name") <- x$layers$name[[layer_index]]
  attr(result, "base_hash") <- base_hash(x)
  attr(result, "distance_method") <- .ngeo_metric_name(
    distance_method %||% switch(
      x$base$type,
      surface = "edge_geodesic",
      volume = "world_euclidean",
      point = "euclidean",
      parcellation = "region_centroid",
      grayordinate = "edge_geodesic"
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
    "  layer: ", attr(x, "layer_name"), "\n",
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
    "  distance_method: ", attr(x, "distance_method"), "\n",
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
    main = paste(x$statistic, "-", x$layer_name),
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
    main = paste("Local Moran -", attr(x, "layer_name")),
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
    xlab = paste("Distance (", attr(x, "distance_method"), ")", sep = ""),
    ylab = "Semivariance",
    main = paste("Empirical variogram -", attr(x, "layer_name")),
    ...
  )
  invisible(x)
}
