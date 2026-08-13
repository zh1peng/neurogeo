.ngeo_undirected_adjacency <- function(spatial_weights) {
  if (!inherits(spatial_weights, "ngeo_spatial_weights")) {
    .ngeo_abort(
      "`spatial_weights` must be an ngeo spatial-weights object.",
      "ngeo_error_argument"
    )
  }
  adjacency <- .ngeo_binary(spatial_weights$raw_matrix)
  .ngeo_binary(adjacency + Matrix::t(adjacency))
}

.ngeo_gis_result <- function(result, class) {
  history <- result$history %||% list()
  identity_fields <- c(
    "base_hash", "source_base_hash", "target_base_hash", "weights_hash",
    "support_hash", "support_map_hash", "ensemble_hash", "layer_id",
    "axis_hash", "index_hash", "basis_hash", "operator_hash",
    "value_hash", "analysis_hash", "estimand_hash", "graph_hash",
    "path_hash", "result_hash"
  )
  for (field in identity_fields) {
    if (is.null(result[[field]]) && !is.null(history[[field]])) {
      result[[field]] <- history[[field]]
    }
  }
  if (is.null(result$result_hash)) {
    result$result_hash <- .ngeo_gis_result_hash(result, class)
  }
  class(result) <- c(class, "ngeo_gis_analysis")
  result
}

.ngeo_gis_result_hash <- function(x, result_class = class(x)[[1L]]) {
  .ngeo_layer_digest(list(
    result_class = result_class,
    content = unclass(x)[setdiff(names(x), "result_hash")]
  ))
}

.ngeo_validate_gis_result <- function(x) {
  if (!inherits(x, "ngeo_gis_analysis") ||
      !identical(x$result_hash, .ngeo_gis_result_hash(x))) {
    .ngeo_abort(
      "The GIS analysis result content no longer matches its identity hash.",
      "ngeo_error_identity"
    )
  }
  invisible(x)
}

#' @export
print.ngeo_gis_analysis <- function(x, ...) {
  .ngeo_validate_gis_result(x)
  label <- sub("^ngeo_", "", class(x)[[1L]])
  history <- x$history %||% list()
  base_hash_value <- history$base_hash %||% x$base_hash %||%
    x$source_base_hash
  cat("<", class(x)[[1L]], ">\n", sep = "")
  cat("  analysis: ", gsub("_", " ", label), "\n", sep = "")
  if (!is.null(base_hash_value)) {
    cat("  base hash: ", base_hash_value, "\n", sep = "")
  }
  if (!is.null(history$method)) {
    cat("  method: ", history$method, "\n", sep = "")
  }
  invisible(x)
}

.ngeo_support_family_inputs <- function(
    x, support_maps, targets, outcome, predictor, scale, zoning) {
  ngeo_validate(x, "strict")
  selected <- .ngeo_support_model_maps(x, outcome, predictor)
  if (inherits(targets, "ngeo")) {
    targets <- rep(list(targets), length(support_maps))
  }
  if (!is.list(support_maps) || !length(support_maps) ||
      !is.list(targets) || length(targets) != length(support_maps)) {
    .ngeo_abort(
      "`support_maps` and `targets` must be aligned non-empty lists.",
      "ngeo_error_alignment"
    )
  }
  n_family <- length(support_maps)
  if (length(scale) != n_family || anyNA(scale) ||
      length(zoning) != n_family || anyNA(zoning)) {
    .ngeo_abort(
      "`scale` and `zoning` must identify every support map.",
      "ngeo_error_alignment"
    )
  }
  scale <- as.character(scale)
  zoning <- as.character(zoning)
  if (any(!nzchar(scale)) || any(!nzchar(zoning))) {
    .ngeo_abort(
      "`scale` and `zoning` identifiers must be non-empty.",
      "ngeo_error_argument"
    )
  }
  if (anyDuplicated(paste(scale, zoning, sep = "\u001f"))) {
    .ngeo_abort(
      "Every declared scale-zoning combination must be unique.",
      "ngeo_error_argument"
    )
  }
  scale_levels <- unique(scale)
  zoning_by_scale <- lapply(scale_levels, function(value) {
    sort(unique(zoning[scale == value]))
  })
  if (length(scale_levels) > 1L &&
      !all(vapply(zoning_by_scale, identical, logical(1), zoning_by_scale[[1L]]))) {
    .ngeo_abort(
      paste(
        "Between-scale and within-scale zoning decomposition requires a",
        "crossed design with the same zoning identifiers at every scale."
      ),
      "ngeo_error_argument"
    )
  }
  for (i in seq_along(support_maps)) {
    .ngeo_validate_support_bases(x, targets[[i]], support_maps[[i]])
    if (!identical(support_maps[[i]]$coverage, "complete")) {
      .ngeo_abort(
        "MAUP analysis requires complete support maps.",
        "ngeo_error_coverage"
      )
    }
  }
  family_id <- names(support_maps)
  if (is.null(family_id) || anyNA(family_id) || any(!nzchar(family_id))) {
    family_id <- paste0("support_", seq_along(support_maps))
  }
  if (anyDuplicated(family_id)) {
    .ngeo_abort(
      "Support-map names must uniquely identify the family.",
      "ngeo_error_argument"
    )
  }
  list(
    selected = selected,
    support_maps = support_maps,
    targets = targets,
    scale = scale,
    zoning = zoning,
    family_id = family_id
  )
}

.ngeo_support_effect_detail <- function(
    x, target, support_map, selected, statistic, family_id, scale, zoning,
    budget) {
  changed <- aggregate_to(
    x, target, support_map, layers = unname(selected), budget = budget
  )
  .ngeo_budget_checkpoint(budget)
  values <- as.matrix(changed$values)
  support <- support_map$target_support %||%
    as.numeric(
      support_map$operator %*%
        (support_map$source_support %||% rep.int(1, ncol(support_map$operator)))
    )
  keep <- is.finite(values[, 1L]) & is.finite(values[, 2L]) &
    is.finite(support) & support > 0
  minimum <- if (identical(statistic, "slope")) 3L else 2L
  if (sum(keep) < minimum) {
    .ngeo_abort(
      "A support-family effect has too few finite target elements.",
      "ngeo_error_inference"
    )
  }
  outcome <- values[keep, 1L]
  predictor <- values[keep, 2L]
  weight <- support[keep]
  if (identical(statistic, "correlation")) {
    standardized_outcome <- .ngeo_weighted_standardize(outcome, weight)
    standardized_predictor <- .ngeo_weighted_standardize(predictor, weight)
    if (any(!is.finite(standardized_outcome$values)) ||
        any(!is.finite(standardized_predictor$values))) {
      .ngeo_abort(
        "A target-level correlation is undefined.",
        "ngeo_error_inference"
      )
    }
    normalized_weight <- weight / max(weight)
    normalized_weight <- normalized_weight / sum(normalized_weight)
    contribution <- normalized_weight * standardized_outcome$values *
      standardized_predictor$values
    estimate <- sum(contribution)
  } else {
    normalized_weight <- weight / max(weight)
    normalized_weight <- normalized_weight / sum(normalized_weight)
    outcome_deviation <- outcome - outcome[[1L]]
    predictor_deviation <- predictor - predictor[[1L]]
    outcome_amplitude <- max(abs(outcome_deviation))
    predictor_amplitude <- max(abs(predictor_deviation))
    centered_outcome <- if (is.finite(outcome_amplitude) &&
                            outcome_amplitude > 0) {
      scaled_outcome <- outcome_deviation / outcome_amplitude
      scaled_outcome - sum(normalized_weight * scaled_outcome)
    } else {
      rep.int(0, length(outcome))
    }
    if (!is.finite(predictor_amplitude) || predictor_amplitude <= 0) {
      .ngeo_abort(
        "A target-level slope is undefined.",
        "ngeo_error_inference"
      )
    }
    scaled_predictor <- predictor_deviation / predictor_amplitude
    centered_predictor <- scaled_predictor -
      sum(normalized_weight * scaled_predictor)
    denominator <- sum(normalized_weight * centered_predictor^2)
    amplitude_ratio <- outcome_amplitude / predictor_amplitude
    if (!is.finite(denominator) || denominator <= 0 ||
        !is.finite(amplitude_ratio)) {
      .ngeo_abort(
        "A target-level slope is undefined.",
        "ngeo_error_inference"
      )
    }
    contribution <- amplitude_ratio * normalized_weight *
      centered_predictor * centered_outcome / denominator
    estimate <- sum(contribution)
  }
  target_id <- support_map$target_element_id[keep]
  list(
    estimate = estimate,
    used_target_elements = sum(keep),
    used_support_fraction = sum(support[keep]) / sum(support),
    contribution = data.frame(
      family_id = family_id,
      scale = scale,
      zoning = zoning,
      target_element_id = target_id,
      target_support = weight,
      outcome = outcome,
      predictor = predictor,
      contribution = contribution,
      stringsAsFactors = FALSE
    )
  )
}

.ngeo_maup_decomposition <- function(estimate, scale) {
  scale_levels <- unique(scale)
  scale_index <- match(scale, scale_levels)
  scale_mean <- vapply(scale_levels, function(value) {
    mean(estimate[scale == value])
  }, numeric(1))
  scale_range <- vapply(scale_levels, function(value) {
    diff(range(estimate[scale == value]))
  }, numeric(1))
  scale_n <- tabulate(scale_index, nbins = length(scale_levels))
  grand <- mean(estimate)
  within_ss <- sum((estimate - scale_mean[scale_index])^2)
  between_ss <- sum(scale_n * (scale_mean - grand)^2)
  total_ss <- within_ss + between_ss
  if (any(!is.finite(c(within_ss, between_ss, total_ss)))) {
    .ngeo_abort(
      paste(
        "MAUP sums of squares exceed the finite numeric range;",
        "rescale the analyzed layers."
      ),
      "ngeo_error_measure"
    )
  }
  list(
    scale = data.frame(
      scale = scale_levels,
      scale_order = seq_along(scale_levels),
      support_maps = scale_n,
      mean_estimate = scale_mean,
      zoning_range = scale_range,
      magnitude_ratio_from_first = if (abs(scale_mean[[1L]]) > 0) {
        abs(scale_mean) / abs(scale_mean[[1L]])
      } else {
        rep.int(NA_real_, length(scale_mean))
      },
      stringsAsFactors = FALSE
    ),
    sum_of_squares = c(
      within_scale_zoning = within_ss,
      between_scale = between_ss,
      total = total_ss
    ),
    fraction = c(
      within_scale_zoning = if (total_ss > 0) within_ss / total_ss else 0,
      between_scale = if (total_ss > 0) between_ss / total_ss else 0
    )
  )
}

.ngeo_maup_pairwise <- function(estimates) {
  if (nrow(estimates) < 2L) {
    return(data.frame(
      first = character(), second = character(), difference = numeric(),
      sign_reversal = logical(), scale_change = logical(),
      stringsAsFactors = FALSE
    ))
  }
  pair <- utils::combn(seq_len(nrow(estimates)), 2L)
  data.frame(
    first = estimates$family_id[pair[1L, ]],
    second = estimates$family_id[pair[2L, ]],
    difference = estimates$estimate[pair[2L, ]] -
      estimates$estimate[pair[1L, ]],
    sign_reversal = sign(estimates$estimate[pair[1L, ]]) !=
      sign(estimates$estimate[pair[2L, ]]) &
      estimates$estimate[pair[1L, ]] != 0 &
      estimates$estimate[pair[2L, ]] != 0,
    scale_change = estimates$scale[pair[1L, ]] !=
      estimates$scale[pair[2L, ]],
    stringsAsFactors = FALSE
  )
}

#' Profile support-scale and zoning sensitivity
#'
#' Computes a support-weighted association for each declared support map and
#' separates variability between scales from variability among zonings within
#' scale. This is a descriptive MAUP sensitivity profile, not a test that an
#' atlas is invariant or a license to select a support on the same data.
#'
#' @param x Source `ngeo` object.
#' @param support_maps Named list of complete source-to-target support maps.
#' @param targets One matching target `ngeo` per support map, or one shared
#'   target object.
#' @param outcome,predictor Single numeric layer selectors.
#' @param scale,zoning Character identifiers aligned with `support_maps`.
#'   Each scale-zoning combination must be unique; scale order is the order of
#'   first appearance.
#' @param statistic Support-weighted correlation or slope.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#'
#' @return An `ngeo_maup_sensitivity` containing support-specific estimates,
#'   a descriptive sums-of-squares decomposition, pairwise reversals, and
#'   target-region contributions that sum to each estimate.
#' @templateVar example_call ngeo_maup_sensitivity(source_data, support_maps, targets, "outcome", "predictor", scale, zoning)
#' @template stable-neuroimaging-method
#' @references
#' Fotheringham, A. S. and Wong, D. W. S. (1991). The modifiable areal unit
#' problem in multivariate statistical analysis. *Environment and Planning A*,
#' 23, 1025--1044.
#' @export
ngeo_maup_sensitivity <- function(
    x,
    support_maps,
    targets,
    outcome,
    predictor,
    scale,
    zoning,
    statistic = c("correlation", "slope"),
    budget = ngeo_resource_budget()) {
  statistic <- match.arg(statistic)
  inputs <- .ngeo_support_family_inputs(
    x, support_maps, targets, outcome, predictor, scale, zoning
  )
  context <- .ngeo_budget_context(budget)
  target_rows <- sum(vapply(
    inputs$support_maps, function(map) nrow(map$operator), numeric(1)
  ))
  map_nonzero <- sum(vapply(
    inputs$support_maps,
    function(map) length(map$operator@x),
    numeric(1)
  ))
  family_count <- length(inputs$support_maps)
  pair_count <- family_count * (family_count - 1) / 2
  .ngeo_budget_assert(context, "blocks", family_count)
  .ngeo_budget_assert(
    context, "materialized_elements",
    map_nonzero + 12 * target_rows + 12 * family_count + 8 * pair_count
  )
  .ngeo_budget_assert(
    context, "memory_bytes",
    32 * map_nonzero + 256 * target_rows + 256 * family_count
  )
  detail <- lapply(seq_along(inputs$support_maps), function(i) {
    .ngeo_budget_checkpoint(context)
    .ngeo_support_effect_detail(
      x, inputs$targets[[i]], inputs$support_maps[[i]], inputs$selected,
      statistic, inputs$family_id[[i]], inputs$scale[[i]],
      inputs$zoning[[i]], context
    )
  })
  estimates <- data.frame(
    family_id = inputs$family_id,
    scale = inputs$scale,
    zoning = inputs$zoning,
    estimate = vapply(detail, `[[`, numeric(1), "estimate"),
    target_elements = vapply(
      inputs$support_maps, function(map) nrow(map$operator), integer(1)
    ),
    used_target_elements = vapply(
      detail, `[[`, integer(1), "used_target_elements"
    ),
    used_support_fraction = vapply(
      detail, `[[`, numeric(1), "used_support_fraction"
    ),
    support_map_hash = vapply(
      inputs$support_maps, ngeo_support_map_hash, character(1)
    ),
    stringsAsFactors = FALSE
  )
  result <- list(
    estimates = estimates,
    decomposition = .ngeo_maup_decomposition(
      estimates$estimate, estimates$scale
    ),
    pairwise = .ngeo_maup_pairwise(estimates),
    regional_contribution = do.call(
      rbind, lapply(detail, `[[`, "contribution")
    ),
    statistic = statistic,
    outcome = x$layers$name[inputs$selected[["outcome"]]],
    predictor = x$layers$name[inputs$selected[["predictor"]]],
    source_base_hash = base_hash(x),
    analysis_hash = .ngeo_layer_digest(list(
      base_hash = base_hash(x),
      support_map_hash = estimates$support_map_hash,
      scale = estimates$scale,
      zoning = estimates$zoning,
      selected_layer_id = x$layers$layer_id[inputs$selected],
      selected_values = as.matrix(x$values[, inputs$selected, drop = FALSE]),
      statistic = statistic
    )),
    status = "stable",
    interpretation = paste(
      "support-scale and zoning sensitivity;",
      "not atlas invariance or selection-safe inference"
    ),
    history = list(
      method = "support_weighted_maup_profile",
      base_hash = base_hash(x),
      weighting = "target support",
      null = "none",
      population_inference = FALSE,
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_maup_sensitivity")
}

.ngeo_local_z <- function(values, support) {
  standardized <- .ngeo_weighted_standardize(values, support)
  if (any(!is.finite(standardized$values))) {
    .ngeo_abort(
      "Local layer coupling is undefined for a constant map.",
      "ngeo_error_measure"
    )
  }
  standardized$values
}

.ngeo_local_coupling_block <- function(
    x, index, lookup, pair, direction, metric, support, weights,
    exceedance, permutation = NULL, surrogate = NULL) {
  is_simulation <- !is.null(permutation) || !is.null(surrogate)
  first <- if (identical(direction, "x_to_y")) pair$x else pair$y
  second <- if (identical(direction, "x_to_y")) pair$y else pair$x
  output <- vector("list", nrow(index$unit))
  for (unit in seq_len(nrow(index$unit))) {
    layers <- lookup[unit, c(first, second)]
    if (anyNA(layers)) next
    current <- .ngeo_coupling_values(x, layers)
    z_x <- .ngeo_local_z(current[, 1L], support)
    z_y <- .ngeo_local_z(current[, 2L], support)
    simulated_y <- if (!is.null(surrogate)) {
      .ngeo_local_z(
        surrogate[, as.character(layers[[2L]])], support
      )
    } else if (is.null(permutation)) {
      z_y
    } else {
      z_y[permutation]
    }
    lag_y <- as.numeric(weights %*% simulated_y)
    statistic <- switch(
      metric,
      local_cross_moran = z_x * lag_y,
      local_geary = as.numeric(weights %*% (simulated_y^2)) -
        2 * z_x * lag_y + z_x^2 * Matrix::rowSums(weights),
      coexceedance = {
        row_weight <- Matrix::rowSums(abs(weights))
        high_y <- as.numeric(weights %*% (simulated_y >= exceedance)) /
          row_weight
        low_y <- as.numeric(weights %*% (simulated_y <= -exceedance)) /
          row_weight
        as.numeric(z_x >= exceedance) * high_y -
          as.numeric(z_x <= -exceedance) * low_y
      }
    )
    if (is_simulation) {
      output[[unit]] <- statistic
      next
    }
    cluster <- ifelse(
      z_x >= 0 & lag_y >= 0, "high-high",
      ifelse(
        z_x < 0 & lag_y < 0, "low-low",
        ifelse(z_x >= 0, "high-low", "low-high")
      )
    )
    output[[unit]] <- data.frame(
      unit_id = index$unit$unit_id[[unit]],
      pair_id = pair$pair_id,
      layer_x = first,
      layer_y = second,
      direction = direction,
      metric = metric,
      element_id = x$base$elements$element_id,
      value_x = current[, 1L],
      value_y = current[, 2L],
      z_x = z_x,
      z_y = z_y,
      spatial_lag_y = lag_y,
      statistic = statistic,
      bivariate_lag_quadrant = cluster,
      stringsAsFactors = FALSE
    )
  }
  output <- Filter(Negate(is.null), output)
  if (!length(output)) return(NULL)
  if (is_simulation) return(unlist(output, use.names = FALSE))
  do.call(rbind, output)
}

.ngeo_local_coupling_specs <- function(pair_table, directions, metrics) {
  specs <- list()
  for (pair in seq_len(nrow(pair_table))) {
    current <- pair_table[pair, , drop = FALSE]
    for (direction in directions) {
      for (metric in metrics) {
        specs[[length(specs) + 1L]] <- list(
          pair = current, direction = direction, metric = metric
        )
      }
    }
  }
  specs
}

.ngeo_local_coupling_simulation <- function(
    x, index, lookup, specs, support, weights, exceedance,
    permutation = NULL, surrogate = NULL) {
  unlist(lapply(specs, function(spec) {
    .ngeo_local_coupling_block(
      x, index, lookup, spec$pair, spec$direction, spec$metric,
      support, weights, exceedance, permutation, surrogate
    )
  }), use.names = FALSE)
}

.ngeo_local_coupling_inference <- function(
    result, x, index, lookup, specs, support, weights, exceedance,
    permutations, seed, adjust, null, maxT_scope, budget) {
  result$p_value <- NA_real_
  result$p_adjusted <- NA_real_
  infer <- result$metric != "coexceedance"
  if (!permutations || !any(infer)) return(result)
  observed <- result$statistic[infer]
  metric <- result$metric[infer]
  raw_count <- integer(length(observed))
  adjusted_count <- integer(length(observed))
  family <- if (identical(maxT_scope, "subject")) {
    factor(result$unit_id[infer])
  } else {
    interaction(
      result$unit_id[infer], result$pair_id[infer],
      result$direction[infer], result$metric[infer],
      drop = TRUE, lex.order = TRUE
    )
  }
  lagged_layers <- unique(unlist(lapply(specs, function(spec) {
    lagged <- if (identical(spec$direction, "x_to_y")) {
      spec$pair$y
    } else {
      spec$pair$x
    }
    as.integer(lookup[, lagged])
  }), use.names = FALSE))
  lagged_layers <- lagged_layers[!is.na(lagged_layers)]
  spatial_draws <- if (identical(null, "moran")) {
    maximum <- getOption("neurogeo.max_spectral_null_elements", 2000L)
    if (nrow(x$base$elements) > maximum) {
      .ngeo_abort(
        sprintf("Spectral nulls are limited to %d analysed elements.", maximum),
        "ngeo_error_resource"
      )
    }
    .ngeo_moran_randomizations(
      as.matrix(x$values[, lagged_layers, drop = FALSE]),
      weights, permutations, seed
    )$draws
  } else {
    NULL
  }
  run <- function() {
    for (simulation in seq_len(permutations)) {
      .ngeo_budget_checkpoint(budget)
      permutation <- if (identical(null, "free")) {
        sample(seq_len(nrow(x$base$elements)))
      } else {
        NULL
      }
      surrogate <- if (identical(null, "moran")) {
        current_draw <- spatial_draws[[simulation]]
        colnames(current_draw) <- as.character(lagged_layers)
        current_draw
      } else {
        NULL
      }
      current <- .ngeo_local_coupling_simulation(
        x, index, lookup, specs, support, weights, exceedance,
        permutation, surrogate
      )[infer]
      for (name in unique(metric)) {
        rows <- which(metric == name)
        transformed <- if (identical(name, "local_geary")) {
          current[rows]
        } else {
          abs(current[rows])
        }
        target <- if (identical(name, "local_geary")) {
          observed[rows]
        } else {
          abs(observed[rows])
        }
        raw_count[rows] <<- raw_count[rows] + (transformed >= target)
      }
      if (identical(adjust, "maxT")) {
        transformed <- ifelse(
          metric == "local_geary", current, abs(current)
        )
        target <- ifelse(
          metric == "local_geary", observed, abs(observed)
        )
        for (group in levels(family)) {
          rows <- which(family == group)
          adjusted_count[rows] <<- adjusted_count[rows] +
            (max(transformed[rows]) >= target[rows])
        }
      }
    }
    invisible(NULL)
  }
  if (identical(null, "free")) .ngeo_with_seed(seed, run) else run()
  result$p_value[infer] <- (raw_count + 1) / (permutations + 1)
  result$p_adjusted[infer] <- if (identical(adjust, "maxT")) {
    (adjusted_count + 1) / (permutations + 1)
  } else {
    result$p_value[infer]
  }
  result
}

#' Map local cross-layer spatial coupling
#'
#' Computes directed bivariate local Moran, cross-layer local Geary, and
#' weighted signed co-exceedance maps for aligned layer pairs. Inference can use
#' either a reference-map free permutation or singleton Moran spectral
#' randomization of the lagged stack. The latter preserves each randomized
#' map's global Moran quadratic form. Neither option is population inference.
#'
#' @param x Aligned multilayer `ngeo` object.
#' @param index Matching `ngeo_layer_index`.
#' @param spatial_weights Matching spatial weights. Directed weights are
#'   retained for directed coupling.
#' @param pairs Optional feature-pair data frame accepted by
#'   [ngeo_layer_coupling()].
#' @param metrics One or more local coupling metrics.
#' @param direction Compute both directions or one declared direction.
#'   Inferential calls must select one direction so that every statistic in a
#'   multiplicity family is generated by one coherent permutation action.
#' @param exceedance Non-negative standardized threshold used by
#'   `coexceedance`.
#' @param permutations Number of null randomizations. Zero returns descriptive
#'   maps only.
#' @param seed Reproducible permutation seed.
#' @param null Reference-map null: Moran spectral randomization preserving the
#'   observed global spatial autocorrelation, or unconstrained free element
#'   permutation.
#' @param adjust Max-T adjustment across locations within each declared
#'   family, or no multiplicity adjustment.
#' @param maxT_scope Make each independent subject/unit one max-T family, or
#'   use a separate family for every unit-pair-direction-metric map.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#'
#' @return An `ngeo_local_layer_coupling` with one row per available
#'   unit-pair-direction-element combination and an explicit null contract.
#' @templateVar example_call ngeo_local_layer_coupling(x, index, spatial_weights, permutations = 0)
#' @template stable-statistical-method
#' @references
#' Anselin, L. (1995). Local indicators of spatial association--LISA.
#' *Geographical Analysis*, 27, 93--115.
#'
#' Anselin, L. (2019). A local indicator of multivariate spatial association:
#' Extending Geary's c. *Geographical Analysis*, 51, 133--150.
#' @examples
#' \dontrun{
#' ngeo_local_layer_coupling(x, index, spatial_weights, permutations = 0)
#' }
#' @name ngeo_local_layer_coupling
#' @usage NULL
.ngeo_local_coupling_prepare <- function(
    x, index, spatial_weights, pairs, metrics, direction, exceedance,
    permutations, null, adjust, maxT_scope, budget) {
  ngeo_validate(x, "basic")
  lookup <- .ngeo_coupling_index(x, index)
  allowed <- c("local_cross_moran", "local_geary", "coexceedance")
  if (!is.character(metrics) || !length(metrics) || anyNA(metrics) ||
      any(!metrics %in% allowed) || anyDuplicated(metrics)) {
    .ngeo_abort(
      "Unknown or duplicate local coupling metrics.",
      "ngeo_error_argument"
    )
  }
  direction <- match.arg(direction, c("both", "x_to_y", "y_to_x"))
  directions <- if (identical(direction, "both")) {
    c("x_to_y", "y_to_x")
  } else {
    direction
  }
  if (!is.numeric(exceedance) || length(exceedance) != 1L ||
      is.na(exceedance) || !is.finite(exceedance) || exceedance < 0) {
    .ngeo_abort(
      "`exceedance` must be one non-negative finite z threshold.",
      "ngeo_error_argument"
    )
  }
  permutations <- .ngeo_permutations(permutations)
  null <- match.arg(null, c("moran", "free"))
  if (permutations > 0L && identical(direction, "both")) {
    .ngeo_abort(
      paste(
        "Permutation inference requires one declared direction;",
        "run `x_to_y` and `y_to_x` as separate families."
      ),
      "ngeo_error_argument"
    )
  }
  if (permutations > 0L && "coexceedance" %in% metrics) {
    .ngeo_abort(
      paste(
        "`coexceedance` is descriptive in version 6.2; request it in a",
        "separate call with `permutations = 0`."
      ),
      "ngeo_error_argument"
    )
  }
  adjust <- match.arg(adjust, c("maxT", "none"))
  maxT_scope <- match.arg(maxT_scope, c("subject", "map"))
  pair_table <- .ngeo_coupling_pairs(index, pairs, TRUE)
  .ngeo_coupling_measures(
    x, unique(c(pair_table$x, pair_table$y))
  )
  support_info <- .ngeo_coupling_support(x, NULL)
  weights_info <- .ngeo_coupling_weights(x, spatial_weights)
  specs <- .ngeo_local_coupling_specs(pair_table, directions, metrics)
  context <- .ngeo_budget_context(budget)
  maximum_rows <- as.double(nrow(x$base$elements)) * nrow(index$unit) *
    length(specs)
  .ngeo_budget_assert(context, "blocks", length(specs) + permutations)
  .ngeo_budget_assert(
    context, "materialized_elements", 18 * maximum_rows
  )
  .ngeo_budget_assert(
    context, "memory_bytes", 256 * maximum_rows
  )
  list(
    lookup = lookup, metrics = metrics, directions = directions,
    permutations = permutations, null = null, adjust = adjust,
    maxT_scope = maxT_scope, pair_table = pair_table,
    support_info = support_info, weights_info = weights_info, specs = specs,
    context = context
  )
}

#' @rdname ngeo_local_layer_coupling
#' @export
ngeo_local_layer_coupling <- function(
    x,
    index,
    spatial_weights,
    pairs = NULL,
    metrics = c("local_cross_moran", "local_geary", "coexceedance"),
    direction = c("both", "x_to_y", "y_to_x"),
    exceedance = 0,
    permutations = 0L,
    seed = NULL,
    null = c("moran", "free"),
    adjust = c("maxT", "none"),
    maxT_scope = c("subject", "map"),
    budget = ngeo_resource_budget()) {
  prepared <- .ngeo_local_coupling_prepare(
    x, index, spatial_weights, pairs, metrics, direction, exceedance,
    permutations, null, adjust, maxT_scope, budget
  )
  lookup <- prepared$lookup
  metrics <- prepared$metrics
  directions <- prepared$directions
  permutations <- prepared$permutations
  null <- prepared$null
  adjust <- prepared$adjust
  maxT_scope <- prepared$maxT_scope
  pair_table <- prepared$pair_table
  support_info <- prepared$support_info
  weights_info <- prepared$weights_info
  specs <- prepared$specs
  context <- prepared$context
  blocks <- lapply(specs, function(spec) {
    .ngeo_budget_checkpoint(context)
    .ngeo_local_coupling_block(
      x, index, lookup, spec$pair, spec$direction, spec$metric,
      support_info$values, weights_info$matrix, exceedance
    )
  })
  blocks <- Filter(Negate(is.null), blocks)
  if (!length(blocks)) {
    .ngeo_abort(
      "No complete unit-layer pair is available for local coupling.",
      "ngeo_error_layer_missing"
    )
  }
  values <- do.call(rbind, blocks)
  rownames(values) <- NULL
  values <- .ngeo_local_coupling_inference(
    values, x, index, lookup, specs, support_info$values,
    weights_info$matrix, exceedance, permutations, seed, adjust, null,
    maxT_scope, context
  )
  result <- list(
    values = values,
    base_hash = base_hash(x),
    index_hash = index$index_hash,
    support_hash = support_info$hash,
    weights_hash = weights_info$hash,
    value_hash = .ngeo_layer_digest(as.matrix(x$values)),
    analysis_hash = .ngeo_layer_digest(list(
      base_hash = base_hash(x), index_hash = index$index_hash,
      support_hash = support_info$hash, weights_hash = weights_info$hash,
      values = as.matrix(x$values),
      pair = pair_table, direction = directions, metrics = metrics,
      exceedance = exceedance, permutations = permutations,
      seed = if (permutations) .ngeo_seed(seed) else NULL, null = null,
      adjust = adjust, maxT_scope = maxT_scope
    )),
    diagnostics = list(
      units = length(unique(values$unit_id)),
      pairs = nrow(pair_table),
      directions = directions,
      metrics = metrics,
      elements = nrow(x$base$elements),
      family = if (identical(maxT_scope, "subject")) {
        "all requested local endpoints within each independent unit"
      } else {
        "locations within each unit-pair-direction-metric map"
      },
      adjustment = adjust
    ),
    history = list(
      method = "local_cross_layer_coupling",
      base_hash = base_hash(x),
      index_hash = index$index_hash,
      support_hash = support_info$hash,
      weights_hash = weights_info$hash,
      permutations = permutations,
      seed = if (permutations) .ngeo_seed(seed) else NULL,
      null = if (permutations) {
        if (identical(null, "moran")) {
          paste(
            "singleton Moran spectral randomization of the lagged stack;",
            "shared eigenmode signs across randomized maps"
          )
        } else {
          "shared unconstrained element permutation of the lagged stack"
        }
      } else {
        "none"
      },
      preserves_spatial_autocorrelation = permutations > 0L &&
        identical(null, "moran"),
      maxT_scope = maxT_scope,
      inference_unit = "spatial_map",
      population_inference = FALSE,
      geary_alternative = "greater (local cross-layer dissimilarity)",
      moran_alternative = "two-sided",
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_local_layer_coupling")
}

.ngeo_operator_graph_edge <- function(map, edge_id, base_hashes) {
  ngeo_validate_support_map(map)
  from <- names(base_hashes)[base_hashes == map$source_base_hash]
  to <- names(base_hashes)[base_hashes == map$target_base_hash]
  if (length(from) != 1L || length(to) != 1L) {
    .ngeo_abort(
      "Every support-map endpoint must match exactly one named graph base.",
      "ngeo_error_base_mismatch"
    )
  }
  data.frame(
    edge_id = edge_id,
    from = from,
    to = to,
    support_map_hash = ngeo_support_map_hash(map),
    stringsAsFactors = FALSE
  )
}

#' Build a directed graph of cross-support operators
#'
#' Registers existing `ngeo_support_map` objects as directed edges between
#' named spatial bases. The result stores base identities and sparse operators,
#' not duplicate neuroimaging value arrays.
#'
#' @param bases Named list of `ngeo` objects with distinct ordered bases.
#' @param edges Named non-empty list of support maps whose endpoints occur in
#'   `bases`.
#'
#' @return An `ngeo_operator_graph` containing base identities, support-map
#'   edges, and a content hash.
#' @templateVar example_call ngeo_operator_graph(list(source = x, target = y), list(source_to_target = support_map))
#' @template stable-neuroimaging-method
#' @export
ngeo_operator_graph <- function(bases, edges) {
  if (!is.list(bases) || !length(bases) ||
      is.null(names(bases)) || anyNA(names(bases)) ||
      any(!nzchar(names(bases))) || anyDuplicated(names(bases)) ||
      any(!vapply(bases, inherits, logical(1), "ngeo"))) {
    .ngeo_abort(
      "`bases` must be a named list of uniquely identified ngeo objects.",
      "ngeo_error_argument"
    )
  }
  lapply(bases, ngeo_validate, level = "basic")
  base_hashes <- vapply(bases, base_hash, character(1))
  if (anyDuplicated(base_hashes)) {
    .ngeo_abort(
      "Operator-graph base names must not alias the same ordered base.",
      "ngeo_error_argument"
    )
  }
  if (!is.list(edges) || !length(edges) ||
      any(!vapply(edges, inherits, logical(1), "ngeo_support_map"))) {
    .ngeo_abort(
      "`edges` must be a non-empty list of support maps.",
      "ngeo_error_argument"
    )
  }
  edge_id <- names(edges)
  if (is.null(edge_id) || anyNA(edge_id) || any(!nzchar(edge_id))) {
    edge_id <- paste0("edge_", seq_along(edges))
  }
  if (anyDuplicated(edge_id)) {
    .ngeo_abort(
      "Operator-graph edge names must be unique.",
      "ngeo_error_argument"
    )
  }
  names(edges) <- edge_id
  edge_table <- do.call(rbind, Map(
    .ngeo_operator_graph_edge, edges, edge_id,
    MoreArgs = list(base_hashes = base_hashes)
  ))
  rownames(edge_table) <- NULL
  node_table <- data.frame(
    node_id = names(bases),
    base_hash = unname(base_hashes),
    base_type = vapply(bases, function(x) x$base$type, character(1)),
    elements = vapply(
      bases, function(x) nrow(x$base$elements), integer(1)
    ),
    stringsAsFactors = FALSE
  )
  result <- list(
    edges = edges,
    nodes = node_table,
    edge_table = edge_table,
    graph_hash = .ngeo_layer_digest(list(
      nodes = node_table,
      edges = edge_table[c("edge_id", "from", "to", "support_map_hash")]
    )),
    status = "stable",
    history = list(
      method = "directed_support_operator_graph",
      stores_value_arrays = FALSE,
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_operator_graph")
}

.ngeo_validate_operator_graph <- function(graph) {
  required_nodes <- c("node_id", "base_hash", "base_type", "elements")
  required_edges <- c("edge_id", "from", "to", "support_map_hash")
  if (!inherits(graph, "ngeo_operator_graph") || !is.list(graph) ||
      !is.data.frame(graph$nodes) || !is.data.frame(graph$edge_table) ||
      !identical(names(graph$nodes), required_nodes) ||
      !identical(names(graph$edge_table), required_edges) ||
      anyNA(graph$nodes) || anyDuplicated(graph$nodes$node_id) ||
      anyDuplicated(graph$nodes$base_hash) ||
      !is.integer(graph$nodes$elements) || any(graph$nodes$elements < 1L) ||
      !is.list(graph$edges) ||
      !identical(names(graph$edges), graph$edge_table$edge_id) ||
      !identical(
        unname(vapply(graph$edges, ngeo_support_map_hash, character(1))),
        graph$edge_table$support_map_hash
      )) {
    .ngeo_abort(
      "`graph` is not a valid ngeo operator graph or its edges were modified.",
      "ngeo_error_argument"
    )
  }
  base_hashes <- stats::setNames(graph$nodes$base_hash, graph$nodes$node_id)
  rebuilt <- do.call(rbind, Map(
    .ngeo_operator_graph_edge,
    graph$edges,
    names(graph$edges),
    MoreArgs = list(base_hashes = base_hashes)
  ))
  rownames(rebuilt) <- NULL
  expected_graph_hash <- .ngeo_layer_digest(list(
    nodes = graph$nodes,
    edges = rebuilt[c("edge_id", "from", "to", "support_map_hash")]
  ))
  if (!identical(rebuilt, graph$edge_table) ||
      !identical(expected_graph_hash, graph$graph_hash)) {
    .ngeo_abort(
      "The operator graph endpoint table or content hash was modified.",
      "ngeo_error_identity"
    )
  }
  invisible(graph)
}

.ngeo_operator_one_path <- function(graph, from, to, excluded_edge = NULL) {
  queue <- from
  visited <- stats::setNames(
    rep.int(FALSE, nrow(graph$nodes)), graph$nodes$node_id
  )
  visited[[from]] <- TRUE
  parent_node <- stats::setNames(rep.int(NA_character_, nrow(graph$nodes)),
                                 graph$nodes$node_id)
  parent_edge <- parent_node
  while (length(queue)) {
    current <- queue[[1L]]
    queue <- queue[-1L]
    candidates <- graph$edge_table[
      graph$edge_table$from == current &
        graph$edge_table$edge_id != (excluded_edge %||% ""),
      , drop = FALSE
    ]
    for (i in seq_len(nrow(candidates))) {
      next_node <- candidates$to[[i]]
      if (visited[[next_node]]) next
      visited[[next_node]] <- TRUE
      parent_node[[next_node]] <- current
      parent_edge[[next_node]] <- candidates$edge_id[[i]]
      if (identical(next_node, to)) {
        nodes <- to
        edges <- character()
        node <- to
        while (!identical(node, from)) {
          edges <- c(parent_edge[[node]], edges)
          node <- parent_node[[node]]
          nodes <- c(node, nodes)
        }
        return(list(node = to, edges = edges, nodes = nodes))
      }
      queue <- c(queue, next_node)
    }
  }
  NULL
}

.ngeo_operator_paths <- function(graph, from, to) {
  first <- .ngeo_operator_one_path(graph, from, to)
  if (is.null(first)) return(list())
  for (edge in first$edges) {
    alternative <- .ngeo_operator_one_path(
      graph, from, to, excluded_edge = edge
    )
    if (!is.null(alternative)) return(list(first, alternative))
  }
  list(first)
}

.ngeo_operator_explicit_path <- function(graph, from, to, edge_id) {
  if (!is.character(edge_id) || !length(edge_id) || anyNA(edge_id) ||
      any(!edge_id %in% graph$edge_table$edge_id)) {
    .ngeo_abort(
      "`edge_id` must name a non-empty ordered graph path.",
      "ngeo_error_argument"
    )
  }
  current <- from
  nodes <- from
  for (id in edge_id) {
    edge <- graph$edge_table[
      match(id, graph$edge_table$edge_id), , drop = FALSE
    ]
    if (!identical(edge$from[[1L]], current)) {
      .ngeo_abort(
        "The declared operator edges do not form a directed path.",
        "ngeo_error_alignment"
      )
    }
    current <- edge$to[[1L]]
    if (current %in% nodes) {
      .ngeo_abort(
        "The declared operator path must be simple and cannot revisit a base.",
        "ngeo_error_alignment"
      )
    }
    nodes <- c(nodes, current)
  }
  if (!identical(current, to)) {
    .ngeo_abort(
      "The declared operator path does not reach `to`.",
      "ngeo_error_alignment"
    )
  }
  list(node = current, edges = edge_id, nodes = nodes)
}

.ngeo_operator_diagnostic_row <- function(map, edge_id, from, to) {
  diagnostic <- ngeo_support_diagnostics(map, condition = FALSE)
  summary <- stats::setNames(
    diagnostic$summary$value, diagnostic$summary$distance_method
  )
  sigma_max <- .ngeo_power_norm(map$operator, 30L)
  stable_rank <- if (sigma_max > 0) {
    sum(map$operator@x^2) / sigma_max^2
  } else {
    0
  }
  data.frame(
    edge_id = edge_id,
    from = from,
    to = to,
    source_elements = ncol(map$operator),
    target_elements = nrow(map$operator),
    nonzero = length(map$operator@x),
    source_coverage_fraction = summary[["source_coverage_fraction"]],
    mean_normalized_entropy = summary[["mean_normalized_entropy"]],
    stable_rank = stable_rank,
    numerical_rank = NA_integer_,
    condition_number = NA_real_,
    support_map_hash = ngeo_support_map_hash(map),
    stringsAsFactors = FALSE
  )
}

#' Compose a path through a support-operator graph
#'
#' Selects the only directed path, or validates an explicit edge sequence, and
#' composes it with [ngeo_compose_support_map()]. Any alternative route is
#' rejected because hop count is not an information-preservation criterion.
#'
#' @param graph An `ngeo_operator_graph`.
#' @param from,to Names of different graph bases.
#' @param edge_id Optional ordered character vector of edge identifiers.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#'
#' @return An `ngeo_operator_path` with the composed support map and per-edge
#'   plus composed coverage, entropy, and sparse stable-rank diagnostics.
#' @templateVar example_call ngeo_operator_path(graph, "source", "target")
#' @template stable-neuroimaging-method
#' @export
ngeo_operator_path <- function(
    graph, from, to, edge_id = NULL, budget = ngeo_resource_budget()) {
  context <- .ngeo_budget_context(budget)
  .ngeo_validate_operator_graph(graph)
  if (!is.character(from) || length(from) != 1L ||
      !is.character(to) || length(to) != 1L ||
      !from %in% graph$nodes$node_id || !to %in% graph$nodes$node_id ||
      identical(from, to)) {
    .ngeo_abort(
      "`from` and `to` must name two different graph bases.",
      "ngeo_error_argument"
    )
  }
  path <- if (is.null(edge_id)) {
    candidates <- .ngeo_operator_paths(graph, from, to)
    if (!length(candidates)) {
      .ngeo_abort(
        "No directed support-map path connects the requested bases.",
        "ngeo_error_path"
      )
    }
    if (length(candidates) > 1L) {
      .ngeo_abort(
        paste(
          "Several directed operator paths exist; declare `edge_id`",
          "explicitly because hop count is not an information-loss metric."
        ),
        "ngeo_error_path_ambiguous"
      )
    }
    candidates[[1L]]
  } else {
    .ngeo_operator_explicit_path(graph, from, to, edge_id)
  }
  maps <- graph$edges[path$edges]
  .ngeo_budget_assert(context, "blocks", 2 * length(maps) + 1L)
  .ngeo_budget_assert(
    context, "materialized_elements",
    sum(vapply(maps, function(map) length(map$operator@x), numeric(1)))
  )
  composed <- if (length(maps) == 1L) {
    maps[[1L]]
  } else {
    Reduce(
      function(first, second) {
        .ngeo_budget_checkpoint(context)
        ngeo_compose_support_map(first, second, budget = context)
      },
      maps
    )
  }
  diagnostic_maps <- c(maps, list(composed))
  diagnostic_elements <- sum(vapply(
    diagnostic_maps,
    function(map) sum(dim(map$operator)) + length(map$operator@x),
    numeric(1)
  ))
  .ngeo_budget_assert(context, "blocks", length(diagnostic_maps))
  .ngeo_budget_assert(
    context, "materialized_elements", diagnostic_elements
  )
  .ngeo_budget_assert(
    context, "memory_bytes", 96 * diagnostic_elements
  )
  edge_rows <- lapply(seq_along(path$edges), function(i) {
    .ngeo_budget_checkpoint(context)
    id <- path$edges[[i]]
    row <- graph$edge_table[
      match(id, graph$edge_table$edge_id), , drop = FALSE
    ]
    .ngeo_operator_diagnostic_row(
      maps[[i]], id, row$from[[1L]], row$to[[1L]]
    )
  })
  diagnostics <- do.call(rbind, c(
    edge_rows,
    list(.ngeo_operator_diagnostic_row(
      composed, "composed", from, to
    ))
  ))
  rownames(diagnostics) <- NULL
  result <- list(
    from = from,
    to = to,
    source_base_hash = graph$nodes$base_hash[
      match(from, graph$nodes$node_id)
    ],
    target_base_hash = graph$nodes$base_hash[
      match(to, graph$nodes$node_id)
    ],
    nodes = path$nodes,
    edge_id = path$edges,
    support_map = composed,
    diagnostics = diagnostics,
    graph_hash = graph$graph_hash,
    path_hash = .ngeo_layer_digest(list(
      graph_hash = graph$graph_hash,
      from = from,
      to = to,
      edge_id = path$edges,
      composed = ngeo_support_map_hash(composed)
    )),
    status = "stable",
    history = list(
      method = "explicit_support_operator_path",
      graph_hash = graph$graph_hash,
      implicit_path_choice = is.null(edge_id),
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_operator_path")
}
