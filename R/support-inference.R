.ngeo_support_model_maps <- function(x, outcome, predictor) {
  outcome <- .ngeo_map_selection(x, outcome)
  predictor <- .ngeo_map_selection(x, predictor)
  if (length(outcome) != 1L || length(predictor) != 1L ||
      identical(outcome, predictor)) {
    .ngeo_abort(
      "`outcome` and `predictor` must select two different maps.",
      "ngeo_error_argument"
    )
  }
  semantics <- x$measures$spatial_semantics[c(outcome, predictor)]
  if (any(semantics != "intensive")) {
    .ngeo_abort(
      "Support-aware regression currently requires two intensive maps.",
      "ngeo_error_measure"
    )
  }
  c(outcome = outcome, predictor = predictor)
}

.ngeo_fit_support_effect <- function(values, support) {
  keep <- is.finite(values[, 1L]) &
    is.finite(values[, 2L]) &
    is.finite(support) & support > 0
  if (sum(keep) < 3L ||
      stats::sd(values[keep, 2L]) <= sqrt(.Machine$double.eps)) {
    .ngeo_abort(
      "A support-aware effect requires at least three variable target values.",
      "ngeo_error_model"
    )
  }
  frame <- data.frame(
    outcome = values[keep, 1L],
    predictor = values[keep, 2L],
    support = support[keep]
  )
  fit <- stats::lm(outcome ~ predictor, data = frame, weights = support)
  coefficients <- summary(fit)$coefficients
  c(
    estimate = coefficients["predictor", "Estimate"],
    standard_error = coefficients["predictor", "Std. Error"],
    statistic = coefficients["predictor", "t value"],
    p_value = coefficients["predictor", "Pr(>|t|)"],
    n_target = sum(keep)
  )
}

#' Estimate an effect across declared atlases
#'
#' The same two source maps are changed to every declared support before a
#' support-weighted target-level regression is fitted. Coefficients are
#' compared, not claimed to be parcellation invariant.
#'
#' @param x Source `ngeo` dataset.
#' @param support_maps Source-to-atlas support maps.
#' @param targets Matching target templates.
#' @param outcome Outcome map.
#' @param predictor Predictor map.
#' @param allocation Extensive overlap policy passed through for API
#'   consistency; current maps must be intensive.
#' @param confidence Atlas-specific and consensus confidence level.
#' @param bootstrap Optional common-source paired-value bootstrap replicates.
#' @param seed Reproducible bootstrap seed.
#'
#' @return An `ngeo_atlas_robust_effect`.
#' @export
ngeo_atlas_robust_effect <- function(
    x,
    support_maps,
    targets,
    outcome,
    predictor,
    allocation = c("error", "normalize"),
    confidence = 0.95,
    bootstrap = 0L,
    seed = NULL) {
  ngeo_validate(x, "strict")
  allocation <- match.arg(allocation)
  if (!is.numeric(confidence) || length(confidence) != 1L ||
      is.na(confidence) || confidence <= 0 || confidence >= 1) {
    .ngeo_abort(
      "`confidence` must lie strictly between zero and one.",
      "ngeo_error_argument"
    )
  }
  bootstrap <- .ngeo_as_integer(bootstrap, "bootstrap")
  if (length(bootstrap) != 1L || bootstrap < 0L) {
    .ngeo_abort(
      "`bootstrap` must be one non-negative integer.",
      "ngeo_error_argument"
    )
  }
  selected <- .ngeo_support_model_maps(x, outcome, predictor)
  if (!is.list(support_maps) || !length(support_maps)) {
    .ngeo_abort(
      "`support_maps` must be a non-empty list.",
      "ngeo_error_argument"
    )
  }
  if (inherits(targets, "ngeo")) {
    targets <- rep(list(targets), length(support_maps))
  }
  if (!is.list(targets) || length(targets) != length(support_maps)) {
    .ngeo_abort(
      "`targets` must align with `support_maps`.",
      "ngeo_error_alignment"
    )
  }
  source_hash <- ngeo_domain_hash(x)
  estimates <- lapply(seq_along(support_maps), function(i) {
    map <- support_maps[[i]]
    target <- targets[[i]]
    .ngeo_validate_support_domains(x, target, map)
    if (!identical(map$coverage, "complete")) {
      .ngeo_abort(
        "Atlas-robust effects require complete source coverage.",
        "ngeo_error_coverage"
      )
    }
    changed <- ngeo_change_support(
      x,
      target,
      map,
      maps = unname(selected),
      allocation = allocation
    )
    support <- map$target_support %||%
      as.numeric(map$operator %*% .ngeo_builder_support(x))
    .ngeo_fit_support_effect(changed$values, support)
  })
  estimate_matrix <- do.call(rbind, estimates)
  atlas_name <- names(support_maps)
  if (is.null(atlas_name) || any(!nzchar(atlas_name))) {
    atlas_name <- paste0("atlas_", seq_along(support_maps))
  }
  estimates <- data.frame(
    atlas = atlas_name,
    estimate_matrix,
    adjusted_p_value = stats::p.adjust(
      estimate_matrix[, "p_value"],
      method = "BH"
    ),
    support_map_hash = vapply(
      support_maps,
      ngeo_support_map_hash,
      character(1)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  critical <- stats::qt(
    1 - (1 - confidence) / 2,
    df = pmax(1, estimates$n_target - 2)
  )
  estimates$confidence_lower <-
    estimates$estimate - critical * estimates$standard_error
  estimates$confidence_upper <-
    estimates$estimate + critical * estimates$standard_error
  meta_analysis <- if (nrow(estimates) >= 2L) {
    ngeo_cross_atlas_consensus(
      estimates,
      method = "random",
      confidence = confidence
    )
  } else {
    NULL
  }
  bootstrap_result <- NULL
  if (bootstrap > 0L) {
    bootstrap_result <- .ngeo_with_seed(seed, function() {
      draw <- matrix(
        NA_real_,
        nrow = bootstrap,
        ncol = length(support_maps)
      )
      for (simulation in seq_len(bootstrap)) {
        index <- sample(
          seq_len(nrow(x$values)),
          replace = TRUE
        )
        current <- x
        current$values[, unname(selected)] <-
          x$values[index, unname(selected), drop = FALSE]
        draw[simulation, ] <- ngeo_atlas_robust_effect(
          current,
          support_maps,
          targets,
          outcome,
          predictor,
          allocation = allocation,
          confidence = confidence,
          bootstrap = 0L
        )$estimates$estimate
      }
      colnames(draw) <- atlas_name
      list(
        estimates = draw,
        consensus = apply(draw, 1L, stats::median),
        consensus_interval = stats::quantile(
          apply(draw, 1L, stats::median),
          probs = c(
            (1 - confidence) / 2,
            1 - (1 - confidence) / 2
          ),
          names = FALSE
        )
      )
    })
  }
  result <- list(
    estimates = estimates,
    consensus = c(
      median = stats::median(estimates$estimate),
      minimum = min(estimates$estimate),
      maximum = max(estimates$estimate),
      range = diff(range(estimates$estimate)),
      mad = stats::mad(estimates$estimate)
    ),
    outcome = x$maps$name[selected[["outcome"]]],
    predictor = x$maps$name[selected[["predictor"]]],
    source_domain_hash = source_hash,
    meta_analysis = meta_analysis,
    leave_one_out = if (is.null(meta_analysis)) {
      NULL
    } else {
      meta_analysis$leave_one_out
    },
    bootstrap = bootstrap_result,
    bootstrap_replicates = bootstrap,
    seed = if (bootstrap > 0L) .ngeo_seed(seed) else NULL,
    confidence = confidence,
    claim = "atlas-robust comparison; not local parcellation invariance"
  )
  class(result) <- "ngeo_atlas_robust_effect"
  result
}

#' @export
print.ngeo_atlas_robust_effect <- function(x, ...) {
  cat(
    "<ngeo_atlas_robust_effect>\n",
    "  predictor: ", x$predictor, "\n",
    "  outcome: ", x$outcome, "\n",
    "  atlases: ", nrow(x$estimates), "\n",
    "  median effect: ", format(x$consensus[["median"]], digits = 5L), "\n",
    "  effect range: ", format(x$consensus[["range"]], digits = 5L), "\n",
    sep = ""
  )
  invisible(x)
}

#' Common-source permutation test across support maps
#'
#' One source-domain permutation is reused across every atlas in a replicate,
#' preserving comparability among atlas-specific null statistics.
#'
#' @inheritParams ngeo_atlas_robust_effect
#' @param statistic Target-level correlation or support-weighted slope.
#' @param nsim Number of permutations.
#' @param seed Reproducible seed.
#' @param adjustment Multiple-testing adjustment for atlas-specific p-values.
#'
#' @return An `ngeo_support_test`.
#' @export
ngeo_support_test <- function(
    x,
    support_maps,
    targets,
    outcome,
    predictor,
    statistic = c("correlation", "slope"),
    nsim = 999L,
    seed = NULL,
    adjustment = "BH") {
  ngeo_validate(x, "strict")
  statistic <- match.arg(statistic)
  selected <- .ngeo_support_model_maps(x, outcome, predictor)
  nsim <- .ngeo_nsim(nsim)
  if (inherits(targets, "ngeo")) {
    targets <- rep(list(targets), length(support_maps))
  }
  if (!is.list(support_maps) || !length(support_maps) ||
      !is.list(targets) ||
      length(targets) != length(support_maps)) {
    .ngeo_abort(
      "`support_maps` and `targets` must be aligned non-empty lists.",
      "ngeo_error_alignment"
    )
  }
  for (i in seq_along(support_maps)) {
    .ngeo_validate_support_domains(
      x, targets[[i]], support_maps[[i]]
    )
    if (!identical(support_maps[[i]]$coverage, "complete")) {
      .ngeo_abort(
        "Permutation comparison requires complete support maps.",
        "ngeo_error_coverage"
      )
    }
  }
  compute <- function(dataset) {
    vapply(seq_along(support_maps), function(i) {
      changed <- ngeo_change_support(
        dataset,
        targets[[i]],
        support_maps[[i]],
        maps = unname(selected)
      )
      values <- changed$values
      if (identical(statistic, "correlation")) {
        stats::cor(values[, 1L], values[, 2L])
      } else {
        support <- support_maps[[i]]$target_support %||%
          as.numeric(
            support_maps[[i]]$operator %*%
              (support_maps[[i]]$source_support %||%
                rep.int(1, ncol(support_maps[[i]]$operator)))
          )
        .ngeo_fit_support_effect(values, support)[["estimate"]]
      }
    }, numeric(1))
  }
  observed <- compute(x)
  simulated <- .ngeo_with_seed(seed, function() {
    result <- matrix(
      NA_real_,
      nrow = nsim,
      ncol = length(support_maps)
    )
    for (simulation in seq_len(nsim)) {
      permuted <- x
      permuted$values[, selected[["predictor"]]] <-
        sample(x$values[, selected[["predictor"]]])
      result[simulation, ] <- compute(permuted)
    }
    result
  })
  p_value <- vapply(seq_along(observed), function(i) {
    (1 + sum(abs(simulated[, i]) >= abs(observed[[i]]))) /
      (nsim + 1)
  }, numeric(1))
  atlas_name <- names(support_maps)
  if (is.null(atlas_name) || any(!nzchar(atlas_name))) {
    atlas_name <- paste0("atlas_", seq_along(support_maps))
  }
  estimates <- data.frame(
    atlas = atlas_name,
    statistic = observed,
    p_value = p_value,
    adjusted_p_value = stats::p.adjust(p_value, method = adjustment),
    support_map_hash = vapply(
      support_maps,
      ngeo_support_map_hash,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  result <- list(
    estimates = estimates,
    simulated = simulated,
    statistic = statistic,
    nsim = nsim,
    seed = .ngeo_seed(seed),
    adjustment = adjustment,
    source_domain_hash = ngeo_domain_hash(x),
    permutation_domain = "common_source",
    claim = "common-source atlas comparison; not a spatially constrained null"
  )
  class(result) <- "ngeo_support_test"
  result
}

#' @export
print.ngeo_support_test <- function(x, ...) {
  cat(
    "<ngeo_support_test>\n",
    "  statistic: ", x$statistic, "\n",
    "  atlases: ", nrow(x$estimates), "\n",
    "  permutations: ", x$nsim, "\n",
    "  permutation domain: ", x$permutation_domain, "\n",
    sep = ""
  )
  invisible(x)
}

.ngeo_support_argmax <- function(x) {
  entries <- Matrix::summary(x$operator)
  result <- rep.int(NA_character_, ncol(x$operator))
  if (!nrow(entries)) {
    return(result)
  }
  split_rows <- split(seq_len(nrow(entries)), entries$j)
  for (source in names(split_rows)) {
    current <- split_rows[[source]]
    best <- current[which.max(entries$x[current])]
    result[as.integer(source)] <- x$target_element_id[entries$i[[best]]]
  }
  result
}

#' Summarize boundary and assignment sensitivity among support maps
#'
#' @param support_maps Two or more maps on one ordered source domain.
#' @param reference Reference map index.
#' @param source_support Optional common source support.
#'
#' @return A data frame of assignment disagreement and entropy change.
#' @export
ngeo_boundary_sensitivity <- function(
    support_maps,
    reference = 1L,
    source_support = NULL) {
  if (!is.list(support_maps) || length(support_maps) < 2L) {
    .ngeo_abort(
      "`support_maps` must contain at least two maps.",
      "ngeo_error_argument"
    )
  }
  lapply(support_maps, ngeo_validate_support_map)
  source_hash <- vapply(
    support_maps,
    function(map) map$source_domain_hash,
    character(1)
  )
  if (length(unique(source_hash)) != 1L ||
      !all(vapply(
        support_maps[-1L],
        function(map) identical(
          map$source_element_id,
          support_maps[[1L]]$source_element_id
        ),
        logical(1)
      ))) {
    .ngeo_abort(
      "Boundary sensitivity requires one ordered source domain.",
      "ngeo_error_domain_mismatch"
    )
  }
  reference <- .ngeo_as_integer(reference, "reference")
  if (length(reference) != 1L ||
      reference < 1L || reference > length(support_maps)) {
    .ngeo_abort("`reference` is out of range.", "ngeo_error_argument")
  }
  support <- source_support %||%
    support_maps[[reference]]$source_support %||%
    rep.int(1, ncol(support_maps[[reference]]$operator))
  if (!is.numeric(support) ||
      length(support) != ncol(support_maps[[reference]]$operator) ||
      anyNA(support) || any(!is.finite(support)) ||
      any(support <= 0)) {
    .ngeo_abort(
      "`source_support` must be positive and source-aligned.",
      "ngeo_error_support"
    )
  }
  reference_assignment <- .ngeo_support_argmax(
    support_maps[[reference]]
  )
  reference_entropy <- ngeo_support_entropy(
    support_maps[[reference]]
  )
  name <- names(support_maps)
  if (is.null(name) || any(!nzchar(name))) {
    name <- paste0("map_", seq_along(support_maps))
  }
  do.call(rbind, lapply(seq_along(support_maps), function(i) {
    assignment <- .ngeo_support_argmax(support_maps[[i]])
    comparable <- !is.na(reference_assignment) & !is.na(assignment)
    disagreement <- comparable &
      assignment != reference_assignment
    entropy <- ngeo_support_entropy(support_maps[[i]])
    data.frame(
      support_map = name[[i]],
      assignment_disagreement_fraction = if (any(comparable)) {
        mean(disagreement[comparable])
      } else {
        NA_real_
      },
      support_weighted_disagreement = if (any(comparable)) {
        sum(support[disagreement]) / sum(support[comparable])
      } else {
        NA_real_
      },
      mean_entropy_change = mean(
        entropy - reference_entropy,
        na.rm = TRUE
      ),
      support_map_hash = ngeo_support_map_hash(support_maps[[i]]),
      stringsAsFactors = FALSE
    )
  }))
}
