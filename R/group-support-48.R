.ngeo_support_feature_hash <- function(feature) {
  candidates <- character()
  if (is.data.frame(feature$endpoints) &&
      "support_hash" %in% names(feature$endpoints)) {
    candidates <- unique(as.character(feature$endpoints$support_hash))
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  }
  provenance_hash <- feature$history$support_hash
  if (!is.null(provenance_hash)) {
    candidates <- unique(c(candidates, as.character(provenance_hash)))
  }
  if (length(candidates) != 1L) {
    .ngeo_abort(
      "Every support feature object must declare exactly one support hash.",
      "ngeo_error_support_family"
    )
  }
  candidates[[1L]]
}

.ngeo_combine_support_features <- function(features) {
  support_id <- names(features)
  if (!is.list(features) || length(features) < 2L ||
      is.null(support_id) || anyNA(support_id) ||
      any(!nzchar(support_id)) || anyDuplicated(support_id)) {
    .ngeo_abort(
      "Support-family features must be a named list with at least two members.",
      "ngeo_error_support_family"
    )
  }
  valid <- vapply(features, inherits, logical(1), "ngeo_subject_features")
  if (any(!valid)) {
    .ngeo_abort("Every support member must be `ngeo_subject_features`.",
                "ngeo_error_support_family")
  }
  reference_units <- as.character(features[[1L]]$unit$unit_id)
  for (position in seq_along(features)) {
    current <- features[[position]]
    if (!is.matrix(current$values) || !is.numeric(current$values) ||
        !is.data.frame(current$unit) ||
        !"unit_id" %in% names(current$unit) ||
        !identical(as.character(current$unit$unit_id), reference_units) ||
        nrow(current$values) != length(reference_units) ||
        !is.data.frame(current$endpoints) ||
        nrow(current$endpoints) != ncol(current$values)) {
      .ngeo_abort(
        "Every support must have the same ordered independent-unit rows.",
        "ngeo_error_alignment"
      )
    }
  }
  hashes <- vapply(features, .ngeo_support_feature_hash, character(1))
  endpoint_tables <- vector("list", length(features))
  values <- vector("list", length(features))
  for (position in seq_along(features)) {
    current <- features[[position]]
    endpoints <- current$endpoints
    if (!"endpoint_id" %in% names(endpoints) ||
        anyNA(endpoints$endpoint_id) || any(!nzchar(endpoints$endpoint_id)) ||
        anyDuplicated(endpoints$endpoint_id)) {
      .ngeo_abort("Support endpoints require unique identities.",
                  "ngeo_error_support_family")
    }
    endpoints$support_endpoint_id <- as.character(endpoints$endpoint_id)
    endpoints$endpoint_id <- paste(
      support_id[[position]], endpoints$support_endpoint_id, sep = "::"
    )
    endpoints$support_id <- support_id[[position]]
    endpoints$support_hash <- hashes[[position]]
    endpoint_tables[[position]] <- endpoints
    values[[position]] <- current$values
    colnames(values[[position]]) <- endpoints$endpoint_id
  }
  combined_endpoints <- do.call(rbind, endpoint_tables)
  rownames(combined_endpoints) <- NULL
  combined <- list(
    values = do.call(cbind, values),
    unit = features[[1L]]$unit,
    endpoints = combined_endpoints,
    diagnostics = list(
      support_family = support_id,
      source_diagnostics = lapply(features, `[[`, "diagnostics")
    ),
    history = list(
      method = "declared_support_family_endpoint_binding",
      support_id = support_id,
      support_hash = hashes,
      source_provenance = lapply(features, `[[`, "history")
    )
  )
  class(combined) <- "ngeo_subject_features"
  list(
    combined = combined,
    support_id = support_id,
    support_hash = hashes,
    sources = features
  )
}

.ngeo_semantic_field <- function(endpoints, name, default = "none") {
  if (!name %in% names(endpoints)) return(rep.int(default, nrow(endpoints)))
  value <- as.character(endpoints[[name]])
  value[is.na(value) | !nzchar(value)] <- default
  value
}

.ngeo_support_semantic_key <- function(tests) {
  scale_type <- .ngeo_semantic_field(tests, "scale_type", "unmatched")
  band <- .ngeo_semantic_field(tests, "band")
  mode_count <- .ngeo_semantic_field(tests, "mode_count", "unknown")
  eigen_min <- .ngeo_semantic_field(tests, "eigenvalue_min", "unknown")
  eigen_max <- .ngeo_semantic_field(tests, "eigenvalue_max", "unknown")
  band_definition <- ifelse(
    scale_type == "rank_matched",
    paste("rank", band, mode_count, sep = ":"),
    ifelse(
      grepl("physical", scale_type, fixed = TRUE),
      paste("physical", band, eigen_min, eigen_max, sep = ":"),
      paste("unmatched", tests$support_id,
            tests$support_endpoint_id, sep = ":")
    )
  )
  do.call(paste, c(list(
    .ngeo_semantic_field(tests, "estimand"),
    .ngeo_semantic_field(tests, "layer_x"),
    .ngeo_semantic_field(tests, "layer_y"),
    .ngeo_semantic_field(tests, "direction"),
    .ngeo_semantic_field(tests, "component"),
    scale_type,
    band_definition,
    .ngeo_semantic_field(tests, "transform")
  ), sep = "\u001f"))
}

.ngeo_support_stability <- function(tests) {
  keys <- unique(tests$semantic_key)
  output <- list()
  for (key in keys) {
    current <- tests[tests$semantic_key == key, , drop = FALSE]
    if (nrow(current) < 2L ||
        length(unique(current$support_id)) != nrow(current) ||
        any(current$scale_type == "unmatched") ||
        any(!is.finite(current$coefficient))) next
    effects <- current$coefficient
    overall_mean <- mean(effects)
    loso <- vapply(seq_along(effects), function(i) {
      abs(mean(effects[-i]) - overall_mean)
    }, numeric(1))
    nonzero_sign <- sign(effects[effects != 0])
    direction_agreement <- length(unique(nonzero_sign)) <= 1L
    output[[length(output) + 1L]] <- data.frame(
      semantic_key = key,
      estimand = current$estimand[[1L]],
      layer_x = current$layer_x[[1L]],
      layer_y = current$layer_y[[1L]],
      direction = current$direction[[1L]],
      component = current$component[[1L]],
      band = current$band[[1L]],
      scale_comparability = current$scale_type[[1L]],
      supports = nrow(current),
      direction_agreement = direction_agreement,
      effect_median = stats::median(effects),
      effect_min = min(effects),
      effect_max = max(effects),
      effect_range = diff(range(effects)),
      effect_iqr = stats::IQR(effects),
      effect_sd = stats::sd(effects),
      maximum_deviation = max(abs(effects - stats::median(effects))),
      significance_persistence = mean(current$p_maxT <= 0.05),
      max_loso_influence = max(loso),
      driving_support = if (max(loso) <= sqrt(.Machine$double.eps))
        NA_character_ else current$support_id[[which.max(loso)]],
      stringsAsFactors = FALSE
    )
  }
  if (length(output)) return(do.call(rbind, output))
  data.frame(
    semantic_key = character(), estimand = character(),
    layer_x = character(), layer_y = character(), direction = character(),
    component = character(), band = character(),
    scale_comparability = character(), supports = integer(),
    direction_agreement = logical(), effect_median = numeric(),
    effect_min = numeric(), effect_max = numeric(), effect_range = numeric(),
    effect_iqr = numeric(), effect_sd = numeric(),
    maximum_deviation = numeric(), significance_persistence = numeric(),
    max_loso_influence = numeric(), driving_support = character(),
    stringsAsFactors = FALSE
  )
}

.ngeo_support_boundary_table <- function(sources, support_id, support_hash) {
  diagnostics <- lapply(sources, function(feature) {
    feature$diagnostics$boundary_sensitivity
  })
  data.frame(
    support_id = support_id,
    support_hash = unname(support_hash),
    available = !vapply(diagnostics, is.null, logical(1)),
    diagnostic_hash = vapply(diagnostics, function(value) {
      if (is.null(value)) return(NA_character_)
      .ngeo_layer_digest(value)
    }, character(1)),
    diagnostic = I(diagnostics),
    stringsAsFactors = FALSE
  )
}

.ngeo_group_support_test <- function(
    features, data, model, test, exchangeability, family, transform,
    adjustment, omnibus, missing, retain_null, workers, budget) {
  bound <- .ngeo_combine_support_features(features)
  result <- ngeo_group_test(
    bound$combined, data, model, test, exchangeability,
    family = family, transform = transform, adjustment = adjustment,
    omnibus = omnibus, missing = missing, retain_null = retain_null,
    workers = workers, budget = budget
  )
  result$tests$semantic_key <- .ngeo_support_semantic_key(result$tests)
  stability <- .ngeo_support_stability(result$tests)
  support_table <- data.frame(
    support_id = bound$support_id,
    support_hash = unname(bound$support_hash),
    endpoints = vapply(features, function(feature) ncol(feature$values),
                       integer(1)),
    stringsAsFactors = FALSE
  )
  family_identity <- list(
    analysis_order = bound$support_id,
    support_hash = bound$support_hash,
    endpoints = result$tests[c(
      "endpoint_id", "support_id", "support_hash", "semantic_key",
      "maxT_family"
    )]
  )
  result$support <- list(
    analysis_order = bound$support_id,
    members = support_table,
    stability = stability,
    boundary = .ngeo_support_boundary_table(
      bound$sources, bound$support_id, bound$support_hash
    ),
    schedule_hash = exchangeability$schedule_hash,
    family_hash = .ngeo_layer_digest(family_identity),
    diagnostics = list(
      unmatched_endpoints = sum(result$tests$scale_type == "unmatched"),
      automatic_stability_classification = FALSE,
      support_dispersion_combined_with_sampling_variance = FALSE,
      boundary_p_values_reused = FALSE
    )
  )
  result$diagnostics$common_schedule_all_supports <- TRUE
  result$diagnostics$complete_family_across_supports <- TRUE
  result$history$support_family_hash <- result$support$family_hash
  result$history$analysis_order <- bound$support_id
  result$history$operation_order <- c(
    "change_support", "build_operator", "build_basis",
    "derive_endpoints", "common_subject_schedule"
  )
  result$claim <- paste(
    result$claim,
    "Support summaries apply only to the named, hashed declared support family",
    "and are not parcellation-invariant or a combined support/sampling variance."
  )
  result
}
