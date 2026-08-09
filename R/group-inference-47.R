.ngeo_align_subject_design <- function(features, data, model, missing) {
  if (!inherits(features, "ngeo_subject_features") ||
      !is.matrix(features$values) || !is.numeric(features$values) ||
      !is.data.frame(features$unit) ||
      !"unit_id" %in% names(features$unit) ||
      !is.data.frame(features$endpoints) ||
      nrow(features$unit) != nrow(features$values) ||
      nrow(features$endpoints) != ncol(features$values)) {
    .ngeo_abort("`features` must be one aligned `ngeo_subject_features` object.",
                "ngeo_error_subject_features")
  }
  unit_id <- as.character(features$unit$unit_id)
  if (anyNA(unit_id) || any(!nzchar(unit_id)) || anyDuplicated(unit_id)) {
    .ngeo_abort("Feature rows must have unique independent-unit identifiers.",
                "ngeo_error_independent_unit")
  }
  if (!"endpoint_id" %in% names(features$endpoints) ||
      anyNA(features$endpoints$endpoint_id) ||
      any(!nzchar(features$endpoints$endpoint_id)) ||
      anyDuplicated(features$endpoints$endpoint_id)) {
    .ngeo_abort("Feature columns require unique non-empty endpoint identities.",
                "ngeo_error_subject_features")
  }
  colnames(features$values) <- as.character(features$endpoints$endpoint_id)
  if (!is.data.frame(data) || !"unit_id" %in% names(data)) {
    .ngeo_abort("`data` must contain one `unit_id` column.",
                "ngeo_error_design")
  }
  data$unit_id <- as.character(data$unit_id)
  if (anyNA(data$unit_id) || any(!nzchar(data$unit_id)) ||
      anyDuplicated(data$unit_id)) {
    .ngeo_abort("Design unit identifiers must be unique and non-missing.",
                "ngeo_error_design")
  }
  if (!setequal(data$unit_id, unit_id)) {
    .ngeo_abort("Feature and design unit identifiers do not match exactly.",
                "ngeo_error_alignment")
  }
  data <- data[match(unit_id, data$unit_id), , drop = FALSE]
  if ("subject_id" %in% names(data) && anyDuplicated(data$subject_id)) {
    .ngeo_abort(
      "Repeated subject rows require a precomputed subject summary or paired contrast.",
      "ngeo_error_independent_unit"
    )
  }
  if (!inherits(model, "formula") || length(model) != 2L) {
    .ngeo_abort("`model` must be a one-sided formula.",
                "ngeo_error_design")
  }
  variables <- all.vars(model)
  missing_variables <- setdiff(variables, names(data))
  if (length(missing_variables)) {
    .ngeo_abort(
      sprintf("Model variables are missing: %s.",
              paste(missing_variables, collapse = ", ")),
      "ngeo_error_design"
    )
  }
  if (any(vapply(data[variables], anyNA, logical(1)))) {
    .ngeo_abort("Design covariates may not contain missing values.",
                "ngeo_error_missing")
  }
  finite <- apply(features$values, 1L, function(row) all(is.finite(row)))
  if (identical(missing, "error") && any(!finite)) {
    .ngeo_abort("Subject endpoints contain missing or non-finite values.",
                "ngeo_error_missing")
  }
  kept <- if (identical(missing, "complete_family")) which(finite) else
    seq_along(unit_id)
  if (length(kept) < 3L) {
    .ngeo_abort("Too few complete independent unit remain for inference.",
                "ngeo_error_independent_unit")
  }
  list(
    values = features$values[kept, , drop = FALSE],
    unit = features$unit[kept, , drop = FALSE],
    endpoints = features$endpoints,
    data = data[kept, , drop = FALSE],
    unit_id = unit_id[kept],
    dropped = unit_id[-kept]
  )
}

.ngeo_transform_subject_features <- function(values, endpoints, transform) {
  transformations <- rep.int("none", ncol(values))
  if (identical(transform, "auto")) {
    bounded <- if ("bounds" %in% names(endpoints))
      endpoints$bounds == "[-1,1]" else rep.int(FALSE, ncol(values))
    recommended <- if ("recommended_transform" %in% names(endpoints))
      endpoints$recommended_transform == "fisher_z" else
        rep.int(FALSE, ncol(values))
    transformations[bounded | recommended] <- "fisher_z"
  }
  transformed <- values
  for (column in which(transformations == "fisher_z")) {
    if (any(values[, column] < -1 | values[, column] > 1)) {
      .ngeo_abort("Fisher-z endpoints must lie within [-1, 1].",
                  "ngeo_error_measure")
    }
    tolerance <- sqrt(.Machine$double.eps)
    clipped <- pmin(1 - tolerance, pmax(-1 + tolerance, values[, column]))
    transformed[, column] <- atanh(clipped)
  }
  list(values = transformed, transform = transformations)
}

.ngeo_test_contrast <- function(model_matrix, terms, test) {
  columns <- colnames(model_matrix)
  if (is.character(test) && length(test) == 1L && !is.na(test) &&
      nzchar(test)) {
    if (identical(test, "(Intercept)")) {
      selected <- which(attr(model_matrix, "assign") == 0L)
    } else {
      labels <- attr(terms, "term.labels")
      term <- match(test, labels)
      if (is.na(term)) {
        .ngeo_abort("`test` does not name one model term.",
                    "ngeo_error_test_term")
      }
      selected <- which(attr(model_matrix, "assign") == term)
    }
    if (!length(selected)) {
      .ngeo_abort("The tested term has no model-matrix columns.",
                  "ngeo_error_test_term")
    }
    reduced <- model_matrix[, -selected, drop = FALSE]
    contrast <- if (length(selected) == 1L) {
      value <- numeric(ncol(model_matrix))
      value[selected] <- 1
      value
    } else NULL
    return(list(
      label = test, columns = selected, df = length(selected),
      contrast = contrast, reduced = reduced
    ))
  }
  if (is.numeric(test) && length(test) == ncol(model_matrix) &&
      !anyNA(test) && all(is.finite(test)) && any(test != 0)) {
    if (is.null(names(test)) || !setequal(names(test), columns)) {
      .ngeo_abort("A numeric contrast must name every model-matrix column.",
                  "ngeo_error_test_term")
    }
    contrast <- as.numeric(test[columns])
    q <- qr(matrix(contrast, ncol = 1L))
    null_basis <- qr.Q(q, complete = TRUE)[, -1L, drop = FALSE]
    return(list(
      label = "named_contrast", columns = which(contrast != 0), df = 1L,
      contrast = contrast,
      reduced = model_matrix %*% null_basis
    ))
  }
  .ngeo_abort("`test` must name one term or define one named contrast.",
              "ngeo_error_test_term")
}

.ngeo_model_matrices <- function(data, model, test) {
  frame <- tryCatch(
    stats::model.frame(model, data = data, na.action = stats::na.fail),
    error = function(error) {
      .ngeo_abort(conditionMessage(error), "ngeo_error_design")
    }
  )
  terms <- stats::terms(frame)
  full <- stats::model.matrix(terms, frame)
  full_qr <- qr(full)
  if (full_qr$rank != ncol(full)) {
    .ngeo_abort("The full subject-level design is rank deficient.",
                "ngeo_error_design_rank")
  }
  test_spec <- .ngeo_test_contrast(full, terms, test)
  reduced <- test_spec$reduced
  reduced_qr <- if (ncol(reduced)) qr(reduced) else NULL
  if (ncol(reduced) && reduced_qr$rank != ncol(reduced)) {
    .ngeo_abort("The reduced subject-level design is rank deficient.",
                "ngeo_error_design_rank")
  }
  residual_df <- nrow(full) - ncol(full)
  if (residual_df < 1L) {
    .ngeo_abort("The full model has no residual degrees of freedom.",
                "ngeo_error_design_rank")
  }
  inverse <- chol2inv(qr.R(full_qr))
  list(
    full = full, reduced = reduced,
    full_qr = full_qr, reduced_qr = reduced_qr,
    test = test_spec, residual_df = residual_df,
    inverse_crossproduct = inverse,
    terms = terms
  )
}

.ngeo_reduced_fit <- function(design, values) {
  if (!ncol(design$reduced)) {
    return(list(fitted = matrix(0, nrow(values), ncol(values)),
                residuals = values))
  }
  list(
    fitted = qr.fitted(design$reduced_qr, values),
    residuals = qr.resid(design$reduced_qr, values)
  )
}

.ngeo_group_statistics <- function(values, design, details = FALSE) {
  coefficients <- qr.coef(design$full_qr, values)
  residuals <- qr.resid(design$full_qr, values)
  sse_full <- colSums(residuals^2)
  reduced <- .ngeo_reduced_fit(design, values)
  sse_reduced <- colSums(reduced$residuals^2)
  partial_r2 <- pmax(0, pmin(1, (sse_reduced - sse_full) / sse_reduced))
  if (design$test$df == 1L) {
    contrast <- design$test$contrast
    effect <- as.numeric(crossprod(contrast, coefficients))
    variance_factor <- as.numeric(crossprod(
      contrast, design$inverse_crossproduct %*% contrast
    ))
    standard_error <- sqrt(
      (sse_full / design$residual_df) * variance_factor
    )
    statistic <- effect / standard_error
    statistic_type <- "t"
  } else {
    effect <- rep.int(NA_real_, ncol(values))
    standard_error <- rep.int(NA_real_, ncol(values))
    statistic <- ((sse_reduced - sse_full) / design$test$df) /
      (sse_full / design$residual_df)
    statistic_type <- "F"
  }
  output <- list(statistic = statistic)
  if (details) {
    output <- c(output, list(
      statistic_type = statistic_type,
      effect = effect,
      standard_error = standard_error,
      partial_r2 = partial_r2,
      sse_full = sse_full,
      sse_reduced = sse_reduced
    ))
  }
  output
}

.ngeo_exchangeability_design <- function(exchangeability, unit_id, design) {
  if (!inherits(exchangeability, "ngeo_exchangeability") ||
      !identical(exchangeability$unit_id, unit_id) ||
      !identical(colnames(exchangeability$schedule), unit_id)) {
    .ngeo_abort(
      "Exchangeability unit must exactly equal the retained feature unit.",
      "ngeo_error_alignment"
    )
  }
  if (identical(exchangeability$scheme, "within_block")) {
    blocks <- unname(exchangeability$blocks[unit_id])
    for (block in unique(blocks)) {
      rows <- which(blocks == block)
      full_rank <- qr(design$full[rows, , drop = FALSE])$rank
      reduced_rank <- if (ncol(design$reduced))
        qr(design$reduced[rows, , drop = FALSE])$rank else 0L
      if (full_rank <= reduced_rank) {
        .ngeo_abort(
          "The tested term has no identifiable variation within every block.",
          "ngeo_error_exchangeability_design"
        )
      }
    }
  }
  if (identical(exchangeability$transformation, "sign_flip") &&
      !identical(design$test$df, 1L)) {
    .ngeo_abort("Sign flipping supports one-df paired/one-sample tests.",
                "ngeo_error_exchangeability_design")
  }
  invisible(TRUE)
}

.ngeo_family_labels <- function(family, endpoints) {
  p <- nrow(endpoints)
  if (is.null(family)) return(rep.int("all", p))
  if (is.character(family) && length(family) == 1L &&
      family %in% names(endpoints)) {
    labels <- as.character(endpoints[[family]])
  } else if (length(family) == p && !is.list(family)) {
    labels <- as.character(family)
  } else {
    .ngeo_abort("`family` must be an endpoint metadata column or label vector.",
                "ngeo_error_family")
  }
  if (anyNA(labels) || any(!nzchar(labels))) {
    .ngeo_abort("Every endpoint must belong to one non-empty max-T family.",
                "ngeo_error_family")
  }
  labels
}

.ngeo_group_permutation_block <- function(
    rows, schedule, transformation, fitted, residuals, design,
    endpoint_chunk) {
  output <- matrix(NA_real_, length(rows), ncol(residuals))
  column_groups <- split(
    seq_len(ncol(residuals)),
    ceiling(seq_len(ncol(residuals)) / endpoint_chunk)
  )
  for (position in seq_along(rows)) {
    transformation_row <- schedule[rows[[position]], ]
    permuted_residuals <- if (identical(transformation, "sign_flip")) {
      residuals * transformation_row
    } else {
      residuals[transformation_row, , drop = FALSE]
    }
    for (columns in column_groups) {
      current <- fitted[, columns, drop = FALSE] +
        permuted_residuals[, columns, drop = FALSE]
      coefficients <- qr.coef(design$full_qr, current)
      full_residuals <- qr.resid(design$full_qr, current)
      sse_full <- colSums(full_residuals^2)
      if (design$test$df == 1L) {
        contrast <- design$test$contrast
        effect <- as.numeric(crossprod(contrast, coefficients))
        variance_factor <- as.numeric(crossprod(
          contrast, design$inverse_crossproduct %*% contrast
        ))
        output[position, columns] <- effect / sqrt(
          (sse_full / design$residual_df) * variance_factor
        )
      } else {
        reduced_residuals <- if (!ncol(design$reduced)) current else
          qr.resid(design$reduced_qr, current)
        sse_reduced <- colSums(reduced_residuals^2)
        output[position, columns] <-
          ((sse_reduced - sse_full) / design$test$df) /
          (sse_full / design$residual_df)
      }
    }
  }
  output
}

.ngeo_group_null <- function(
    values, design, exchangeability, observed, family_labels,
    adjustment, omnibus, retain_null, workers, budget) {
  budget_context <- .ngeo_budget_context(budget)
  b <- nrow(exchangeability$schedule)
  p <- ncol(values)
  family_levels <- unique(family_labels)
  .ngeo_budget_assert(budget_context, "materialized_elements", nrow(values) * p)
  if (retain_null) {
    .ngeo_budget_assert(budget_context, "materialized_elements", as.double(b) * p)
    .ngeo_budget_assert(budget_context, "memory_bytes", as.double(b) * p * 8)
    endpoint_null <- matrix(NA_real_, b, p)
  } else {
    endpoint_null <- NULL
  }
  reduced <- .ngeo_reduced_fit(design, values)
  exceedance <- integer(p)
  maxima <- matrix(
    NA_real_, b, length(family_levels),
    dimnames = list(NULL, family_levels)
  )
  omnibus_null <- matrix(
    NA_real_, b, length(omnibus), dimnames = list(NULL, omnibus)
  )
  permutation_block <- getOption("neurogeo.permutation_block", 16L)
  endpoint_chunk <- getOption("neurogeo.endpoint_chunk", 128L)
  blocks <- split(seq_len(b), ceiling(seq_len(b) / permutation_block))
  .ngeo_budget_assert(budget_context, "blocks", length(blocks))
  cluster <- NULL
  if (workers > 1L) {
    cluster <- parallel::makeCluster(min(workers, length(blocks)))
    on.exit(parallel::stopCluster(cluster), add = TRUE)
  }
  transform_statistic <- if (identical(observed$statistic_type, "t")) abs else
    identity
  observed_comparison <- transform_statistic(observed$statistic)
  group_batches <- split(
    seq_along(blocks), ceiling(seq_along(blocks) / max(1L, workers))
  )
  for (batch in group_batches) {
    .ngeo_budget_checkpoint(budget_context)
    current_blocks <- blocks[batch]
    run <- function(rows, schedule, transformation, fitted, residuals,
                    design, endpoint_chunk, block_fun) {
      block_fun(
        rows, schedule, transformation, fitted, residuals, design,
        endpoint_chunk
      )
    }
    outputs <- if (is.null(cluster)) {
      lapply(current_blocks, run,
             schedule = exchangeability$schedule,
             transformation = exchangeability$transformation,
             fitted = reduced$fitted, residuals = reduced$residuals,
             design = design, endpoint_chunk = endpoint_chunk,
             block_fun = .ngeo_group_permutation_block)
    } else {
      parallel::parLapply(
        cluster, current_blocks, run,
        schedule = exchangeability$schedule,
        transformation = exchangeability$transformation,
        fitted = reduced$fitted, residuals = reduced$residuals,
        design = design, endpoint_chunk = endpoint_chunk,
        block_fun = .ngeo_group_permutation_block
      )
    }
    for (position in seq_along(outputs)) {
      rows <- current_blocks[[position]]
      statistics <- outputs[[position]]
      comparison <- transform_statistic(statistics)
      exceedance <- exceedance + colSums(
        sweep(comparison, 2L, observed_comparison, ">=")
      )
      for (family in seq_along(family_levels)) {
        columns <- which(family_labels == family_levels[[family]])
        maxima[rows, family] <- apply(
          comparison[, columns, drop = FALSE], 1L, max
        )
      }
      if ("max" %in% omnibus) {
        omnibus_null[rows, "max"] <- apply(comparison, 1L, max)
      }
      if ("sum_sq" %in% omnibus) {
        omnibus_null[rows, "sum_sq"] <- rowSums(statistics^2)
      }
      if (retain_null) endpoint_null[rows, ] <- statistics
    }
  }
  raw <- (1 + exceedance) / (b + 1)
  adjusted <- rep.int(NA_real_, p)
  if (identical(adjustment, "maxT")) {
    for (column in seq_len(p)) {
      family <- match(family_labels[[column]], family_levels)
      adjusted[[column]] <-
        (1 + sum(maxima[, family] >= observed_comparison[[column]])) /
        (b + 1)
    }
  }
  observed_omnibus <- c(
    max = max(observed_comparison),
    sum_sq = sum(observed$statistic^2)
  )[omnibus]
  omnibus_p <- vapply(omnibus, function(name) {
    (1 + sum(omnibus_null[, name] >= observed_omnibus[[name]])) / (b + 1)
  }, numeric(1))
  list(
    raw = raw, adjusted = adjusted,
    maxima = maxima, omnibus_null = omnibus_null,
    endpoint_null = endpoint_null,
    observed_omnibus = observed_omnibus,
    omnibus_p = omnibus_p,
    family_labels = family_labels,
    family_hash = .ngeo_layer_digest(list(
      endpoint_id = colnames(values), family = family_labels
    )),
    block_size = permutation_block,
    endpoint_chunk = endpoint_chunk
  )
}

.ngeo_group_summaries <- function(raw_values, endpoints, data, test) {
  if (!is.character(test) || length(test) != 1L ||
      !test %in% names(data)) {
    return(data.frame())
  }
  group <- data[[test]]
  levels <- unique(as.character(group))
  if (length(levels) < 2L || length(levels) > 10L) return(data.frame())
  output <- vector("list", length(levels) * ncol(raw_values))
  position <- 0L
  for (level in levels) {
    rows <- which(as.character(group) == level)
    for (column in seq_len(ncol(raw_values))) {
      position <- position + 1L
      output[[position]] <- data.frame(
        endpoint_id = endpoints$endpoint_id[[column]],
        group = level,
        n = length(rows),
        mean = mean(raw_values[rows, column]),
        sd = stats::sd(raw_values[rows, column]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, output)
}

.ngeo_design_diagnostics <- function(design, group_summaries) {
  q <- qr.Q(design$full_qr)
  leverage <- rowSums(q[, seq_len(ncol(design$full)), drop = FALSE]^2)
  variance_ratio <- NA_real_
  if (nrow(group_summaries)) {
    ratios <- vapply(split(
      group_summaries$sd^2, group_summaries$endpoint_id
    ), function(current) {
      current <- current[is.finite(current) & current > 0]
      if (length(current) < 2L) return(NA_real_)
      max(current) / min(current)
    }, numeric(1))
    if (any(is.finite(ratios))) variance_ratio <- max(ratios, na.rm = TRUE)
  }
  if (is.finite(variance_ratio) && variance_ratio > 10) {
    .ngeo_warn(
      "Large group-wise endpoint variance differences were detected; permutation validity still assumes exchangeable residuals.",
      "ngeo_warning_heteroscedasticity"
    )
  }
  list(
    full_rank = design$full_qr$rank,
    full_columns = ncol(design$full),
    reduced_rank = if (is.null(design$reduced_qr)) 0L else
      design$reduced_qr$rank,
    residual_df = design$residual_df,
    maximum_leverage = max(leverage),
    leverage = leverage,
    maximum_group_variance_ratio = variance_ratio,
    heteroscedasticity_solved = FALSE
  )
}

#' Test subject-level spatial endpoints by Freedman--Lane permutation
#'
#' Complete independent-subject rows are transformed together across every
#' endpoint. The default streams endpoint exceedances, family maxima, and
#' omnibus nulls without retaining a full permutation-by-endpoint matrix.
#'
#' @param features One `ngeo_subject_features` object or a named declared
#'   support-family list of such objects.
#' @param data Subject design table with `unit_id`.
#' @param model One-sided full-model formula.
#' @param test Tested formula term or named one-df contrast.
#' @param exchangeability Matching `ngeo_exchangeability`.
#' @param family Optional max-T family metadata column or endpoint labels.
#' @param transform Auditable automatic Fisher-z or no transformation.
#' @param adjustment Single-step max-T or raw inference only.
#' @param omnibus Omnibus statistics to retain.
#' @param missing Complete-family deletion or error.
#' @param retain_null Whether to retain the bounded endpoint null matrix.
#' @param workers Deterministic worker count.
#' @param budget Hard execution resource limits.
#'
#' @return An `ngeo_group_result`.
#' @examples
#' \dontrun{
#' ngeo_group_test(
#'   subject_features, subject_data, outcome ~ age + group, test = "group",
#'   exchangeability = subject_exchangeability
#' )
#' }
#' @template stable-statistical-method
#' @export
ngeo_group_test <- function(
    features,
    data,
    model,
    test,
    exchangeability,
    family = NULL,
    transform = c("auto", "none"),
    adjustment = c("maxT", "none"),
    omnibus = c("max", "sum_sq"),
    missing = c("complete_family", "error"),
    retain_null = FALSE,
    workers = 1L,
    budget = ngeo_resource_budget()) {
  if (is.list(features) && !inherits(features, "ngeo_subject_features")) {
    return(.ngeo_group_support_test(
      features, data, model, test, exchangeability, family, transform,
      adjustment, omnibus, missing, retain_null, workers, budget
    ))
  }
  transform <- match.arg(transform)
  adjustment <- match.arg(adjustment)
  missing <- match.arg(missing)
  if (!is.character(omnibus) || !length(omnibus) || anyNA(omnibus) ||
      any(!omnibus %in% c("max", "sum_sq")) || anyDuplicated(omnibus)) {
    .ngeo_abort("`omnibus` must select `max` and/or `sum_sq`.",
                "ngeo_error_argument")
  }
  if (!is.logical(retain_null) || length(retain_null) != 1L ||
      is.na(retain_null)) {
    .ngeo_abort("`retain_null` must be TRUE or FALSE.",
                "ngeo_error_argument")
  }
  workers <- .ngeo_workers(workers)
  aligned <- .ngeo_align_subject_design(features, data, model, missing)
  transformed <- .ngeo_transform_subject_features(
    aligned$values, aligned$endpoints, transform
  )
  endpoint_variance <- apply(transformed$values, 2L, stats::var)
  if (any(!is.finite(endpoint_variance)) || any(endpoint_variance <= 0)) {
    .ngeo_abort("Group inference is undefined for a constant endpoint.",
                "ngeo_error_statistic")
  }
  design <- .ngeo_model_matrices(aligned$data, model, test)
  .ngeo_exchangeability_design(exchangeability, aligned$unit_id, design)
  family_labels <- .ngeo_family_labels(family, aligned$endpoints)
  observed <- .ngeo_group_statistics(
    transformed$values, design, details = TRUE
  )
  if (any(!is.finite(observed$statistic))) {
    .ngeo_abort(
      "The tested model produced a non-finite endpoint statistic.",
      "ngeo_error_statistic"
    )
  }
  null_result <- .ngeo_group_null(
    transformed$values, design, exchangeability, observed,
    family_labels, adjustment, omnibus, retain_null, workers, budget
  )
  critical <- stats::qt(0.975, design$residual_df)
  interval_low <- observed$effect - critical * observed$standard_error
  interval_high <- observed$effect + critical * observed$standard_error
  tests <- cbind(
    aligned$endpoints,
    data.frame(
      transform = transformed$transform,
      coefficient = observed$effect,
      standard_error = observed$standard_error,
      interval_low = interval_low,
      interval_high = interval_high,
      statistic = observed$statistic,
      statistic_type = observed$statistic_type,
      df_num = design$test$df,
      df_den = design$residual_df,
      partial_r2 = observed$partial_r2,
      p_raw = null_result$raw,
      p_maxT = null_result$adjusted,
      maxT_family = family_labels,
      stringsAsFactors = FALSE
    )
  )
  group_summaries <- .ngeo_group_summaries(
    aligned$values, aligned$endpoints, aligned$data, test
  )
  design_diagnostics <- .ngeo_design_diagnostics(design, group_summaries)
  omnibus_table <- data.frame(
    omnibus = omnibus,
    statistic = as.numeric(null_result$observed_omnibus),
    p_value = as.numeric(null_result$omnibus_p),
    stringsAsFactors = FALSE
  )
  null_output <- list(
    maxima = null_result$maxima,
    omnibus = null_result$omnibus_null
  )
  if (retain_null) null_output$endpoint <- null_result$endpoint_null
  unit_kind <- exchangeability$unit_kind %||% "subject"
  unit_label <- switch(
    unit_kind,
    subject = "subjects",
    site = "sites",
    spatial_block = "spatial blocks"
  )
  result <- list(
    tests = tests,
    omnibus = omnibus_table,
    support = NULL,
    design = list(
      unit_id = aligned$unit_id,
      model = paste(deparse(model), collapse = " "),
      test = design$test$label,
      full_columns = colnames(design$full),
      nuisance_columns = colnames(design$reduced),
      group_summaries = group_summaries
    ),
    exchangeability = exchangeability,
    sampling_unit = unit_kind,
    diagnostics = c(design_diagnostics, list(
      dropped_incomplete_units = aligned$dropped,
      complete_family = TRUE,
      permutations = nrow(exchangeability$schedule),
      monte_carlo_resolution = 1 / (nrow(exchangeability$schedule) + 1),
      family_hash = null_result$family_hash,
      permutation_block = null_result$block_size,
      endpoint_chunk = null_result$endpoint_chunk,
      workers = workers,
      endpoint_null_retained = retain_null,
      vertices_permuted = FALSE
    )),
    history = list(
      method = "freedman_lane",
      schedule_hash = exchangeability$schedule_hash,
      feature_provenance = features$history,
      transform = stats::setNames(
        transformed$transform, aligned$endpoints$endpoint_id
      ),
      adjustment = adjustment,
      omnibus = omnibus,
      inference_unit = paste0(
        "independent_", unit_kind
      ),
      endpoint_selection_refit = FALSE
    ),
    claim = paste(
      paste0(
        "Freedman-Lane inference over complete independent ",
        unit_label, ";"
      ),
      "validity assumes the declared exchangeability of residuals or sign symmetry,",
      "and does not treat spatial elements as independent observations."
    ),
    raw_values = aligned$values,
    analysis_values = transformed$values,
    null = null_output
  )
  class(result) <- "ngeo_group_result"
  result
}

#' @export
print.ngeo_group_result <- function(x, ...) {
  cat(
    "<ngeo_group_result>\n",
    "  subjects: ", length(x$design$unit_id), "\n",
    "  endpoints: ", nrow(x$tests), "\n",
    "  test: ", x$design$test, "\n",
    "  permutations: ", x$diagnostics$permutations, "\n",
    "  schedule hash: ", x$exchangeability$schedule_hash, "\n",
    sep = ""
  )
  invisible(x)
}
