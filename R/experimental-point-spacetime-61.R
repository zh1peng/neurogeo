.ngeo_event_index <- function(x, value, name) {
  n <- nrow(x$base$elements)
  if (!is.atomic(value) || !length(value) || anyNA(value)) {
    .ngeo_abort(
      sprintf("Event type `%s` must contain one or more elements.", name),
      "ngeo_error_argument"
    )
  }
  index <- if (is.numeric(value)) {
    value <- .ngeo_as_integer(value, name)
    if (any(value < 1L | value > n)) rep.int(NA_integer_, length(value)) else
      value
  } else {
    match(as.character(value), x$base$elements$element_id)
  }
  if (anyNA(index)) {
    .ngeo_abort(
      sprintf("Event type `%s` references an unknown base element.", name),
      "ngeo_error_alignment"
    )
  }
  index
}

.ngeo_event_types <- function(x, events) {
  if (!is.list(events) || inherits(events, "data.frame")) {
    events <- list(event = events)
  }
  event_type <- names(events)
  if (is.null(event_type) || anyNA(event_type) || any(!nzchar(event_type))) {
    if (length(events) == 1L) {
      event_type <- "event"
    } else {
      .ngeo_abort(
        "Multiple event types require unique non-empty names.",
        "ngeo_error_argument"
      )
    }
  }
  if (anyDuplicated(event_type)) {
    .ngeo_abort(
      "Event type names must be unique.",
      "ngeo_error_argument"
    )
  }
  names(events) <- event_type
  Map(function(value, name) .ngeo_event_index(x, value, name),
      events, event_type)
}

.ngeo_event_occupancy <- function(event, occupancy, n_element) {
  if (identical(occupancy, "simple") &&
      any(vapply(event, anyDuplicated, integer(1)) > 0L)) {
    .ngeo_abort(
      "`occupancy = \"simple\"` does not allow repeated event elements within a type.",
      "ngeo_error_argument"
    )
  }
  if (identical(occupancy, "simple") &&
      any(vapply(event, length, integer(1)) > n_element)) {
    .ngeo_abort(
      "A simple event type cannot contain more events than base elements.",
      "ngeo_error_argument"
    )
  }
  invisible(TRUE)
}

.ngeo_event_pairs <- function(types, pairs) {
  if (is.null(pairs)) {
    combination <- if (length(types) > 1L) {
      utils::combn(types, 2L, simplify = FALSE)
    } else {
      list()
    }
    output <- c(lapply(types, function(type) c(type, type)), combination)
    return(data.frame(
      type_x = vapply(output, `[[`, character(1), 1L),
      type_y = vapply(output, `[[`, character(1), 2L),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.data.frame(pairs)) {
    .ngeo_abort(
      "`pairs` must be a data frame of event type pairs.",
      "ngeo_error_argument"
    )
  }
  if (all(c("x", "y") %in% names(pairs))) {
    pairs <- data.frame(
      type_x = pairs$x, type_y = pairs$y,
      stringsAsFactors = FALSE
    )
  } else if (all(c("type_x", "type_y") %in% names(pairs))) {
    pairs <- pairs[c("type_x", "type_y")]
  } else {
    .ngeo_abort(
      "Event pairs require `type_x`/`type_y` or `x`/`y` columns.",
      "ngeo_error_argument"
    )
  }
  pairs$type_x <- as.character(pairs$type_x)
  pairs$type_y <- as.character(pairs$type_y)
  type_order <- stats::setNames(seq_along(types), types)
  reverse <- type_order[pairs$type_x] > type_order[pairs$type_y]
  temporary <- pairs$type_x[reverse]
  pairs$type_x[reverse] <- pairs$type_y[reverse]
  pairs$type_y[reverse] <- temporary
  if (!nrow(pairs) || anyNA(pairs) ||
      any(!pairs$type_x %in% types) || any(!pairs$type_y %in% types) ||
      anyDuplicated(paste(pairs$type_x, pairs$type_y, sep = "\u001f"))) {
    .ngeo_abort(
      "Event pairs must uniquely reference declared event types.",
      "ngeo_error_argument"
    )
  }
  pairs
}

.ngeo_k_values <- function(
    event, pair_table, radii, distance, exposure, reference_probability) {
  normalized_exposure <- exposure / max(exposure)
  exposure_probability <- normalized_exposure / sum(normalized_exposure)
  output <- vector("list", nrow(pair_table) * length(radii))
  position <- 0L
  for (pair in seq_len(nrow(pair_table))) {
    type_x <- pair_table$type_x[[pair]]
    type_y <- pair_table$type_y[[pair]]
    event_x <- event[[type_x]]
    event_y <- event[[type_y]]
    event_distance <- distance[event_x, event_y, drop = FALSE]
    for (radius in radii) {
      position <- position + 1L
      local_exposure_fraction <- rowSums(
        sweep(
          distance[event_x, , drop = FALSE] <= radius,
          2L, exposure_probability, "*"
        )
      )
      neighbor <- rowSums(event_distance <= radius)
      if (identical(type_x, type_y)) neighbor <- neighbor - 1
      denominator <- if (identical(type_x, type_y)) {
        length(event_x) * (length(event_x) - 1L)
      } else {
        length(event_x) * length(event_y)
      }
      reference <- reference_probability[
        pair, match(radius, radii)
      ]
      estimate <- if (denominator > 0) {
        sum(neighbor) / denominator
      } else {
        NA_real_
      }
      output[[position]] <- data.frame(
        type_x = type_x,
        type_y = type_y,
        radius = radius,
        n_x = length(event_x),
        n_y = length(event_y),
        pair_fraction = estimate,
        csr_pair_probability = reference,
        relative_pair_enrichment = if (is.finite(estimate) && reference > 0) {
          estimate / reference
        } else {
          NA_real_
        },
        mean_ball_exposure_fraction_including_focal = mean(
          local_exposure_fraction
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, output)
}

.ngeo_csr_pair_probability <- function(
    radii, distance, probability, occupancy, same_type) {
  vapply(radii, function(radius) {
    close <- distance <= radius
    if (identical(occupancy, "simple") && same_type) {
      diag(close) <- FALSE
      weight <- outer(probability, probability)
      diag(weight) <- 0
      denominator <- sum(weight)
      if (denominator <= 0) return(NA_real_)
      sum(weight * close) / denominator
    } else {
      sum(outer(probability, probability) * close)
    }
  }, numeric(1))
}

.ngeo_k_simulation <- function(
    event, pair_table, radii, distance, exposure, reference_probability,
    simulations, seed, occupancy, level, budget) {
  if (!simulations) return(NULL)
  normalized_exposure <- exposure / max(exposure)
  probability <- normalized_exposure / sum(normalized_exposure)
  draw <- .ngeo_with_seed(seed, function() {
    result <- matrix(
      NA_real_, nrow = simulations,
      ncol = nrow(pair_table) * length(radii)
    )
    fraction <- result
    for (simulation in seq_len(simulations)) {
      .ngeo_budget_checkpoint(budget)
      current <- lapply(event, function(index) {
        sample.int(
          length(exposure), length(index),
          replace = identical(occupancy, "coincident"),
          prob = probability
        )
      })
      current_values <- .ngeo_k_values(
        current, pair_table, radii, distance, exposure,
        reference_probability
      )
      result[simulation, ] <- current_values$relative_pair_enrichment
      fraction[simulation, ] <- current_values$pair_fraction
    }
    list(estimates = result, pair_fraction = fraction)
  })
  list(
    estimates = draw$estimates,
    pair_fraction = draw$pair_fraction,
    lower = apply(draw$estimates, 2L, stats::quantile,
                  probs = (1 - level) / 2,
                  names = FALSE, na.rm = TRUE),
    upper = apply(draw$estimates, 2L, stats::quantile,
                  probs = 1 - (1 - level) / 2,
                  names = FALSE, na.rm = TRUE),
    median = apply(draw$estimates, 2L, stats::median, na.rm = TRUE),
    simulations = simulations,
    seed = .ngeo_seed(seed),
    null = paste(
      "exposure-weighted discrete complete spatial randomness;",
      if (identical(occupancy, "simple")) {
        "without replacement within each event type"
      } else {
        "with replacement; coincident events allowed"
      }
    )
  )
}

.ngeo_k_refresh_simulation <- function(simulation, level) {
  draw <- simulation$estimates
  simulation$lower <- apply(
    draw, 2L, stats::quantile, probs = (1 - level) / 2,
    names = FALSE, na.rm = TRUE
  )
  simulation$upper <- apply(
    draw, 2L, stats::quantile, probs = 1 - (1 - level) / 2,
    names = FALSE, na.rm = TRUE
  )
  simulation$median <- apply(draw, 2L, stats::median, na.rm = TRUE)
  simulation
}

.ngeo_k_global_envelope <- function(
    draw, observed, pair_table, radii, level) {
  lower <- upper <- p_value <- rep.int(NA_real_, length(observed))
  width <- length(radii)
  for (pair in seq_len(nrow(pair_table))) {
    rows <- (pair - 1L) * width + seq_len(width)
    current <- draw[, rows, drop = FALSE]
    center <- apply(current, 2L, stats::median, na.rm = TRUE)
    scale <- apply(current, 2L, stats::sd, na.rm = TRUE)
    valid_scale <- is.finite(scale) & scale > sqrt(.Machine$double.eps)
    standardized <- matrix(0, nrow(current), ncol(current))
    standardized[, valid_scale] <- sweep(
      current[, valid_scale, drop = FALSE],
      2L, center[valid_scale], "-"
    ) / rep(scale[valid_scale], each = nrow(current))
    maximum <- apply(abs(standardized), 1L, max, na.rm = TRUE)
    critical <- stats::quantile(
      maximum, probs = level, names = FALSE, na.rm = TRUE
    )
    lower[rows] <- center
    upper[rows] <- center
    lower[rows[valid_scale]] <- center[valid_scale] -
      critical * scale[valid_scale]
    upper[rows[valid_scale]] <- center[valid_scale] +
      critical * scale[valid_scale]
    if (!any(is.finite(observed[rows]))) next
    observed_standardized <- rep.int(0, width)
    observed_standardized[valid_scale] <-
      (observed[rows[valid_scale]] - center[valid_scale]) /
        scale[valid_scale]
    fixed <- !valid_scale & is.finite(observed[rows]) & is.finite(center)
    observed_standardized[fixed] <- ifelse(
      abs(observed[rows[fixed]] - center[fixed]) <=
        sqrt(.Machine$double.eps),
      0, Inf
    )
    observed_maximum <- max(observed_standardized, na.rm = TRUE)
    null_maximum <- apply(standardized, 1L, max, na.rm = TRUE)
    family_p <- (1 + sum(null_maximum >= observed_maximum)) /
      (length(null_maximum) + 1)
    p_value[rows] <- family_p
  }
  list(lower = lower, upper = upper, p_value = p_value)
}

.ngeo_point_process_prepare <- function(
    x, events, radii, pairs, exposure, distance_method, simulations,
    envelope, envelope_level, occupancy, retain_simulations, max_elements,
    budget) {
  ngeo_validate(x, "basic")
  if (!is.numeric(radii) || !length(radii) || anyNA(radii) ||
      any(!is.finite(radii)) || any(radii < 0) || anyDuplicated(radii)) {
    .ngeo_abort(
      "`radii` must contain unique non-negative finite distances.",
      "ngeo_error_argument"
    )
  }
  radii <- sort(as.numeric(radii))
  occupancy <- match.arg(occupancy, c("coincident", "simple"))
  envelope <- match.arg(envelope, c("pointwise", "global", "both"))
  simulations <- .ngeo_permutations(simulations)
  if (!is.numeric(envelope_level) || length(envelope_level) != 1L ||
      is.na(envelope_level) || !is.finite(envelope_level) ||
      envelope_level <= 0 || envelope_level >= 1) {
    .ngeo_abort(
      "`envelope_level` must be one probability strictly between zero and one.",
      "ngeo_error_argument"
    )
  }
  max_elements <- .ngeo_as_integer(max_elements, "max_elements")
  if (length(max_elements) != 1L || max_elements < 1L) {
    .ngeo_abort("`max_elements` must be one positive integer.",
                "ngeo_error_argument")
  }
  if (max_elements > 1000L) {
    .ngeo_abort(
      "`max_elements` cannot exceed the stable finite-domain cap of 1000.",
      "ngeo_error_resource"
    )
  }
  if (!is.logical(retain_simulations) || length(retain_simulations) != 1L ||
      is.na(retain_simulations)) {
    .ngeo_abort("`retain_simulations` must be TRUE or FALSE.",
                "ngeo_error_argument")
  }
  event <- .ngeo_event_types(x, events)
  pair_table <- .ngeo_event_pairs(names(event), pairs)
  exposure <- exposure %||% .ngeo_support_weights(x, "auto")$values
  if (!is.numeric(exposure) || length(exposure) != nrow(x$base$elements) ||
      anyNA(exposure) || any(!is.finite(exposure)) || any(exposure <= 0)) {
    .ngeo_abort(
      "`exposure` must be positive, finite, and base-aligned.",
      "ngeo_error_support"
    )
  }
  .ngeo_event_occupancy(event, occupancy, nrow(x$base$elements))
  unequal_exposure <- max(exposure) - min(exposure) >
    100 * .Machine$double.eps * max(exposure)
  if (identical(occupancy, "simple") && unequal_exposure && !simulations) {
    .ngeo_abort(
      paste(
        "Unequal-exposure simple-process analysis requires `simulations > 0`",
        "for sampling-design-matched CSR normalization."
      ),
      "ngeo_error_argument"
    )
  }
  n <- nrow(x$base$elements)
  if (n > max_elements) {
    .ngeo_abort(
      "Point-process edge correction exceeds the configured distance limit.",
      "ngeo_error_resource"
    )
  }
  normalized_exposure <- exposure / max(exposure)
  probability <- normalized_exposure / sum(normalized_exposure)
  output_cells <- as.double(nrow(pair_table)) * length(radii)
  simulation_cells <- as.double(simulations) * output_cells
  event_pairs <- sum(vapply(seq_len(nrow(pair_table)), function(i) {
    as.double(length(event[[pair_table$type_x[[i]]]])) *
      length(event[[pair_table$type_y[[i]]]])
  }, numeric(1)))
  peak_pair_cells <- max(vapply(seq_len(nrow(pair_table)), function(i) {
    n_x <- as.double(length(event[[pair_table$type_x[[i]]]]))
    n_y <- as.double(length(event[[pair_table$type_y[[i]]]]))
    n_x * n_y + n_x * n
  }, numeric(1)))
  context <- .ngeo_budget_context(budget)
  .ngeo_budget_assert(context, "blocks", max(1, simulations))
  .ngeo_budget_assert(
    context, "materialized_elements",
    as.double(n)^2 + simulation_cells + output_cells + event_pairs
  )
  .ngeo_budget_assert(
    context, "memory_bytes",
    8 * as.double(n)^2 + 8 * simulation_cells +
      8 * peak_pair_cells + 128 * output_cells
  )
  resolved_distance_method <- distance_method %||% switch(
    x$base$type,
    surface = "edge_geodesic", volume = "world_euclidean",
    point = "euclidean", parcellation = "region_centroid",
    grayordinate = "edge_geodesic"
  )
  resolved_distance_method <- .ngeo_metric_name(resolved_distance_method)
  distance <- ngeo_distance(
    x, from = seq_len(n), to = seq_len(n),
    distance_method = resolved_distance_method
  )
  reference_probability <- matrix(
    NA_real_, nrow = nrow(pair_table), ncol = length(radii)
  )
  for (i in seq_len(nrow(pair_table))) {
    reference_probability[i, ] <- .ngeo_csr_pair_probability(
      radii, distance, probability, occupancy,
      identical(pair_table$type_x[[i]], pair_table$type_y[[i]])
    )
  }
  colnames(reference_probability) <- as.character(radii)
  list(
    radii = radii, occupancy = occupancy, envelope = envelope,
    simulations = simulations, event = event, pair_table = pair_table,
    exposure = exposure, unequal_exposure = unequal_exposure,
    normalized_exposure = normalized_exposure,
    resolved_distance_method = resolved_distance_method, distance = distance,
    reference_probability = reference_probability, context = context,
    envelope_level = envelope_level,
    retain_simulations = retain_simulations
  )
}

#' Analyze events on a finite brain base
#'
#' Computes an ordered-pair summary for one or more event types over the
#' declared base metric. At each radius, the observed fraction of event pairs
#' within that radius is divided by its exposure-weighted discrete CSR
#' probability on the same finite base. This reference is analytic for the
#' coincident process and equal-exposure simple process; unequal-exposure
#' simple processes use the Monte Carlo mean under the matched sampling design.
#' The result is dimensionless cumulative
#' pair enrichment: one is the finite-domain CSR reference, values above one
#' indicate excess close pairs, and values below one indicate fewer close
#' pairs. It is not the classical Euclidean area-valued Ripley K and need not
#' be monotone in radius.
#'
#' @param x An `ngeo` object whose elements form the finite event domain.
#' @param events One atomic vector of element IDs/one-based indices, or a named
#'   list for marked event types. Repeated IDs require
#'   `occupancy = "coincident"`.
#' @param radii Unique non-negative distances in the selected metric.
#' @param pairs Optional marked-pair table with `x`/`y` or
#'   `type_x`/`type_y` columns.
#' @param exposure Positive finite base-aligned opportunity weights. Defaults
#'   to the base support measure.
#' @param distance_method Explicit [ngeo_distance()] method; `NULL` uses its
#'   base-specific default.
#' @param simulations Number of exposure-weighted discrete CSR simulations for
#'   envelopes and one-sided clustering p-values.
#' @param seed Reproducible simulation seed.
#' @param envelope Pointwise envelopes, a simultaneous studentized maximum-
#'   deviation envelope across radii within each marked-pair family, or both.
#' @param envelope_level Coverage level for the selected envelope.
#' @param occupancy Whether to sample an independent discrete CSR process with
#'   replacement (the default, permitting coincident events), or a simple
#'   process by exposure-weighted successive sampling without replacement.
#'   Unequal-exposure simple processes require simulations so their CSR
#'   normalization is estimated under the same sampling design. Cross-type
#'   overlap remains possible in both modes.
#' @param retain_simulations Whether to retain the complete simulation matrix.
#'   Pointwise summaries and p-values are returned either way.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#' @param max_elements Hard cap for the finite-domain distance matrix, from 1
#'   through the stable maximum of 1000 elements.
#'
#' @return An `ngeo_brain_point_process` with cumulative pair-enrichment estimates,
#'   finite-domain CSR references, event records, and optional pointwise or
#'   simultaneous null envelopes. Population inference is not implied.
#' @templateVar example_call ngeo_brain_point_process(x, events, radii = c(1, 2, 5))
#' @template stable-statistical-method
#' @references
#' Baddeley, A., Rubak, E., and Turner, R. (2015). *Spatial Point Patterns:
#' Methodology and Applications with R*. Chapman and Hall/CRC.
#' @examples
#' \dontrun{
#' ngeo_brain_point_process(x, events, radii = c(1, 2, 5))
#' }
#' @export
ngeo_brain_point_process <- function(
    x,
    events,
    radii,
    pairs = NULL,
    exposure = NULL,
    distance_method = NULL,
    simulations = 0L,
    seed = NULL,
    envelope = c("pointwise", "global", "both"),
    envelope_level = 0.95,
    occupancy = c("coincident", "simple"),
    retain_simulations = FALSE,
    max_elements = 1000L,
    budget = ngeo_resource_budget()) {
  prepared <- .ngeo_point_process_prepare(
    x, events, radii, pairs, exposure, distance_method, simulations,
    envelope, envelope_level, occupancy, retain_simulations, max_elements,
    budget
  )
  event <- prepared$event
  pair_table <- prepared$pair_table
  radii <- prepared$radii
  distance <- prepared$distance
  exposure <- prepared$exposure
  reference_probability <- prepared$reference_probability
  simulations <- prepared$simulations
  occupancy <- prepared$occupancy
  envelope <- prepared$envelope
  envelope_level <- prepared$envelope_level
  context <- prepared$context
  unequal_exposure <- prepared$unequal_exposure
  normalized_exposure <- prepared$normalized_exposure
  resolved_distance_method <- prepared$resolved_distance_method
  retain_simulations <- prepared$retain_simulations
  estimates <- .ngeo_k_values(
    event, pair_table, radii, distance, exposure,
    reference_probability
  )
  simulation <- .ngeo_k_simulation(
    event, pair_table, radii, distance, exposure,
    reference_probability,
    simulations, seed, occupancy, envelope_level, context
  )
  if (!is.null(simulation) && identical(occupancy, "simple") &&
      unequal_exposure) {
    empirical_reference <- colMeans(
      simulation$pair_fraction, na.rm = TRUE
    )
    estimates$csr_pair_probability <- empirical_reference
    estimates$relative_pair_enrichment <- ifelse(
      is.finite(estimates$pair_fraction) & empirical_reference > 0,
      estimates$pair_fraction / empirical_reference,
      NA_real_
    )
    simulation$estimates <- sweep(
      simulation$pair_fraction, 2L, empirical_reference, "/"
    )
    simulation <- .ngeo_k_refresh_simulation(simulation, envelope_level)
  }
  if (!is.null(simulation)) {
    estimates$null_median <- simulation$median
    estimates$envelope_lower <- simulation$lower
    estimates$envelope_upper <- simulation$upper
    estimates$p_clustering <- vapply(seq_len(nrow(estimates)), function(i) {
      observed <- estimates$relative_pair_enrichment[[i]]
      if (!is.finite(observed)) return(NA_real_)
      valid <- is.finite(simulation$estimates[, i])
      if (!any(valid)) return(NA_real_)
      (1 + sum(simulation$estimates[valid, i] >= observed)) /
        (sum(valid) + 1)
    }, numeric(1))
    global <- if (envelope %in% c("global", "both")) {
      .ngeo_k_global_envelope(
        simulation$estimates, estimates$relative_pair_enrichment,
        pair_table, radii, envelope_level
      )
    } else {
      list(
        lower = rep.int(NA_real_, nrow(estimates)),
        upper = rep.int(NA_real_, nrow(estimates)),
        p_value = rep.int(NA_real_, nrow(estimates))
      )
    }
    estimates$global_envelope_lower <- global$lower
    estimates$global_envelope_upper <- global$upper
    estimates$p_global_clustering <- global$p_value
  } else {
    estimates$null_median <- estimates$envelope_lower <-
      estimates$envelope_upper <- estimates$p_clustering <- NA_real_
    estimates$global_envelope_lower <- estimates$global_envelope_upper <-
      estimates$p_global_clustering <- NA_real_
  }
  retained_simulation <- simulation
  if (!is.null(retained_simulation) && !retain_simulations) {
    retained_simulation$estimates <- NULL
    retained_simulation$pair_fraction <- NULL
  }
  event_table <- do.call(rbind, lapply(names(event), function(type) {
    data.frame(
      type = type,
      event_id = seq_along(event[[type]]),
      element_id = x$base$elements$element_id[event[[type]]],
      stringsAsFactors = FALSE
    )
  }))
  rownames(event_table) <- NULL
  distance_unit <- if (identical(resolved_distance_method, "hops")) {
    "hops"
  } else {
    x$base$coordinate_space$unit
  }
  analysis_hash <- .ngeo_layer_digest(list(
    base_hash = base_hash(x),
    events = event,
    exposure = exposure,
    radii = radii,
    pairs = pair_table,
    distance_method = resolved_distance_method,
    occupancy = occupancy,
    simulations = simulations,
    envelope = envelope,
    envelope_level = envelope_level
  ))
  result <- list(
    estimates = estimates,
    events = event_table,
    simulations = retained_simulation,
    base_hash = base_hash(x),
    analysis_hash = analysis_hash,
    history = list(
      method = "brain_base_relative_pair_enrichment",
      base_hash = base_hash(x),
      distance_method = resolved_distance_method,
      distance_unit = distance_unit,
      estimand = paste(
        "observed ordered-pair fraction within radius divided by",
        if (identical(occupancy, "simple") && unequal_exposure) {
          "the matched Monte Carlo weighted-simple CSR pair probability"
        } else {
          "the analytic exposure-weighted CSR pair probability"
        }
      ),
      edge_correction = "finite-domain exposure-weighted CSR normalization",
      exposure_scaled_total = sum(normalized_exposure),
      exposure_scale = max(exposure),
      exposure_hash = .ngeo_layer_digest(exposure),
      event_hash = .ngeo_layer_digest(event),
      analysis_hash = analysis_hash,
      null_envelope = if (simulations) envelope else "none",
      envelope_level = envelope_level,
      simulations = simulations,
      monte_carlo_resolution = if (simulations) 1 / (simulations + 1) else NA_real_,
      simulation_draws_retained = retain_simulations && simulations > 0L,
      familywise_envelope = simulations > 0L &&
        envelope %in% c("global", "both"),
      occupancy = occupancy,
      simple_sampling = if (identical(occupancy, "simple")) {
        "exposure-weighted successive sampling without replacement"
      } else {
        "not_applicable"
      },
      csr_normalization = if (identical(occupancy, "simple") &&
          unequal_exposure) {
        "Monte Carlo mean under the matched weighted-without-replacement design"
      } else {
        "analytic finite-domain pair probability"
      },
      coincident_events_allowed = identical(occupancy, "coincident"),
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_brain_point_process")
}

.ngeo_hop_neighborhood <- function(adjacency, maximum, budget) {
  n <- nrow(adjacency)
  entries <- Matrix::summary(adjacency)
  neighbor <- split(entries$j, factor(entries$i, levels = seq_len(n)))
  output <- vector("list", n)
  total <- 0
  seen <- integer(n)
  for (source in seq_len(n)) {
    node <- source
    hop <- 0L
    frontier <- source
    seen[[source]] <- source
    for (depth in seq_len(maximum)) {
      next_frontier <- unique(unlist(neighbor[frontier], use.names = FALSE))
      next_frontier <- next_frontier[seen[next_frontier] != source]
      if (!length(next_frontier)) break
      node <- c(node, next_frontier)
      hop <- c(hop, rep.int(depth, length(next_frontier)))
      seen[next_frontier] <- source
      frontier <- next_frontier
    }
    output[[source]] <- list(node = node, hop = hop)
    total <- total + length(node)
    .ngeo_budget_assert(budget, "materialized_elements", total)
    .ngeo_budget_checkpoint(budget)
  }
  .ngeo_budget_assert(budget, "memory_bytes", 16 * total + 16 * n)
  attr(output, "materialized_nodes") <- total
  output
}

.ngeo_local_gistar <- function(
    values, spatial_neighborhood, temporal_neighborhood,
    spatial_bandwidth, temporal_bandwidth, interaction, budget) {
  n_space <- nrow(values)
  n_time <- ncol(values)
  n_observation <- length(values)
  standardized_values <- .ngeo_weighted_standardize(
    as.numeric(values), rep.int(1, n_observation)
  )$values
  if (any(!is.finite(standardized_values))) {
    .ngeo_abort(
      "Emerging-hotspot values must vary over space-time.",
      "ngeo_error_measure"
    )
  }
  standardized <- matrix(
    standardized_values, nrow = n_space, ncol = n_time
  )
  result <- matrix(NA_real_, nrow = n_space, ncol = n_time)
  for (time in seq_len(n_time)) {
    time_index <- temporal_neighborhood[[time]]$index
    temporal <- temporal_neighborhood[[time]]$distance /
      temporal_bandwidth
    for (space in seq_len(n_space)) {
      space_index <- spatial_neighborhood[[space]]$node
      spatial <- spatial_neighborhood[[space]]$hop / spatial_bandwidth
      exponent <- -outer(spatial, rep.int(1, length(temporal))) -
        outer(rep.int(1, length(spatial)), temporal) -
        interaction * outer(spatial, temporal)
      weight <- exp(exponent)
      local_values <- standardized[space_index, time_index, drop = FALSE]
      sum_weight <- sum(weight)
      mean_weight <- sum_weight / n_observation
      centered_weight_sum <- sum((weight - mean_weight)^2) +
        (n_observation - length(weight)) * mean_weight^2
      denominator <- sqrt(
        n_observation * centered_weight_sum /
          (n_observation - 1L)
      )
      if (is.finite(denominator) && denominator > 0) {
        result[space, time] <- sum(weight * local_values) / denominator
      }
    }
    .ngeo_budget_checkpoint(budget)
  }
  if (any(!is.finite(result))) {
    .ngeo_abort(
      "Hotspot weights have zero variance for at least one space-time location.",
      "ngeo_error_measure"
    )
  }
  result
}

.ngeo_hotspot_inference <- function(
    values, observed, spatial_neighborhood, temporal_neighborhood,
    spatial_bandwidth, temporal_bandwidth, interaction,
    permutations, seed, adjust, budget) {
  p_value <- p_adjusted <- matrix(
    NA_real_, nrow = nrow(observed), ncol = ncol(observed)
  )
  if (!permutations) {
    return(list(p_value = p_value, p_adjusted = p_adjusted))
  }
  raw_count <- adjusted_count <- integer(length(observed))
  target <- abs(as.numeric(observed))
  .ngeo_with_seed(seed, function() {
    for (simulation in seq_len(permutations)) {
      .ngeo_budget_checkpoint(budget)
      space_permutation <- sample(seq_len(nrow(values)))
      time_permutation <- sample(seq_len(ncol(values)))
      current <- .ngeo_local_gistar(
        values[space_permutation, time_permutation, drop = FALSE],
        spatial_neighborhood, temporal_neighborhood,
        spatial_bandwidth, temporal_bandwidth, interaction, budget
      )
      transformed <- abs(as.numeric(current))
      raw_count <<- raw_count + (transformed >= target)
      if (identical(adjust, "maxT")) {
        adjusted_count <<- adjusted_count + (max(transformed) >= target)
      }
    }
    invisible(NULL)
  })
  p_value[] <- (raw_count + 1) / (permutations + 1)
  p_adjusted[] <- if (identical(adjust, "maxT")) {
    (adjusted_count + 1) / (permutations + 1)
  } else {
    p_value
  }
  list(p_value = p_value, p_adjusted = p_adjusted)
}

.ngeo_final_run <- function(indicator) {
  if (!length(indicator) || !isTRUE(indicator[[length(indicator)]])) return(0L)
  current <- rev(indicator)
  first_false <- match(FALSE, current)
  if (is.na(first_false)) length(indicator) else first_false - 1L
}

.ngeo_hotspot_class <- function(indicator) {
  count <- sum(indicator)
  if (!count) return("none")
  final <- indicator[[length(indicator)]]
  if (!final) return("historical")
  run <- .ngeo_final_run(indicator)
  if (count == 1L) return("new")
  if (mean(indicator) >= 0.9) return("persistent")
  if (run == count) return("consecutive")
  "recurrent"
}

.ngeo_hotspot_state <- function(x, axis, z, threshold) {
  hot <- z >= threshold
  cold <- z <= -threshold
  output <- lapply(seq_len(nrow(z)), function(i) {
    first_hot <- which(hot[i, ])[1L]
    first_cold <- which(cold[i, ])[1L]
    change <- if (ncol(z) > 1L) which.max(abs(diff(z[i, ]))) + 1L else NA_integer_
    trend <- if (length(axis$time) > 1L) {
      stats::cov(axis$time, z[i, ]) / stats::var(axis$time)
    } else {
      NA_real_
    }
    data.frame(
      element_id = x$base$elements$element_id[[i]],
      hot_class = .ngeo_hotspot_class(hot[i, ]),
      cold_class = .ngeo_hotspot_class(cold[i, ]),
      hot_time_count = sum(hot[i, ]),
      cold_time_count = sum(cold[i, ]),
      final_hot_run = .ngeo_final_run(hot[i, ]),
      final_cold_run = .ngeo_final_run(cold[i, ]),
      first_hot_index = if (is.na(first_hot)) NA_integer_ else first_hot,
      first_hot_time = if (is.na(first_hot)) NA_real_ else axis$time[[first_hot]],
      first_cold_index = if (is.na(first_cold)) NA_integer_ else first_cold,
      first_cold_time = if (is.na(first_cold)) NA_real_ else axis$time[[first_cold]],
      z_trend_per_time_unit = trend,
      largest_change_index = change,
      largest_change_time = if (is.na(change)) NA_real_ else axis$time[[change]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, output)
}

.ngeo_hotspot_propagation <- function(x, adjacency, state) {
  entries <- Matrix::summary(adjacency)
  keep <- entries$i < entries$j & entries$x != 0
  first <- entries$i[keep]
  second <- entries$j[keep]
  first_time <- state$first_hot_time[first]
  second_time <- state$first_hot_time[second]
  valid <- is.finite(first_time) & is.finite(second_time) &
    first_time != second_time
  first <- first[valid]
  second <- second[valid]
  first_time <- first_time[valid]
  second_time <- second_time[valid]
  from <- ifelse(first_time < second_time, first, second)
  to <- ifelse(first_time < second_time, second, first)
  from_time <- pmin(first_time, second_time)
  to_time <- pmax(first_time, second_time)
  data.frame(
    from = x$base$elements$element_id[from],
    to = x$base$elements$element_id[to],
    from_first_hot_time = from_time,
    to_first_hot_time = to_time,
    lag = to_time - from_time,
    stringsAsFactors = FALSE
  )
}

.ngeo_hotspot_prepare <- function(
    x, spatial_weights, spatial_bandwidth, temporal_bandwidth, interaction,
    spatial_cutoff, temporal_cutoff, z_threshold, permutations, seed, adjust,
    retain_matrix, budget) {
  input_axis <- ngeo_get_time_axis(x)
  permutations <- .ngeo_permutations(permutations)
  adjust <- match.arg(adjust, c("maxT", "none"))
  if (permutations > 0L && length(input_axis$time) < 3L) {
    .ngeo_abort(
      "Joint space-time inference requires at least three declared time coordinates.",
      "ngeo_error_time_axis"
    )
  }
  if (!inherits(spatial_weights, "ngeo_spatial_weights") ||
      !identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort(
      "`spatial_weights` must match the hotspot base.",
      "ngeo_error_base_mismatch"
    )
  }
  selected <- seq_len(nrow(x$layers))
  .ngeo_projection_measures(x, selected)
  measure <- .ngeo_measures_for_layers(x, selected)
  temporal_semantics <- unique(measure$temporal_semantics)
  if (length(temporal_semantics) != 1L ||
      !identical(temporal_semantics, "instantaneous")) {
    .ngeo_abort(
      paste(
        "Hotspot scores currently require one instantaneous temporal",
        "semantics; interval totals, means, and rates need an explicit",
        "support transformation before analysis."
      ),
      "ngeo_error_temporal_measure"
    )
  }
  if (length(unique(measure$unit)) != 1L) {
    .ngeo_abort(
      "Hotspot time maps must share one measurement unit.",
      "ngeo_error_temporal_measure"
    )
  }
  if (!is.logical(retain_matrix) || length(retain_matrix) != 1L ||
      is.na(retain_matrix)) {
    .ngeo_abort("`retain_matrix` must be TRUE or FALSE.",
                "ngeo_error_argument")
  }
  temporal_bandwidth <- temporal_bandwidth %||%
    if (length(input_axis$time) > 1L) {
      stats::median(diff(input_axis$time))
    } else {
      NA_real_
    }
  scalars <- c(spatial_bandwidth, temporal_bandwidth, interaction, z_threshold)
  if (anyNA(scalars) || any(!is.finite(scalars)) ||
      spatial_bandwidth <= 0 || temporal_bandwidth <= 0 ||
      interaction <= 0 || z_threshold <= 0) {
    .ngeo_abort(
      "Hotspot bandwidths, interaction, and threshold must be positive.",
      "ngeo_error_argument"
    )
  }
  spatial_cutoff <- spatial_cutoff %||% ceiling(3 * spatial_bandwidth)
  temporal_cutoff <- temporal_cutoff %||% 3 * temporal_bandwidth
  if (!is.numeric(spatial_cutoff) || length(spatial_cutoff) != 1L ||
      is.na(spatial_cutoff) || !is.finite(spatial_cutoff) ||
      spatial_cutoff > .Machine$integer.max || spatial_cutoff < 1 ||
      spatial_cutoff != floor(spatial_cutoff) ||
      !is.numeric(temporal_cutoff) || length(temporal_cutoff) != 1L ||
      is.na(temporal_cutoff) || !is.finite(temporal_cutoff) ||
      temporal_cutoff <= 0) {
    .ngeo_abort(
      "Hotspot cutoffs must be positive graph hops and time distance.",
      "ngeo_error_argument"
    )
  }
  neighborhoods <- .ngeo_hotspot_neighborhoods(
    x, spatial_weights, input_axis, spatial_cutoff, temporal_cutoff,
    permutations, budget
  )
  c(list(
    input_axis = input_axis, permutations = permutations, adjust = adjust,
    temporal_semantics = temporal_semantics,
    temporal_bandwidth = temporal_bandwidth, spatial_cutoff = spatial_cutoff,
    temporal_cutoff = temporal_cutoff, seed = seed
  ), neighborhoods)
}

.ngeo_hotspot_neighborhoods <- function(
    x, spatial_weights, input_axis, spatial_cutoff, temporal_cutoff,
    permutations, budget) {
  budget_context <- .ngeo_budget_context(budget)
  n_value <- as.double(nrow(x$base$elements)) * nrow(x$layers)
  .ngeo_budget_assert(budget_context, "materialized_elements", 6 * n_value)
  .ngeo_budget_assert(budget_context, "memory_bytes", 192 * n_value)
  .ngeo_budget_assert(
    budget_context, "blocks",
    nrow(x$base$elements) + nrow(x$layers) +
      as.double(permutations) * nrow(x$layers)
  )
  values <- as.matrix(x$values)
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Emerging-hotspot analysis requires finite values.",
      "ngeo_error_missing"
    )
  }
  adjacency <- .ngeo_undirected_adjacency(spatial_weights)
  spatial_neighborhood <- .ngeo_hop_neighborhood(
    adjacency, as.integer(spatial_cutoff), budget_context
  )
  temporal_neighborhood <- lapply(seq_along(input_axis$time), function(i) {
    distance <- abs(input_axis$time - input_axis$time[[i]])
    keep <- which(distance <= temporal_cutoff)
    list(index = keep, distance = distance[keep])
  })
  spatial_interaction_available <- any(vapply(
    spatial_neighborhood, function(z) any(z$hop > 0), logical(1)
  ))
  temporal_interaction_available <- any(vapply(
    temporal_neighborhood, function(z) any(z$distance > 0), logical(1)
  ))
  if (!spatial_interaction_available || !temporal_interaction_available) {
    .ngeo_abort(
      paste(
        "The retained neighborhoods contain no positive spatial and",
        "temporal distances, so a nonseparable interaction is not defined."
      ),
      "ngeo_error_support"
    )
  }
  neighborhood_cells <- attr(spatial_neighborhood, "materialized_nodes") +
    sum(vapply(temporal_neighborhood, function(z) length(z$index), integer(1)))
  maximum_local_cells <- max(vapply(
    spatial_neighborhood, function(z) length(z$node), integer(1)
  )) * max(vapply(
    temporal_neighborhood, function(z) length(z$index), integer(1)
  ))
  .ngeo_budget_assert(
    budget_context, "materialized_elements",
    max(6 * n_value + neighborhood_cells, maximum_local_cells)
  )
  .ngeo_budget_assert(
    budget_context, "memory_bytes", 192 * n_value + 24 * neighborhood_cells
  )
  list(
    budget_context = budget_context, values = values, adjacency = adjacency,
    spatial_neighborhood = spatial_neighborhood,
    temporal_neighborhood = temporal_neighborhood,
    maximum_local_cells = maximum_local_cells
  )
}

#' Map exploratory nonseparable space-time hotspots
#'
#' Computes local Getis--Ord-style standardized scores over graph-hop and
#' irregular-time neighborhoods with a positive space-time interaction term.
#' The calculation uses truncated neighborhood lists rather than a dense
#' all-pairs space-time matrix. Optional inference uses one joint action that
#' permutes spatial identities and declared time coordinates for the complete
#' space-time field, with max-T familywise control across all local cells.
#' State labels and temporally ordered adjacency edges remain descriptive; they
#' do not establish transmission or causality.
#'
#' @param x Time-axis-enabled `ngeo` object whose value columns are time maps.
#' @param spatial_weights Matching spatial weights. Directed graphs are reduced
#'   to their undirected union for hotspot neighborhoods.
#' @param spatial_bandwidth Positive graph-hop bandwidth.
#' @param temporal_bandwidth Positive time bandwidth; defaults to the median
#'   observed interval.
#' @param interaction Positive nonseparable space-time interaction coefficient.
#' @param spatial_cutoff Maximum graph hops retained.
#' @param temporal_cutoff Maximum absolute time distance retained.
#' @param z_threshold Positive descriptive state threshold.
#' @param permutations Number of joint space-time null randomizations. Zero
#'   returns descriptive scores only.
#' @param seed Reproducible null seed.
#' @param adjust Max-T familywise correction across the complete local
#'   space-time map, or unadjusted cellwise Monte Carlo p-values.
#' @param retain_matrix Whether to retain the redundant space-by-time score
#'   matrix in addition to the long-form `local` table.
#' @param budget Hard execution resource limits.
#'
#' @return An `ngeo_nonseparable_hotspots` with local scores, descriptive state
#' summaries, adjacency edges ordered by first hot time, and optional local
#' Monte Carlo p-values.
#' @templateVar example_call ngeo_nonseparable_hotspots(x, spatial_weights, spatial_bandwidth = 2)
#' @template stable-statistical-method
#' @references
#' Getis, A. and Ord, J. K. (1992). The analysis of spatial association by use
#' of distance statistics. *Geographical Analysis*, 24, 189--206.
#' @examples
#' \dontrun{
#' ngeo_nonseparable_hotspots(x, spatial_weights, spatial_bandwidth = 2)
#' }
#' @export
ngeo_nonseparable_hotspots <- function(
    x,
    spatial_weights,
    spatial_bandwidth = 1,
    temporal_bandwidth = NULL,
    interaction = 1,
    spatial_cutoff = NULL,
    temporal_cutoff = NULL,
    z_threshold = 1.96,
    permutations = 0L,
    seed = NULL,
    adjust = c("maxT", "none"),
    retain_matrix = FALSE,
    budget = ngeo_resource_budget()) {
  prepared <- .ngeo_hotspot_prepare(
    x, spatial_weights, spatial_bandwidth, temporal_bandwidth, interaction,
    spatial_cutoff, temporal_cutoff, z_threshold, permutations, seed, adjust,
    retain_matrix, budget
  )
  values <- prepared$values
  spatial_neighborhood <- prepared$spatial_neighborhood
  temporal_neighborhood <- prepared$temporal_neighborhood
  budget_context <- prepared$budget_context
  input_axis <- prepared$input_axis
  adjacency <- prepared$adjacency
  temporal_semantics <- prepared$temporal_semantics
  temporal_bandwidth <- prepared$temporal_bandwidth
  spatial_cutoff <- prepared$spatial_cutoff
  temporal_cutoff <- prepared$temporal_cutoff
  permutations <- prepared$permutations
  adjust <- prepared$adjust
  maximum_local_cells <- prepared$maximum_local_cells
  z <- .ngeo_local_gistar(
    values, spatial_neighborhood, temporal_neighborhood,
    spatial_bandwidth, temporal_bandwidth, interaction, budget_context
  )
  inference <- .ngeo_hotspot_inference(
    values, z, spatial_neighborhood, temporal_neighborhood,
    spatial_bandwidth, temporal_bandwidth, interaction,
    permutations, seed, adjust, budget_context
  )
  state <- .ngeo_hotspot_state(x, input_axis, z, z_threshold)
  local <- data.frame(
    element_id = rep(x$base$elements$element_id, times = ncol(values)),
    time_index = rep(seq_len(ncol(values)), each = nrow(values)),
    time = rep(input_axis$time, each = nrow(values)),
    time_unit = rep(input_axis$unit, length(values)),
    value = as.numeric(values),
    local_gi_star_z = as.numeric(z),
    p_value = as.numeric(inference$p_value),
    p_adjusted = as.numeric(inference$p_adjusted),
    hot = as.numeric(z) >= z_threshold,
    cold = as.numeric(z) <= -z_threshold,
    stringsAsFactors = FALSE
  )
  result <- list(
    local = local,
    state = state,
    propagation = .ngeo_hotspot_propagation(x, adjacency, state),
    z = if (retain_matrix) z else NULL,
    base_hash = base_hash(x),
    axis_hash = input_axis$axis_hash,
    value_hash = .ngeo_layer_digest(values),
    weights_hash = .ngeo_layer_digest(list(
      base_hash = spatial_weights$base_hash,
      method = spatial_weights$method,
      raw_matrix = spatial_weights$raw_matrix
    )),
    analysis_hash = .ngeo_layer_digest(list(
      base_hash = base_hash(x), axis_hash = input_axis$axis_hash,
      value_hash = .ngeo_layer_digest(values),
      weights = spatial_weights$raw_matrix,
      spatial_bandwidth = spatial_bandwidth,
      temporal_bandwidth = temporal_bandwidth,
      interaction = interaction, spatial_cutoff = spatial_cutoff,
      temporal_cutoff = temporal_cutoff, z_threshold = z_threshold,
      permutations = permutations,
      seed = if (permutations) .ngeo_seed(seed) else NULL,
      adjust = adjust
    )),
    history = list(
      method = "truncated_nonseparable_space_time_gi_star",
      base_hash = base_hash(x),
      axis_hash = input_axis$axis_hash,
      time_unit = input_axis$unit,
      weights_method = spatial_weights$method,
      spatial_distance = "graph_hops",
      spatial_bandwidth = spatial_bandwidth,
      temporal_bandwidth = temporal_bandwidth,
      interaction = interaction,
      spatial_cutoff = spatial_cutoff,
      temporal_cutoff = temporal_cutoff,
      z_threshold = z_threshold,
      time_coordinates = input_axis$time,
      time_coordinates_used_directly = TRUE,
      temporal_distance = paste(
        "absolute difference in declared", input_axis$unit, "coordinates"
      ),
      temporal_semantics = temporal_semantics,
      nonseparable = TRUE,
      population_weighting = paste(
        "one element-time observation; base support and temporal interval",
        "duration are not population weights"
      ),
      reference_scope = paste(
        "global base-by-time mean and variance; disconnected components are",
        "not centered or scaled separately"
      ),
      continuum_refinement_invariance_claimed = FALSE,
      inference = if (permutations) {
        "joint space-time reference-map inference"
      } else {
        "exploratory standardized hotspot score"
      },
      null = if (permutations) {
        paste(
          "shared spatial-identity and time-coordinate permutations of",
          "the complete space-time field"
        )
      } else {
        "none"
      },
      permutations = permutations,
      seed = if (permutations) .ngeo_seed(seed) else NULL,
      familywise_error_control = permutations > 0L &&
        identical(adjust, "maxT"),
      multiplicity_family = if (permutations) {
        "all local space-time cells within one independent unit"
      } else {
        "none"
      },
      state_rule = paste(
        "custom threshold-count summary; not calibrated emerging-hotspot",
        "category inference"
      ),
      dense_space_time_matrix = FALSE,
      maximum_local_cells = maximum_local_cells,
      directed_weights_reduced_to_undirected_union = TRUE,
      causal_propagation_claimed = FALSE,
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_nonseparable_hotspots")
}

#' Convert subject hotspot maps to group-inference features
#'
#' Summarizes each subject's local hotspot trajectory at every spatial element
#' without pooling subjects. The returned `ngeo_subject_features` contract can
#' be passed directly to [ngeo_group_test()], whose exchangeability schedule
#' and max-T family then define group-level inference.
#'
#' @param results A named list of `ngeo_nonseparable_hotspots`, one per
#'   independent subject or sampling unit.
#' @param summaries Elementwise time summaries to expose as group endpoints.
#'
#' @return An `ngeo_subject_features` object.
#' @examples
#' \dontrun{
#' features <- ngeo_hotspot_group_features(subject_hotspot_results)
#' }
#' @template stable-statistical-method
#' @export
ngeo_hotspot_group_features <- function(
    results,
    summaries = c("mean_z", "trend", "max_abs_z", "hot_fraction")) {
  allowed <- c(
    "mean_z", "trend", "max_abs_z", "hot_fraction", "cold_fraction"
  )
  if (!is.list(results) || !length(results) ||
      any(!vapply(results, inherits, logical(1),
                  "ngeo_nonseparable_hotspots"))) {
    .ngeo_abort(
      "`results` must be a non-empty list of hotspot results.",
      "ngeo_error_argument"
    )
  }
  if (!is.character(summaries) || !length(summaries) || anyNA(summaries) ||
      any(!summaries %in% allowed) || anyDuplicated(summaries)) {
    .ngeo_abort(
      "Unknown or duplicate hotspot group summaries.",
      "ngeo_error_argument"
    )
  }
  unit_id <- names(results)
  if (is.null(unit_id) || anyNA(unit_id) || any(!nzchar(unit_id))) {
    unit_id <- sprintf("unit_%03d", seq_along(results))
  }
  if (anyDuplicated(unit_id)) {
    .ngeo_abort(
      "Hotspot group units require unique names.",
      "ngeo_error_independent_unit"
    )
  }
  reference <- results[[1L]]
  element_id <- as.character(reference$state$element_id)
  time <- unique(reference$local[c("time_index", "time")])$time
  compatible <- vapply(results, function(result) {
    identical(result$base_hash, reference$base_hash) &&
      identical(result$axis_hash, reference$axis_hash) &&
      identical(as.character(result$state$element_id), element_id) &&
      identical(unique(result$local[c("time_index", "time")])$time, time)
  }, logical(1))
  if (!all(compatible)) {
    .ngeo_abort(
      "All hotspot results must share one ordered base and real-time axis.",
      "ngeo_error_alignment"
    )
  }
  endpoint <- do.call(rbind, lapply(summaries, function(summary) {
    data.frame(
      endpoint_id = paste("hotspot", summary, element_id, sep = "::"),
      family = paste0("hotspot_", summary),
      estimand = summary,
      element_id = element_id,
      bounds = if (summary %in% c("hot_fraction", "cold_fraction")) {
        "[0,1]"
      } else {
        "unbounded"
      },
      recommended_transform = "none",
      stringsAsFactors = FALSE
    )
  }))
  values <- t(vapply(results, function(result) {
    n_space <- nrow(result$state)
    n_time <- length(time)
    z <- matrix(result$local$local_gi_star_z, nrow = n_space, ncol = n_time)
    hot <- matrix(result$local$hot, nrow = n_space, ncol = n_time)
    cold <- matrix(result$local$cold, nrow = n_space, ncol = n_time)
    unlist(lapply(summaries, function(summary) switch(
      summary,
      mean_z = rowMeans(z),
      trend = result$state$z_trend_per_time_unit,
      max_abs_z = apply(abs(z), 1L, max),
      hot_fraction = rowMeans(hot),
      cold_fraction = rowMeans(cold)
    )), use.names = FALSE)
  }, numeric(nrow(endpoint))))
  rownames(values) <- unit_id
  colnames(values) <- endpoint$endpoint_id
  result <- list(
    values = values,
    unit = data.frame(unit_id = unit_id, stringsAsFactors = FALSE),
    endpoints = endpoint,
    diagnostics = list(
      unit = length(unit_id),
      endpoints = nrow(endpoint),
      time_coordinates = time,
      time_unit = reference$history$time_unit
    ),
    history = list(
      method = "subject_hotspot_trajectory_features",
      base_hash = reference$base_hash,
      axis_hash = reference$axis_hash,
      source_result_hash = vapply(
        results, function(result) result$result_hash, character(1)
      ),
      inference_unit = "independent_subject_or_declared_unit",
      group_test = "use ngeo_group_test with an explicit exchangeability schedule"
    )
  )
  class(result) <- "ngeo_subject_features"
  result
}
