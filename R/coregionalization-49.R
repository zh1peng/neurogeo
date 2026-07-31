.ngeo_pair_rank_index <- function(rank, n) {
  cumulative <- cumsum(n - seq_len(n - 1L))
  i <- findInterval(rank - 1, c(0, cumulative))
  previous <- c(0, cumulative)[i]
  cbind(i = i, j = i + rank - previous)
}

.ngeo_cross_variogram_breaks <- function(distance, breaks) {
  if (length(breaks) == 1L) {
    count <- .ngeo_as_integer(breaks, "breaks")
    if (count < 1L) {
      .ngeo_abort("`breaks` must be positive.", "ngeo_error_argument")
    }
    upper <- max(distance)
    if (!is.finite(upper) || upper <= 0) {
      .ngeo_abort(
        "Sampled pairs do not contain positive finite distances.",
        "ngeo_error_statistic"
      )
    }
    return(seq(0, upper, length.out = count + 1L))
  }
  boundaries <- as.numeric(breaks)
  if (anyNA(boundaries) || any(!is.finite(boundaries)) ||
      is.unsorted(boundaries, strictly = TRUE) ||
      boundaries[[1L]] > min(distance) ||
      boundaries[[length(boundaries)]] < max(distance)) {
    .ngeo_abort(
      paste(
        "`breaks` boundaries must be finite, strictly increasing,",
        "and cover sampled distances."
      ),
      "ngeo_error_argument"
    )
  }
  boundaries
}

.ngeo_cross_variograms <- function(
    x, maps, pair_sample, breaks = 10L, metric = NULL, seed = NULL) {
  ngeo_validate(x, "strict")
  if (is.null(x$values)) {
    .ngeo_abort("Cross-variograms require loaded values.", "ngeo_error_values")
  }
  selected <- .ngeo_map_selection(x, maps)
  if (length(selected) < 2L || anyDuplicated(selected)) {
    .ngeo_abort(
      "Cross-variograms require at least two distinct layers.",
      "ngeo_error_argument"
    )
  }
  if (any(x$measures$spatial_semantics[selected] == "categorical")) {
    .ngeo_abort(
      "Categorical layers do not have cross-semivariograms.",
      "ngeo_error_measure"
    )
  }
  values <- as.matrix(x$values[, selected, drop = FALSE])
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Cross-variograms require complete finite layers.",
      "ngeo_error_missing"
    )
  }
  n <- nrow(values)
  if (n < 3L) {
    .ngeo_abort(
      "Cross-variograms require at least three elements.",
      "ngeo_error_statistic"
    )
  }
  pair_sample <- .ngeo_as_integer(pair_sample, "pair_sample")
  maximum <- getOption("neurogeo.max_cross_variogram_pairs", 100000L)
  total <- n * (n - 1) / 2
  if (pair_sample < 1L || pair_sample > total || pair_sample > maximum) {
    .ngeo_abort(
      sprintf(
        "`pair_sample` must be between 1 and %s and not exceed %s.",
        format(total, big.mark = ","), format(maximum, big.mark = ",")
      ),
      "ngeo_error_resource"
    )
  }
  seed <- .ngeo_seed(seed)
  ranks <- .ngeo_with_seed(seed, function() {
    sort(sample.int(total, pair_sample, replace = FALSE))
  })
  pairs <- .ngeo_pair_rank_index(ranks, n)
  metric_name <- .ngeo_metric_name(metric %||% switch(
    x$domain$type,
    surface = "edge_geodesic",
    volume = "world_euclidean",
    points = "euclidean",
    regions = "region_centroid",
    grayordinates = "edge_geodesic"
  ))
  distance <- numeric(pair_sample)
  for (source in unique(pairs[, "i"])) {
    rows <- which(pairs[, "i"] == source)
    distance[rows] <- as.numeric(ngeo_distance(
      x, from = source, to = pairs[rows, "j"], metric = metric_name
    ))
  }
  finite <- is.finite(distance) & distance > 0
  if (!all(finite)) {
    pairs <- pairs[finite, , drop = FALSE]
    ranks <- ranks[finite]
    distance <- distance[finite]
  }
  if (!length(distance)) {
    .ngeo_abort(
      "No positive finite sampled pair distances remain.",
      "ngeo_error_statistic"
    )
  }
  boundaries <- .ngeo_cross_variogram_breaks(distance, breaks)
  bin <- cut(distance, boundaries, include.lowest = TRUE, right = TRUE)
  layer_ids <- paste0("L", seq_along(selected))
  tables <- list()
  position <- 1L
  for (a in seq_along(selected)) {
    for (b in seq.int(a, length(selected))) {
      delta_a <- values[pairs[, "i"], a] - values[pairs[, "j"], a]
      delta_b <- values[pairs[, "i"], b] - values[pairs[, "j"], b]
      gamma <- 0.5 * delta_a * delta_b
      id <- if (a == b) layer_ids[[a]] else
        paste(layer_ids[[a]], layer_ids[[b]], sep = ".")
      current <- lapply(levels(bin), function(level) {
        take <- which(bin == level)
        if (!length(take)) return(NULL)
        data.frame(
          np = length(take) * if (a == b) 1 else 2,
          dist = mean(distance[take]),
          gamma = mean(gamma[take]),
          dir.hor = 0,
          dir.ver = 0,
          id = id,
          stringsAsFactors = FALSE
        )
      })
      tables[[position]] <- do.call(rbind, current)
      position <- position + 1L
    }
  }
  table <- do.call(rbind, tables)
  rownames(table) <- NULL
  table$id <- factor(table$id, levels = unique(table$id))
  direct <- data.frame(
    id = levels(table$id),
    is.direct = !grepl("\\.", levels(table$id)),
    stringsAsFactors = FALSE
  )
  attr(table, "direct") <- direct
  attr(table, "boundaries") <- boundaries
  attr(table, "pseudo") <- 0
  attr(table, "what") <- "cross-semivariance"
  class(table) <- c("gstatVariogram", "data.frame")
  sampling <- list(
    design = "uniform_without_replacement",
    requested_pairs = pair_sample,
    retained_pairs = length(distance),
    population_pairs = total,
    seed = seed,
    convention = "0.5 * (x_i - x_j) * (y_i - y_j)",
    direction = "unordered element pairs; declared layer order",
    hash = .ngeo_layer_digest(list(
      domain_hash = ngeo_domain_hash(x), ranks = ranks, seed = seed
    ))
  )
  list(
    table = table,
    pair_ranks = ranks,
    boundaries = boundaries,
    metric = metric_name,
    layer_ids = stats::setNames(layer_ids, x$maps$name[selected]),
    map_index = selected,
    sampling = sampling
  )
}

.ngeo_lmc_psd <- function(models, layer_ids, tolerance = 1e-8) {
  structures <- seq_len(nrow(models[[layer_ids[[1L]]]]))
  result <- lapply(structures, function(structure) {
    sill <- matrix(0, length(layer_ids), length(layer_ids),
                   dimnames = list(layer_ids, layer_ids))
    for (i in seq_along(layer_ids)) {
      sill[i, i] <- models[[layer_ids[[i]]]]$psill[[structure]]
      if (i < length(layer_ids)) {
        for (j in seq.int(i + 1L, length(layer_ids))) {
          cross_id <- paste(layer_ids[[i]], layer_ids[[j]], sep = ".")
          if (is.null(models[[cross_id]])) {
            cross_id <- paste(layer_ids[[j]], layer_ids[[i]], sep = ".")
          }
          sill[i, j] <- sill[j, i] <- models[[cross_id]]$psill[[structure]]
        }
      }
    }
    eigenvalue <- eigen(sill, symmetric = TRUE, only.values = TRUE)$values
    list(
      summary = data.frame(
        structure = structure,
        model = as.character(models[[layer_ids[[1L]]]]$model[[structure]]),
        min_eigenvalue = min(eigenvalue),
        positive_semidefinite = min(eigenvalue) >= -tolerance,
        stringsAsFactors = FALSE
      ),
      sill = sill
    )
  })
  list(
    summary = do.call(rbind, lapply(result, `[[`, "summary")),
    matrices = lapply(result, `[[`, "sill")
  )
}

#' Experimental linear model of coregionalization
#'
#' Fits a bounded shared-scale decomposition through `gstat::fit.lmc()` from
#' one explicit uniform sample of element pairs. It assumes second-order
#' stationarity and isotropy, reports positive-semidefinite sill diagnostics,
#' and deliberately provides no co-kriging facade.
#'
#' @param x An `ngeo` points or regions dataset.
#' @param maps Two or more continuous map selectors.
#' @param pair_sample Number of unordered element pairs sampled explicitly.
#' @param breaks Number of distance bins or explicit boundaries.
#' @param metric Explicit NGCS metric.
#' @param model A `gstat` variogram model name.
#' @param range Positive fixed shared range used by the initial LMC model.
#' @param nugget Non-negative initial nugget.
#' @param seed Sampling seed.
#'
#' @return An experimental `ngeo_coregionalization` object.
#' @export
ngeo_coregionalization <- function(
    x,
    maps,
    pair_sample,
    breaks = 10L,
    metric = NULL,
    model = c("Exp", "Sph", "Gau"),
    range,
    nugget = 0,
    seed = NULL) {
  .ngeo_require("gstat", "experimental coregionalization")
  .ngeo_require("sf", "experimental coregionalization")
  model <- match.arg(model)
  if (!x$domain$type %in% c("points", "regions")) {
    .ngeo_abort(
      paste(
        "The experimental LMC adapter is limited to points and regions;",
        "bounded surface cross-variograms remain descriptive only."
      ),
      "ngeo_error_capability"
    )
  }
  if (!is.numeric(range) || length(range) != 1L || is.na(range) ||
      !is.finite(range) || range <= 0 || !is.numeric(nugget) ||
      length(nugget) != 1L || is.na(nugget) || !is.finite(nugget) ||
      nugget < 0) {
    .ngeo_abort(
      "`range` must be positive and `nugget` non-negative.",
      "ngeo_error_argument"
    )
  }
  empirical <- .ngeo_cross_variograms(
    x, maps, pair_sample = pair_sample, breaks = breaks,
    metric = metric, seed = seed
  )
  if (length(empirical$map_index) >
      getOption("neurogeo.max_coregionalization_layers", 6L)) {
    .ngeo_abort(
      "The experimental LMC layer count exceeds its resource limit.",
      "ngeo_error_resource"
    )
  }
  coordinates <- .ngeo_element_coordinates(x)
  if (any(!is.finite(coordinates))) {
    .ngeo_abort(
      "Coregionalization requires finite element coordinates.",
      "ngeo_error_capability"
    )
  }
  data <- as.data.frame(x$values[, empirical$map_index, drop = FALSE])
  names(data) <- unname(empirical$layer_ids)
  coordinate_names <- paste0(".coord", seq_len(ncol(coordinates)))
  spatial_data <- cbind(data, stats::setNames(as.data.frame(coordinates),
                                              coordinate_names))
  spatial_data <- sf::st_as_sf(spatial_data, coords = coordinate_names)
  g <- NULL
  layer_ids <- unname(empirical$layer_ids)
  for (id in layer_ids) {
    formula <- stats::as.formula(paste(id, "~ 1"))
    g <- gstat::gstat(g, id = id, formula = formula, data = spatial_data)
  }
  initial <- gstat::vgm(psill = 1, model = model, range = range,
                        nugget = nugget)
  fit <- gstat::fit.lmc(
    empirical$table, g, initial,
    fit.ranges = FALSE, fit.lmc = TRUE, correct.diagonal = 1
  )
  psd <- .ngeo_lmc_psd(fit$model, layer_ids)
  if (!all(psd$summary$positive_semidefinite)) {
    .ngeo_abort(
      "The fitted LMC sill matrices are not positive semidefinite.",
      "ngeo_error_model"
    )
  }
  singular <- vapply(fit$model, function(value) {
    isTRUE(attr(value, "singular"))
  }, logical(1))
  squared_error <- vapply(fit$model, function(value) {
    as.numeric(attr(value, "SSErr") %||% NA_real_)
  }, numeric(1))
  diagnostics <- data.frame(
    variogram = names(fit$model),
    singular = singular,
    weighted_squared_error = squared_error,
    stringsAsFactors = FALSE
  )
  model_hash <- .ngeo_layer_digest(list(
    domain_hash = ngeo_domain_hash(x),
    maps = x$maps$map_id[empirical$map_index],
    empirical_hash = empirical$sampling$hash,
    model = fit$model,
    assumptions = list(stationarity = "second_order", isotropy = TRUE)
  ))
  result <- list(
    status = "experimental",
    backend = "gstat::fit.lmc",
    maps = stats::setNames(x$maps$name[empirical$map_index], layer_ids),
    empirical = empirical,
    models = fit$model,
    psd_diagnostics = psd$summary,
    sill_matrices = psd$matrices,
    convergence = diagnostics,
    assumptions = list(
      stationarity = "second_order",
      isotropy = TRUE,
      shared_range = range,
      variogram_model = model
    ),
    capabilities = list(shared_scale_decomposition = TRUE, co_kriging = FALSE),
    domain_hash = ngeo_domain_hash(x),
    model_hash = model_hash
  )
  class(result) <- "ngeo_coregionalization"
  result
}

#' @export
print.ngeo_coregionalization <- function(x, ...) {
  cat(
    "<ngeo_coregionalization>\n",
    "  status: experimental\n",
    "  backend: ", x$backend, "\n",
    "  layers: ", length(x$maps), "\n",
    "  sampled pairs: ", x$empirical$sampling$retained_pairs, "\n",
    "  co-kriging: FALSE\n",
    sep = ""
  )
  invisible(x)
}
