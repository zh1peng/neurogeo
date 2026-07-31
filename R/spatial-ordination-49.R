.ngeo_ordination_maps <- function(x, maps) {
  ngeo_validate(x, "strict")
  if (is.null(x$values)) {
    .ngeo_abort("Spatial ordination requires loaded values.", "ngeo_error_values")
  }
  selected <- .ngeo_map_selection(x, maps)
  if (length(selected) < 2L || anyDuplicated(selected)) {
    .ngeo_abort(
      "Spatial ordination requires at least two distinct layers.",
      "ngeo_error_argument"
    )
  }
  if (any(x$measures$spatial_semantics[selected] == "categorical")) {
    .ngeo_abort(
      "Categorical layers cannot enter numeric spatial ordination.",
      "ngeo_error_measure"
    )
  }
  values <- as.matrix(x$values[, selected, drop = FALSE])
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Spatial ordination requires complete finite layers.",
      "ngeo_error_missing"
    )
  }
  if (any(vapply(seq_len(ncol(values)), function(i) {
    stats::var(values[, i]) <= 0
  }, logical(1)))) {
    .ngeo_abort(
      "Spatial ordination requires variable layers.",
      "ngeo_error_measure"
    )
  }
  colnames(values) <- x$maps$name[selected]
  list(index = selected, values = values, names = colnames(values))
}

.ngeo_ordination_projection <- function(x, maps, model) {
  if (!identical(ngeo_domain_hash(x), model$domain_hash)) {
    .ngeo_abort(
      "Projection data do not match the frozen training domain.",
      "ngeo_error_domain_mismatch"
    )
  }
  selected <- .ngeo_ordination_maps(x, maps)
  if (ncol(selected$values) != length(model$layer_names)) {
    .ngeo_abort(
      "Projection data must select the frozen layer count and order.",
      "ngeo_error_alignment"
    )
  }
  standardized <- sweep(selected$values, 2L, model$center, "-")
  standardized <- sweep(standardized, 2L, model$scale, "/")
  scores <- standardized %*% model$loadings
  rownames(scores) <- x$domain$elements$element_id
  scores
}

#' Experimental multivariate spatial ordination
#'
#' This optional adapter delegates the spatial eigenproblem to
#' `adespatial::multispati()`. Reference-map ordination is descriptive and
#' never performs population inference. Frozen training projection requires an
#' explicit declaration that training data are independent from projected test
#' data; the returned scores still contain no p-values.
#'
#' @param x An `ngeo` training or reference-map dataset.
#' @param maps At least two map names, IDs, or indices.
#' @param weights Matching `ngeo_weights`.
#' @param axes Maximum number of positive spatial axes retained.
#' @param regime Descriptive reference-map ordination or frozen training basis.
#' @param newdata Named list of test `ngeo` objects for frozen projection.
#' @param new_maps Map selector used in every test object.
#' @param independent_training Whether training independence is explicitly
#'   declared. Required for frozen projection.
#'
#' @return An experimental `ngeo_spatial_ordination` object.
#' @export
ngeo_spatial_ordination <- function(
    x,
    maps,
    weights,
    axes = 2L,
    regime = c("reference_map", "frozen_training"),
    newdata = NULL,
    new_maps = maps,
    independent_training = FALSE) {
  .ngeo_require("adespatial", "experimental spatial ordination")
  .ngeo_require("ade4", "experimental spatial ordination")
  .ngeo_require("spdep", "experimental spatial ordination")
  regime <- match.arg(regime)
  axes <- .ngeo_as_integer(axes, "axes")
  if (axes < 1L) {
    .ngeo_abort("`axes` must be positive.", "ngeo_error_argument")
  }
  selected <- .ngeo_ordination_maps(x, maps)
  if (!inherits(weights, "ngeo_weights")) {
    .ngeo_abort("`weights` must be `ngeo_weights`.", "ngeo_error_argument")
  }
  domain_hash <- ngeo_domain_hash(x)
  if (!identical(weights$domain_hash, domain_hash)) {
    .ngeo_abort(
      "Spatial ordination weights do not match the data domain.",
      "ngeo_error_domain_mismatch"
    )
  }
  raw <- .ngeo_as_dgCMatrix(weights$raw_matrix)
  if (any(Matrix::rowSums(abs(raw)) == 0)) {
    .ngeo_abort(
      "Spatial ordination does not retain isolated elements.",
      "ngeo_error_zero_policy"
    )
  }
  if (identical(regime, "reference_map") && !is.null(newdata)) {
    .ngeo_abort(
      "`newdata` is only valid for a frozen training basis.",
      "ngeo_error_argument"
    )
  }
  if (identical(regime, "frozen_training")) {
    if (!isTRUE(independent_training)) {
      .ngeo_abort(
        paste(
          "Frozen ordination requires an explicit independent training",
          "declaration; full-data component selection is not confirmatory."
        ),
        "ngeo_error_inference_leakage"
      )
    }
    if (!is.list(newdata) || !length(newdata) || is.null(names(newdata)) ||
        any(!nzchar(names(newdata))) || anyDuplicated(names(newdata))) {
      .ngeo_abort(
        "Frozen projection requires a uniquely named list of test datasets.",
        "ngeo_error_argument"
      )
    }
  }

  support <- .ngeo_support_weights(x, "auto")
  pca <- ade4::dudi.pca(
    as.data.frame(selected$values),
    row.w = support$values / sum(support$values),
    center = TRUE,
    scale = TRUE,
    scannf = FALSE,
    nf = min(axes, ncol(selected$values))
  )
  normalized <- weights
  normalized$matrix <- .ngeo_row_standardize(raw)
  listw <- as_spdep_listw(normalized)
  listw$style <- "W"
  fit <- tryCatch(
    adespatial::multispati(
      pca, listw, scannf = FALSE,
      nfposi = min(axes, ncol(selected$values)), nfnega = 0L
    ),
    error = function(error) {
      .ngeo_abort(
        paste(
          "Spatial ordination did not retain a positive spatial axis:",
          conditionMessage(error)
        ),
        "ngeo_error_statistic"
      )
    }
  )
  loadings <- as.matrix(fit$c1)
  training_scores <- as.matrix(fit$li)
  rownames(training_scores) <- x$domain$elements$element_id
  model <- list(
    domain_hash = domain_hash,
    layer_names = selected$names,
    center = unname(pca$cent),
    scale = unname(pca$norm),
    loadings = loadings
  )
  projected <- if (identical(regime, "frozen_training")) {
    lapply(newdata, .ngeo_ordination_projection, maps = new_maps, model = model)
  } else {
    list()
  }
  ordination_hash <- .ngeo_layer_digest(list(
    domain_hash = domain_hash,
    maps = x$maps$map_id[selected$index],
    support_hash = support$hash,
    weights = list(
      method = weights$method,
      normalization = "W",
      matrix = normalized$matrix
    ),
    center = model$center,
    scale = model$scale,
    loadings = loadings,
    regime = regime
  ))
  result <- list(
    regime = regime,
    backend = "adespatial::multispati",
    layer_names = selected$names,
    eigenvalues = fit$eig,
    loadings = loadings,
    training_scores = training_scores,
    projected = projected,
    independent_training = isTRUE(independent_training),
    population_inference = FALSE,
    inference_unit = "spatial_map",
    confirmatory_rule = paste(
      "Group inference requires independent training/test, calibrated",
      "cross-fitting, or refitting inside every permutation."
    ),
    domain_hash = domain_hash,
    support_hash = support$hash,
    ordination_hash = ordination_hash,
    status = "experimental"
  )
  class(result) <- "ngeo_spatial_ordination"
  result
}

#' @export
print.ngeo_spatial_ordination <- function(x, ...) {
  cat(
    "<ngeo_spatial_ordination>\n",
    "  status: experimental\n",
    "  regime: ", x$regime, "\n",
    "  layers: ", length(x$layer_names), "\n",
    "  axes: ", ncol(x$loadings), "\n",
    "  population inference: FALSE\n",
    sep = ""
  )
  invisible(x)
}
