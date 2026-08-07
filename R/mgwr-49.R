#' Experimental fixed-bandwidth MGWR adapter
#'
#' This bounded adapter passes an NGCS distance matrix and explicit
#' predictor-specific bandwidths to `GWmodel::gwr.multiscale()`. It does not
#' select bandwidths and does not return nominal local p-values. Dense distance
#' construction is guarded before allocation, so this function is not a
#' full-cortex workflow and remains outside the stable API.
#'
#' @param x An `ngeo` dataset.
#' @param response One response map.
#' @param predictors Predictor layers.
#' @param bandwidths Positive fixed bandwidths for the intercept followed by
#'   each predictor.
#' @param distance_method Explicit NGCS distance distance_method.
#' @param kernel Bisquare or Gaussian kernel.
#' @param max_iterations Maximum backend coefficient back-fitting iterations.
#' @param threshold Positive convergence threshold.
#'
#' @return An experimental `ngeo_mgwr` object without local p-values.
#' @export
ngeo_mgwr <- function(
    x,
    response,
    predictors,
    bandwidths,
    distance_method = NULL,
    kernel = c("bisquare", "gaussian"),
    max_iterations = 200L,
    threshold = 1e-5) {
  .ngeo_require("GWmodel", "experimental MGWR")
  .ngeo_require("sf", "experimental MGWR")
  kernel <- match.arg(kernel)
  layers <- .ngeo_model_maps(x, response, predictors)
  if (!length(layers$predictors)) {
    .ngeo_abort(
      "MGWR requires at least one predictor.",
      "ngeo_error_argument"
    )
  }
  expected <- length(layers$predictors) + 1L
  if (!is.numeric(bandwidths) || length(bandwidths) != expected ||
      anyNA(bandwidths) || any(!is.finite(bandwidths)) ||
      any(bandwidths <= 0)) {
    .ngeo_abort(
      paste0(
        "`bandwidths` must contain ", expected,
        " positive fixed distances: intercept followed by predictors."
      ),
      "ngeo_error_argument"
    )
  }
  max_iterations <- .ngeo_as_integer(max_iterations, "max_iterations")
  if (max_iterations < 1L || !is.numeric(threshold) ||
      length(threshold) != 1L || is.na(threshold) ||
      !is.finite(threshold) || threshold <= 0) {
    .ngeo_abort(
      "Iteration limit and convergence threshold must be positive.",
      "ngeo_error_argument"
    )
  }
  values <- as.matrix(x$values[, c(layers$response, layers$predictors), drop = FALSE])
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "MGWR requires complete finite response and predictor layers.",
      "ngeo_error_missing"
    )
  }
  n <- nrow(values)
  maximum <- getOption("neurogeo.max_mgwr_elements", 500L)
  if (n > maximum) {
    .ngeo_abort(
      sprintf(
        paste(
          "Experimental MGWR is limited to %d elements before its bounded",
          "dense distance matrix is constructed."
        ),
        maximum
      ),
      "ngeo_error_resource"
    )
  }
  if (n <= expected + 2L) {
    .ngeo_abort(
      "MGWR needs more elements than local coefficients.",
      "ngeo_error_model"
    )
  }
  metric_name <- .ngeo_metric_name(distance_method %||% switch(
    x$base$type,
    surface = "edge_geodesic",
    volume = "world_euclidean",
    point = "euclidean",
    parcellation = "region_centroid",
    grayordinate = "edge_geodesic"
  ))
  index <- seq_len(n)
  distance <- unname(ngeo_distance(
    x, from = index, to = index, distance_method = metric_name
  ))
  if (!identical(dim(distance), c(n, n)) || any(!is.finite(distance))) {
    .ngeo_abort(
      paste(
        "MGWR requires finite all-to-all distances inside its bounded",
        "experimental base."
      ),
      "ngeo_error_capability"
    )
  }

  backend_names <- c("response", paste0("predictor", seq_along(layers$predictors)))
  data <- as.data.frame(values)
  names(data) <- backend_names
  coordinates <- .ngeo_element_coordinates(x)
  if (any(!is.finite(coordinates))) {
    .ngeo_abort("MGWR requires finite coordinates.", "ngeo_error_capability")
  }
  coordinate_names <- paste0(".coord", seq_len(ncol(coordinates)))
  backend_data <- cbind(
    data,
    stats::setNames(as.data.frame(coordinates), coordinate_names)
  )
  backend_data <- sf::st_as_sf(backend_data, coords = coordinate_names)
  formula <- stats::as.formula(paste(
    "response ~", paste(backend_names[-1L], collapse = " + ")
  ))
  backend_output <- utils::capture.output({
    fit <- GWmodel::gwr.multiscale(
      formula = formula,
      data = backend_data,
      kernel = kernel,
      adaptive = FALSE,
      criterion = "dCVR",
      max.iterations = max_iterations,
      threshold = threshold,
      dMats = list(distance),
      var.dMat.indx = rep.int(1L, expected),
      bws0 = as.numeric(bandwidths),
      bw.seled = rep.int(TRUE, expected),
      verbose = FALSE,
      hatmatrix = FALSE,
      force.armadillo = TRUE
    )
  })
  backend_local <- if (inherits(fit$SDF, "sf")) {
    sf::st_drop_geometry(fit$SDF)
  } else {
    as.data.frame(fit$SDF)
  }
  coefficient <- as.data.frame(backend_local[, seq_len(expected), drop = FALSE])
  names(coefficient) <- c("(Intercept)", layers$predictor_names)
  diagnostics <- ngeo_kernel_regression(
    x, response, predictors,
    bandwidth = min(bandwidths), distance_method = metric_name,
    kernel = kernel, singular = "na"
  )
  local <- data.frame(
    element_id = x$base$elements$element_id,
    fitted = as.numeric(backend_local$yhat),
    residual = as.numeric(backend_local$residual),
    effective_n = diagnostics$effective_n,
    condition_number = diagnostics$condition_number,
    coefficient,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(bandwidths) <- c("(Intercept)", layers$predictor_names)
  bandwidth_history <- if (is.null(fit$bws.vars)) {
    matrix(as.numeric(bandwidths), nrow = 1L,
           dimnames = list(NULL, names(bandwidths)))
  } else {
    as.matrix(fit$bws.vars)
  }
  model_hash <- .ngeo_layer_digest(list(
    base_hash = base_hash(x),
    layers = x$layers$layer_id[c(layers$response, layers$predictors)],
    distance_method = metric_name,
    bandwidths = bandwidths,
    kernel = kernel,
    local = local
  ))
  result <- list(
    status = "experimental_not_promoted",
    backend = "GWmodel::gwr.multiscale",
    response = layers$response_name,
    predictors = layers$predictor_names,
    bandwidths = bandwidths,
    bandwidth_selection = "fixed_user_supplied",
    bandwidth_history = bandwidth_history,
    local = local,
    diagnostics = list(
      convention = paste(
        "Effective N and condition number use the smallest declared",
        "term bandwidth as a conservative common local design."
      ),
      diagnostic_bandwidth = min(bandwidths),
      backend_messages = backend_output
    ),
    inference = list(
      nominal_local_p_values = FALSE,
      spatial_null_calibrated = FALSE,
      population_inference = FALSE
    ),
    promotion_blockers = c(
      "bandwidth-selection uncertainty is not calibrated",
      "spatially constrained null inference is unavailable",
      "support-family replication must be performed externally",
      "bounded dense distances prevent full-cortex use"
    ),
    distance_method = metric_name,
    kernel = kernel,
    base_hash = base_hash(x),
    support_hash = .ngeo_support_weights(x, "auto")$hash,
    model_hash = model_hash
  )
  class(result) <- "ngeo_mgwr"
  result
}

#' @export
print.ngeo_mgwr <- function(x, ...) {
  cat(
    "<ngeo_mgwr>\n",
    "  status: experimental, not promoted\n",
    "  backend: ", x$backend, "\n",
    "  elements: ", nrow(x$local), "\n",
    "  term bandwidths: ", paste(format(x$bandwidths), collapse = ", "), "\n",
    "  nominal local p-values: FALSE\n",
    sep = ""
  )
  invisible(x)
}
