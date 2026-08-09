# Bounded streaming computations over aligned values.
#' Compute delayed-native streaming summaries
#'
#' @param x An `ngeo` dataset or values block.
#' @param layers Optional layer selection.
#' @param chunk_size Row chunk size.
#' @param na_rm Whether to omit non-finite values.
#'
#' @return An `ngeo_stream_summary`.
#' @templateVar example_call ngeo_stream_summary(file_backed_data, layer = "signal")
#' @template stable-computation-core
#' @export
ngeo_stream_summary <- function(
    x,
    layers = NULL,
    chunk_size = 65536L,
    na_rm = FALSE) {
  values <- if (inherits(x, "ngeo")) x$values else x
  columns <- if (is.null(layers)) seq_len(ncol(values)) else if (
    inherits(x, "ngeo")
  ) .ngeo_layer_selection(x, layers) else
    .ngeo_delayed_index(layers, ncol(values), colnames(values))
  count <- rep(0, length(columns))
  mean <- m2 <- rep(0, length(columns))
  minimum <- rep(Inf, length(columns))
  maximum <- rep(-Inf, length(columns))
  missing <- rep(0, length(columns))
  iterator <- ngeo_value_chunks(x, chunk_size, columns)
  repeat {
    current <- iterator()
    if (is.null(current)) break
    block <- current$values
    for (j in seq_along(columns)) {
      value <- block[, j]
      finite <- is.finite(value)
      missing[[j]] <- missing[[j]] + sum(!finite)
      if (!all(finite) && !isTRUE(na_rm)) {
        .ngeo_abort("Streaming values contain non-finite entries.",
                    "ngeo_error_missing")
      }
      value <- value[finite]
      if (!length(value)) next
      n2 <- length(value)
      mean2 <- base::mean(value)
      m22 <- sum((value - mean2)^2)
      delta <- mean2 - mean[[j]]
      total <- count[[j]] + n2
      m2[[j]] <- m2[[j]] + m22 +
        delta^2 * count[[j]] * n2 / total
      mean[[j]] <- mean[[j]] + delta * n2 / total
      count[[j]] <- total
      minimum[[j]] <- min(minimum[[j]], value)
      maximum[[j]] <- max(maximum[[j]], value)
    }
  }
  result <- data.frame(
    layer = colnames(values)[columns] %||% paste0("layer_", columns),
    n = count,
    missing = missing,
    mean = mean,
    variance = ifelse(count > 1, m2 / (count - 1), NA_real_),
    minimum = ifelse(count > 0, minimum, NA_real_),
    maximum = ifelse(count > 0, maximum, NA_real_),
    stringsAsFactors = FALSE
  )
  class(result) <- c("ngeo_stream_summary", "data.frame")
  result
}

#' Compute a delayed-native covariance matrix
#'
#' @inheritParams ngeo_stream_summary
#' @return A covariance matrix.
#' @templateVar example_call ngeo_stream_covariance(file_backed_data, layers = c("x", "y"))
#' @template stable-computation-core
#' @export
ngeo_stream_covariance <- function(
    x,
    layers = NULL,
    chunk_size = 65536L) {
  values <- if (inherits(x, "ngeo")) x$values else x
  columns <- if (is.null(layers)) seq_len(ncol(values)) else if (
    inherits(x, "ngeo")
  ) .ngeo_layer_selection(x, layers) else
    .ngeo_delayed_index(layers, ncol(values), colnames(values))
  count <- 0L
  mean <- numeric(length(columns))
  cross <- matrix(0, length(columns), length(columns))
  iterator <- ngeo_value_chunks(x, chunk_size, columns)
  repeat {
    current <- iterator()
    if (is.null(current)) break
    block <- current$values
    if (any(!is.finite(block))) {
      .ngeo_abort("Streaming covariance requires finite values.",
                  "ngeo_error_missing")
    }
    n2 <- nrow(block)
    mean2 <- colMeans(block)
    centered <- sweep(block, 2L, mean2, "-")
    cross2 <- crossprod(centered)
    total <- count + n2
    delta <- mean2 - mean
    cross <- cross + cross2 +
      tcrossprod(delta) * count * n2 / total
    mean <- mean + delta * n2 / total
    count <- total
  }
  result <- cross / (count - 1L)
  dimnames(result) <- rep(list(
    colnames(values)[columns] %||% paste0("layer_", columns)
  ), 2L)
  result
}

#' Fit OLS from delayed chunks using sufficient statistics
#'
#' @param x An `ngeo` dataset.
#' @param response One response layer.
#' @param predictors Predictor layers.
#' @param chunk_size Row chunk size.
#'
#' @return An `ngeo_stream_lm`.
#' @templateVar example_call ngeo_stream_lm(file_backed_data, response = "y", predictors = "x")
#' @template stable-computation-core
#' @export
ngeo_stream_lm <- function(
    x,
    response,
    predictors = character(),
    chunk_size = 65536L) {
  layers <- .ngeo_model_maps(x, response, predictors)
  columns <- c(layers$response, layers$predictors)
  p <- length(layers$predictors) + 1L
  xtx <- matrix(0, p, p)
  xty <- numeric(p)
  yty <- 0
  n <- 0L
  iterator <- ngeo_value_chunks(x, chunk_size, columns)
  repeat {
    current <- iterator()
    if (is.null(current)) break
    block <- current$values
    if (any(!is.finite(block))) {
      .ngeo_abort("Streaming regression requires finite values.",
                  "ngeo_error_missing")
    }
    design <- cbind(1, block[, -1L, drop = FALSE])
    y <- block[, 1L]
    xtx <- xtx + crossprod(design)
    xty <- xty + as.numeric(crossprod(design, y))
    yty <- yty + sum(y^2)
    n <- n + length(y)
  }
  if (n <= p || qr(xtx)$rank < p) {
    .ngeo_abort("Streaming regression design is underpowered or singular.",
                "ngeo_error_model")
  }
  coefficient <- solve(xtx, xty)
  residual_ss <- yty - 2 * sum(coefficient * xty) +
    as.numeric(crossprod(coefficient, xtx %*% coefficient))
  sigma2 <- residual_ss / (n - p)
  names(coefficient) <- c("(Intercept)", layers$predictor_names)
  result <- list(
    coefficients = coefficient,
    covariance = sigma2 * solve(xtx),
    sigma = sqrt(sigma2),
    n = n,
    df_residual = n - p,
    response = layers$response_name,
    predictors = layers$predictor_names,
    base_hash = base_hash(x),
    method = "chunked sufficient statistics"
  )
  class(result) <- "ngeo_stream_lm"
  result
}

#' Compute Moran's I without materializing a delayed values block
#'
#' @param x An `ngeo` dataset.
#' @param spatial_weights Matching `ngeo_spatial_weights`.
#' @param layer One numeric layer.
#' @param chunk_size Row chunk size.
#'
#' @return An `ngeo_stream_moran`.
#' @examples
#' \dontrun{
#' ngeo_stream_moran(file_backed_data, spatial_weights, layer = "signal")
#' }
#' @template stable-statistical-method
#' @export
ngeo_stream_moran <- function(
    x, spatial_weights, layer = 1L, chunk_size = 65536L) {
  ngeo_validate(x, "strict")
  if (!inherits(spatial_weights, "ngeo_spatial_weights") ||
      !identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort("Streaming Moran spatial_weights do not match the base.",
                "ngeo_error_base_mismatch")
  }
  layer <- .ngeo_layer_selection(x, layer)
  if (length(layer) != 1L) {
    .ngeo_abort("Select one layer.", "ngeo_error_argument")
  }
  summary <- ngeo_stream_summary(x, layer, chunk_size)
  center <- summary$mean[[1L]]
  denominator <- (summary$n[[1L]] - 1L) * summary$variance[[1L]]
  matrix <- spatial_weights$matrix
  numerator <- 0
  row_start <- seq.int(1L, nrow(matrix), by = chunk_size)
  for (start in row_start) {
    rows <- seq.int(start, min(nrow(matrix), start + chunk_size - 1L))
    block <- matrix[rows, , drop = FALSE]
    entries <- Matrix::summary(block)
    columns <- sort(unique(entries$j))
    row_values <- as.numeric(x$values[rows, layer]) - center
    column_values <- as.numeric(x$values[columns, layer]) - center
    numerator <- numerator + sum(
      row_values * as.numeric(
        block[, columns, drop = FALSE] %*% column_values
      )
    )
  }
  estimate <- nrow(matrix) / sum(matrix) * numerator / denominator
  result <- list(
    estimate = estimate,
    layer = x$layers$name[[layer]],
    n = nrow(matrix),
    weights_method = spatial_weights$method,
    base_hash = base_hash(x),
    chunk_size = chunk_size,
    materialized_values = FALSE
  )
  class(result) <- "ngeo_stream_moran"
  result
}

#' @export
print.ngeo_stream_lm <- function(x, ...) {
  cat("<ngeo_stream_lm>\n  observations: ", x$n,
      "\n  response: ", x$response, "\n", sep = "")
  invisible(x)
}
