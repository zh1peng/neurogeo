.ngeo_inference_registry <- function() {
  path <- system.file(
    "spec", "inference-contracts-6.0.csv",
    package = "neurogeo"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "spec", "inference-contracts-6.0.csv")
  }
  if (!file.exists(path)) {
    .ngeo_abort(
      "The installed inference-contract registry is missing.",
      "ngeo_error_inference_contract"
    )
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

.ngeo_result_scalar <- function(x, fields) {
  for (field in fields) {
    value <- if (is.list(x) && field %in% names(x)) {
      x[[field]]
    } else {
      attr(x, field, exact = TRUE)
    }
    if (is.character(value) && length(value) == 1L &&
        !is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  NULL
}

.ngeo_inference_identifiers <- function(x) {
  fields <- c(
    "base_hash", "source_base_hash", "target_base_hash", "weights_hash",
    "support_map_hash", "ensemble_hash", "layer_id", "axis_hash"
  )
  values <- lapply(fields, function(field) {
    value <- if (is.list(x) && field %in% names(x)) {
      x[[field]]
    } else {
      attr(x, field, exact = TRUE)
    }
    if (is.atomic(value) && length(value) == 1L && !is.na(value)) {
      as.character(value)
    } else {
      NULL
    }
  })
  names(values) <- fields
  Filter(Negate(is.null), values)
}

.ngeo_validate_inference_contract <- function(x) {
  required <- c(
    "schema", "result_class", "lifecycle", "estimand", "sampling_unit",
    "null_model", "metric", "support", "uncertainty_target", "identifiers"
  )
  if (!inherits(x, "ngeo_inference_contract") || !is.list(x) ||
      any(!required %in% names(x))) {
    .ngeo_abort(
      "Inference contract fields are incomplete.",
      "ngeo_error_inference_contract"
    )
  }
  scalar_fields <- setdiff(required, "identifiers")
  valid <- vapply(x[scalar_fields], function(value) {
    is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(value)
  }, logical(1))
  if (!all(valid) || !is.list(x$identifiers)) {
    .ngeo_abort(
      "Inference contract values must be explicit non-empty text.",
      "ngeo_error_inference_contract"
    )
  }
  invisible(x)
}

#' Describe the scientific inference contract of a stable result
#'
#' The contract answers six questions needed to interpret a result: the
#' estimand, sampling unit, null model, metric, support, and uncertainty target.
#' A descriptive result states explicitly that a null or uncertainty target is
#' not applicable. Experimental result classes are intentionally excluded until
#' promotion evidence is available.
#'
#' @param x A stable neurogeo scientific result or an existing contract.
#'
#' @return An `ngeo_inference_contract` suitable for printing, summary, and
#'   [ngeo_object_manifest()].
#' @export
ngeo_inference_contract <- function(x) {
  if (inherits(x, "ngeo_inference_contract")) {
    .ngeo_validate_inference_contract(x)
    return(x)
  }
  registry <- .ngeo_inference_registry()
  matched_class <- class(x)[class(x) %in% registry$result_class]
  result_class <- if (length(matched_class)) matched_class[[1L]] else NULL
  if (is.null(result_class)) {
    .ngeo_abort(
      "No stable scientific inference contract is registered for this result class.",
      "ngeo_error_inference_contract"
    )
  }
  row <- registry[match(result_class, registry$result_class), , drop = FALSE]
  dynamic_null <- .ngeo_result_scalar(x, c("null_model", "null"))
  dynamic_metric <- .ngeo_result_scalar(x, c(
    "distance_method", "metric", "weights_method", "normalization"
  ))
  result <- structure(
    list(
      schema = "NGCS-inference-contract-1",
      result_class = result_class,
      lifecycle = "stable",
      estimand = row$estimand[[1L]],
      sampling_unit = row$sampling_unit[[1L]],
      null_model = dynamic_null %||% row$null_model[[1L]],
      metric = dynamic_metric %||% row$metric[[1L]],
      support = row$support[[1L]],
      uncertainty_target = row$uncertainty_target[[1L]],
      identifiers = .ngeo_inference_identifiers(x)
    ),
    class = "ngeo_inference_contract"
  )
  .ngeo_validate_inference_contract(result)
  result
}

#' @export
print.ngeo_inference_contract <- function(x, ...) {
  .ngeo_validate_inference_contract(x)
  cat(
    "<ngeo_inference_contract> ", x$result_class, "\n",
    "  estimand: ", x$estimand, "\n",
    "  sampling unit: ", x$sampling_unit, "\n",
    "  null model: ", x$null_model, "\n",
    "  metric: ", x$metric, "\n",
    "  support: ", x$support, "\n",
    "  uncertainty target: ", x$uncertainty_target, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
summary.ngeo_inference_contract <- function(object, ...) {
  .ngeo_validate_inference_contract(object)
  unclass(object)[c(
    "result_class", "lifecycle", "estimand", "sampling_unit", "null_model",
    "metric", "support", "uncertainty_target", "identifiers"
  )]
}
