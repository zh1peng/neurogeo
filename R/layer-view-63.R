#' Extract one complete spatial layer view
#'
#' Returns a stable, read-only view of one spatial field without requiring a
#' consumer to inspect the normalized `values`, `layers`, and `measures`
#' tables inside an `ngeo` dataset. The storage of the source object is not
#' changed.
#'
#' @section When to use and when not to use:
#' Use this view at package boundaries that consume one spatial field. Do not
#' use it to mutate layer or measure metadata; reconstruct or use the audited
#' update APIs instead.
#' @section Units and assumptions:
#' Values retain the selected measure unit, and coordinates retain the Base
#' coordinate unit. The selected layer must already align with the Base.
#' @section Validation:
#' Construction first performs basic dataset validation, requires exactly one
#' layer, and checks the returned Base, one-column values, measure, and metadata
#' alignment.
#'
#' @param x An `ngeo` object.
#' @param layer Exactly one layer position, ID, or unambiguous name.
#' @return An `ngeo_layer_view` with `base`, one-column `values`, one-row
#'   `measure`, and one-row layer `metadata`.
#' @examples
#' x <- ngeo_point(
#'   cbind(x = 0:2, y = 0),
#'   values = cbind(signal = 1:3)
#' )
#' field <- ngeo_layer_view(x, "signal")
#' field$measure
#' @seealso [ngeo_layers()], [ngeo_measures()], [ngeo_spatial_base()]
#' @references neurogeo API 6.3, `inst/spec/API-6.3.md`.
#' @export
ngeo_layer_view <- function(x, layer) {
  ngeo_validate(x, "basic")
  index <- .ngeo_layer_selection(x, layer)
  if (length(index) != 1L) {
    .ngeo_abort(
      "`layer` must select exactly one spatial field.",
      "ngeo_error_layer",
      field = "layer",
      hint = "Supply one layer position, layer ID, or unambiguous layer name."
    )
  }
  result <- structure(
    list(
      base = x$base,
      values = x$values[, index, drop = FALSE],
      measure = .ngeo_measures_for_layers(x, index),
      metadata = x$layers[index, , drop = FALSE]
    ),
    class = c("ngeo_layer_view", "list")
  )
  .ngeo_validate_layer_view(result)
  result
}

.ngeo_validate_layer_view <- function(x) {
  required <- c("base", "values", "measure", "metadata")
  valid_values <- is.matrix(x$values) || inherits(x$values, "Matrix") ||
    inherits(x$values, "ngeo_delayed_values")
  if (!inherits(x, "ngeo_layer_view") || !is.list(x) ||
      any(!required %in% names(x)) ||
      !inherits(x$base, "ngeo_base") ||
      !valid_values || ncol(x$values) != 1L ||
      nrow(x$values) != nrow(x$base$elements) ||
      !is.data.frame(x$metadata) || nrow(x$metadata) != 1L ||
      any(!c("layer_id", "measure_id") %in% names(x$metadata)) ||
      !is.data.frame(x$measure) || nrow(x$measure) != 1L ||
      !"measure_id" %in% names(x$measure) ||
      !identical(
        as.character(x$metadata$measure_id),
        as.character(x$measure$measure_id)
      )) {
    .ngeo_abort("Invalid `ngeo_layer_view` object.", "ngeo_error_layer")
  }
  .ngeo_validate_measures(x$measure)
  invisible(x)
}

#' @export
print.ngeo_layer_view <- function(x, ...) {
  cat(
    "<ngeo_layer_view>\n",
    "  layer: ", x$metadata$layer_id[[1L]], "\n",
    "  measure: ", x$measure$measure_id[[1L]], "\n",
    "  base: ", x$base$type, " (", nrow(x$base$elements), " elements)\n",
    sep = ""
  )
  invisible(x)
}
