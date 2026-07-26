#' Construct measurement semantics
#'
#' @param value_type Value representation.
#' @param spatial_semantics One of intensive, extensive, count, categorical,
#'   or unknown.
#' @param units Measurement units.
#' @param missing_policy Missing-value policy.
#' @param default_aggregation Default aggregation rule.
#'
#' @return A one-row measurement metadata data frame.
#' @export
ngeo_measure <- function(value_type = "continuous",
                         spatial_semantics = c(
                           "unknown", "intensive", "extensive",
                           "count", "categorical"
                         ),
                         units = "unknown",
                         missing_policy = c("preserve", "exclude"),
                         default_aggregation = NULL) {
  spatial_semantics <- match.arg(spatial_semantics)
  missing_policy <- match.arg(missing_policy)
  .ngeo_assert_scalar_character(value_type, "value_type")
  .ngeo_assert_scalar_character(units, "units")

  if (is.null(default_aggregation)) {
    default_aggregation <- switch(
      spatial_semantics,
      intensive = "support_weighted_mean",
      extensive = "sum",
      count = "sum",
      categorical = "mode",
      unknown = "none"
    )
  }
  .ngeo_assert_scalar_character(
    default_aggregation,
    "default_aggregation"
  )

  data.frame(
    value_type = value_type,
    spatial_semantics = spatial_semantics,
    units = units,
    missing_policy = missing_policy,
    default_aggregation = default_aggregation,
    stringsAsFactors = FALSE
  )
}

