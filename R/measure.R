#' Construct measurement semantics
#'
#' @param value_type Value representation.
#' @param support_behavior One of intensive, extensive, count, categorical,
#'   or unknown.
#' @param unit Measurement unit.
#' @param missing_policy Missing-value policy.
#' @param aggregation Default aggregation rule.
#'
#' @return A one-row measurement metadata data frame.
#' @examples
#' ngeo_measure(
#'   value_type = "continuous",
#'   support_behavior = "intensive",
#'   unit = "mm"
#' )
#' ngeo_measure(
#'   value_type = "integer",
#'   support_behavior = "count",
#'   unit = "events"
#' )
#' @export
ngeo_measure <- function(value_type = "continuous",
                         support_behavior = c(
                           "unknown", "intensive", "extensive",
                           "count", "categorical"
                         ),
                         unit = "unknown",
                         missing_policy = c("preserve", "exclude"),
                         aggregation = NULL) {
  support_behavior <- match.arg(support_behavior)
  missing_policy <- match.arg(missing_policy)
  .ngeo_assert_scalar_character(value_type, "value_type")
  .ngeo_assert_scalar_character(unit, "unit")

  if (is.null(aggregation)) {
    aggregation <- switch(
      support_behavior,
      intensive = "support_weighted_mean",
      extensive = "sum",
      count = "sum",
      categorical = "mode",
      unknown = "none"
    )
  }
  .ngeo_assert_scalar_character(
    aggregation,
    "aggregation"
  )

  data.frame(
    value_type = value_type,
    support_behavior = support_behavior,
    unit = unit,
    missing_policy = missing_policy,
    aggregation = aggregation,
    stringsAsFactors = FALSE
  )
}
