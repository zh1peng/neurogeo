#' Construct measurement semantics
#'
#' @param value_type Value representation.
#' @param support_behavior One of intensive, extensive, count, categorical,
#'   or unknown.
#' @param unit Measurement unit.
#' @param missing_policy Missing-value policy.
#' @param aggregation Default aggregation rule.
#' @param measure_id Optional stable unique measure identifier.
#' @param name Optional human-readable measure name.
#'
#' @section When to use and when not to use:
#' Use one measure row to declare what a layer's values mean. Do not use units
#' alone as a substitute for intensive/extensive/count/categorical support
#' behavior, and do not mutate an existing object's measure table directly.
#' @section Units and assumptions:
#' `unit` is the scientific measurement unit, not the coordinate unit.
#' Aggregation must agree with support behavior; unknown semantics deliberately
#' block operations that would otherwise guess.
#' @section Validation:
#' `ngeo_update_measure()` provides the safe audited update path and strict
#' object validation checks every layer-to-measure reference.
#' @return A one-row measurement metadata data frame.
#' @seealso [ngeo_update_measure()], [ngeo_measures()], [ngeo_validate()]
#' @references Neuroimaging Geoinformatics Core Specification 6.0,
#'   `inst/spec/NGCS-6.0.md`.
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
                         aggregation = NULL,
                         measure_id = NULL,
                         name = NULL) {
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

  if (!is.null(measure_id)) {
    .ngeo_assert_scalar_character(measure_id, "measure_id")
  }
  if (!is.null(name)) {
    .ngeo_assert_scalar_character(name, "name")
  }

  result <- data.frame(
    value_type = value_type,
    support_behavior = support_behavior,
    unit = unit,
    missing_policy = missing_policy,
    aggregation = aggregation,
    stringsAsFactors = FALSE
  )
  if (!is.null(name)) result$name <- name
  if (!is.null(measure_id)) result$measure_id <- measure_id
  preferred <- intersect(
    c("measure_id", "name", "unit", "value_type", "support_behavior",
      "missing_policy", "aggregation"),
    names(result)
  )
  result[c(preferred, setdiff(names(result), preferred))]
}

#' Safely update one measurement definition
#'
#' The measure ID is immutable. The returned object is strictly validated and
#' receives a history operation; the input object is not modified.
#'
#' @param x An `ngeo` object.
#' @param measure_id Existing measure ID to update.
#' @param name,unit,value_type,support_behavior,missing_policy,aggregation
#'   Optional replacement fields. Omitted fields are retained.
#'
#' @return A validated `ngeo` object with updated measure metadata.
#' @templateVar example_call ngeo_update_measure(data, "thickness", unit = "mm")
#' @template stable-data-semantics-core
#' @export
ngeo_update_measure <- function(
    x,
    measure_id,
    name = NULL,
    unit = NULL,
    value_type = NULL,
    support_behavior = NULL,
    missing_policy = NULL,
    aggregation = NULL) {
  ngeo_validate(x, "basic")
  .ngeo_assert_scalar_character(measure_id, "measure_id")
  row <- match(measure_id, x$measures$measure_id)
  if (is.na(row)) {
    .ngeo_abort(
      sprintf("Unknown measure ID `%s`.", measure_id),
      "ngeo_error_measure",
      code = "NGEO_ERROR_MEASURE_UNKNOWN",
      field = "measure_id",
      hint = "Choose an ID returned by ngeo_measures(x)$measure_id."
    )
  }
  updates <- list(
    name = name,
    unit = unit,
    value_type = value_type,
    support_behavior = support_behavior,
    missing_policy = missing_policy,
    aggregation = aggregation
  )
  updates <- updates[!vapply(updates, is.null, logical(1))]
  if (!length(updates)) return(x)
  for (field in names(updates)) {
    .ngeo_assert_scalar_character(updates[[field]], field)
  }
  if ("support_behavior" %in% names(updates) &&
      !updates$support_behavior %in% c(
        "unknown", "intensive", "extensive", "count", "categorical"
      )) {
    .ngeo_abort(
      "Unknown `support_behavior`.",
      "ngeo_error_measure",
      code = "NGEO_ERROR_MEASURE_SUPPORT_BEHAVIOR",
      field = "support_behavior",
      hint = paste(
        "Use one of unknown, intensive, extensive, count, or categorical."
      )
    )
  }
  if ("missing_policy" %in% names(updates) &&
      !updates$missing_policy %in% c("preserve", "exclude")) {
    .ngeo_abort(
      "Unknown `missing_policy`.",
      "ngeo_error_measure",
      code = "NGEO_ERROR_MEASURE_MISSING_POLICY",
      field = "missing_policy",
      hint = "Use preserve or exclude."
    )
  }
  result <- x
  for (field in names(updates)) {
    if (!field %in% names(result$measures)) {
      default <- if (identical(field, "missing_policy")) {
        "preserve"
      } else {
        "unknown"
      }
      result$measures[[field]] <- rep.int(default, nrow(result$measures))
    }
    result$measures[[field]][[row]] <- updates[[field]]
  }
  result$history$operations <- c(
    result$history$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_update_measure",
      list(measure_id = measure_id, fields = names(updates))
    ))
  )
  ngeo_validate(result, "strict")
  result
}
