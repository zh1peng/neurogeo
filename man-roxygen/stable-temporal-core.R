#' @section When to use and limitations:
#' Use these functions only after identifying which layers form one ordered time
#' axis and which spatial elements repeat across those layers. Do not encode
#' subjects or unordered conditions as time, and do not treat repeated measures
#' as independent observations.
#'
#' @section Units and assumptions:
#' Time values carry the declared axis unit; temporal distances use that unit.
#' Spatial distances retain the coordinate-space unit, while graph hops are
#' unitless. Operations assume unique ordered time values, aligned layers,
#' stable element identity, and explicit temporal boundary and missingness rules.
#'
#' @section Validation evidence:
#' Temporal-axis, lag, sparse-weight, boundary, and longitudinal fixtures are
#' declared in `inst/spec/validation-registry-6.0.csv`; linked claim status is in
#' `inst/validation/claim-evidence-matrix-6.0.csv`.
#'
#' @references
#' Shumway, R. H. and Stoffer, D. S. (2017). *Time Series Analysis and Its
#' Applications*. Springer.
#'
#' @seealso [ngeo_time_axis()], [ngeo_set_time_axis()]
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
