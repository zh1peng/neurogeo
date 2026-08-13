#' @section When to use and limitations:
#' Use this method only after declaring the spatial support, coordinate space,
#' distance or weights definition, and independent sampling unit. Do not treat
#' vertices, voxels, or parcels as independent subjects, and do not interpret a
#' spatial-map null as population inference.
#'
#' @section Units and assumptions:
#' Measurement units are inherited from the selected measure. Distance units
#' come from the declared coordinate space; a graph metric is unitless unless
#' its edge costs carry a documented unit. Inference requires the assumptions
#' stated by the selected model or null, finite observations, aligned geometry,
#' and an analysis support that matches the spatial weights.
#'
#' @section Validation evidence:
#' Executable simulation and conformance suites for stable statistical methods
#' and their evidence outputs are declared in
#' `inst/spec/validation-registry-6.0.csv`.
#'
#' @references
#' Waller, L. A. and Gotway, C. A. (2004). *Applied Spatial Statistics for
#' Public Health Data*. Wiley.
#'
#' @seealso [ngeo_spatial_weights()], [ngeo_validate()]
