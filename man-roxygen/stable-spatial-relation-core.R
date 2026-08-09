#' @section When to use and limitations:
#' Use these functions after choosing a spatial relation that matches the base
#' and scientific question. Do not substitute Euclidean distance for cortical
#' geodesic distance, infer mesh adjacency from vertex order, or interpret graph
#' hops as physical distance.
#'
#' @section Units and assumptions:
#' Euclidean and geodesic distances use the declared coordinate-space unit;
#' graph hops are unitless and weighted graph costs use the edge-cost unit.
#' Neighbour operations assume aligned element identity, a declared symmetry
#' rule, explicit isolate policy, and non-negative finite edge weights.
#'
#' @section Validation evidence:
#' Distance, adjacency, component, symmetry, isolate, and interoperability
#' fixtures are declared in `inst/spec/validation-registry-6.0.csv` and linked
#' from `inst/validation/claim-evidence-matrix-6.0.csv`.
#'
#' @references
#' Cliff, A. D. and Ord, J. K. (1981). *Spatial Processes: Models &
#' Applications*. Pion.
#'
#' @seealso [ngeo_spatial_weights()], [ngeo_coordinate_space()]
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
