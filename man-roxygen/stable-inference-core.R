#' @section When to use and limitations:
#' Use these interfaces only after declaring the sampling unit, tested estimand,
#' support family, exchangeability schedule, and spatial null. Do not exchange
#' voxels, vertices, parcels, repeated sessions, or atlas variants as if they
#' were independent subjects, and do not interpret map-level inference as a
#' population claim.
#'
#' @section Units and assumptions:
#' Effects retain their measurement units; standardized statistics and adjusted
#' p-values are unitless. Boundary and multiscale procedures assume aligned
#' supports and predeclared families. Permutation validity requires transformations
#' that preserve the null and the declared dependence or blocking structure.
#'
#' @section Validation evidence:
#' Null, exchangeability, family-wise error, boundary, and multiscale calibration
#' targets and their evidence outputs are declared in
#' `inst/spec/validation-registry-6.0.csv`.
#'
#' @references
#' Winkler, A. M. et al. (2014). Permutation inference for the general linear
#' model. *NeuroImage*, 92, 381--397.
#'
#' @seealso [ngeo_inference_contract()], [ngeo_exchangeability()]
<% if (exists("example_call")) { %>
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
<% } %>
