#' @section When to use and limitations:
#' Use these functions with geometry whose element order, mask, topology, and
#' coordinate space are known. Do not infer cortical correspondence from nearby
#' coordinates, treat a visualization chart as anatomical geometry, or interpret
#' interpolation as additional spatial resolution.
#'
#' @section Units and assumptions:
#' Coordinates use the declared space unit; vertex areas use squared coordinate
#' units and voxel volumes use cubed affine units. Measure behavior controls
#' resampling: intensive values are support-weighted and extensive values require
#' conservation. Surface operations assume valid faces and matched hemispheres.
#'
#' @section Validation evidence:
#' Constructor, topology, affine, surface-map, conservation, and cartography
#' fixtures are declared in `inst/spec/validation-registry-6.0.csv`; claim status
#' is linked in `inst/validation/claim-evidence-matrix-6.0.csv`.
#'
#' @references
#' Fischl, B. (2012). FreeSurfer. *NeuroImage*, 62, 774--781.
#'
#' @seealso [ngeo_validate()], [ngeo_support_map()]
<% if (exists("example_call")) { %>
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
<% } %>
