#' @section When to use and limitations:
#' Use these functions to inspect or change aligned values, layers, measures, or
#' derived subject features after element identity has been verified. Do not
#' treat layers as spatial elements, infer subject independence from column
#' count, or combine measurements with incompatible units or support behavior.
#'
#' @section Units and assumptions:
#' Values retain the unit and intensive/extensive behavior declared by their
#' measure. Rows remain aligned to base elements and columns to layer IDs.
#' Binding, subsetting, projection, and coupling assume stable IDs, compatible
#' bases, explicit missingness, and one measurement contract per selected layer.
#'
#' @section Validation evidence:
#' Alignment, layer-index, measure, chunking, projection, and coupling fixtures
#' and their evidence outputs are declared in
#' `inst/spec/validation-registry-6.0.csv`.
#'
#' @references
#' Wickham, H. (2014). Tidy data. *Journal of Statistical Software*, 59, 1--23.
#'
#' @seealso [ngeo_validate_layers()], [ngeo_measure()]
<% if (exists("example_call")) { %>
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
<% } %>
