#' @section When to use and limitations:
#' Use this API at a package boundary to read, validate, or write an explicitly
#' supported neuroimaging or exchange format. Do not infer measurement meaning,
#' coordinate units, subject independence, or transform validity from a filename
#' alone; inspect the returned object and sidecar before analysis.
#'
#' @section Units and assumptions:
#' Array values retain their declared measurement units. Coordinates, affine
#' matrices, surface geometry, time axes, masks, and index bases are preserved
#' as explicit metadata. The caller must resolve conflicting headers, confirm
#' qform/sform or surface-space identity, and authorize overwrite operations.
#'
#' @section Validation evidence:
#' Round-trip, malformed-header, checksum, path-privacy, and golden-file suites
#' and their evidence outputs are declared in
#' `inst/spec/validation-registry-6.0.csv`.
#'
#' @references
#' Gorgolewski, K. J. et al. (2016). The brain imaging data structure, a format
#' for organizing and describing outputs of neuroimaging experiments.
#' *Scientific Data*, 3, 160044.
#'
#' @seealso [read_ngeo()], [write_ngeo()], [ngeo_validate()]
<% if (exists("example_call")) { %>
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
<% } %>
