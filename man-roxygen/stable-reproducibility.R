#' @section When to use and limitations:
#' Use this API to bind data, parameters, software identity, and artifacts to an
#' auditable analysis record. A matching hash proves byte or logical identity;
#' it does not by itself establish scientific validity, provenance trust, or
#' equivalence between different coordinate spaces.
#'
#' @section Units and assumptions:
#' Hashes are unitless SHA-256 identities. Measurement, coordinate, and time
#' units remain in the referenced object contracts. Replay assumes canonical
#' serialization, stable logical identifiers, relative or redacted paths, and
#' explicit verification of every input artifact before an operation runs.
#'
#' @section Validation evidence:
#' Manifest schemas, cross-machine replay fixtures, and source-identity checks
#' are declared in `inst/spec/validation-registry-6.0.csv` and linked to claims
#' in `inst/validation/claim-evidence-matrix-6.0.csv`.
#'
#' @references
#' Sandve, G. K. et al. (2013). Ten simple rules for reproducible computational
#' research. *PLoS Computational Biology*, 9(10), e1003285.
#'
#' @seealso [ngeo_object_manifest()], [ngeo_record_replay()]
<% if (exists("example_call")) { %>
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
<% } %>
