#' @section When to use and limitations:
#' Use these functions to inspect quality, provenance, runtime identity, bundled
#' examples, or migration status before relying on an analysis. Passing structural
#' QC does not prove scientific suitability, and a migrated object must still be
#' checked against its original measurement and coordinate-space documentation.
#'
#' @section Units and assumptions:
#' Measurements and coordinates retain their source units; hashes, status flags,
#' and graph identities are unitless. Reports assume stable logical IDs, acyclic
#' provenance, explicit software versions, privacy-safe paths, and no silent
#' reconstruction of missing semantics.
#'
#' @section Validation evidence:
#' QC, DAG, environment, fixture-integrity, and migration golden tests are
#' declared in `inst/spec/validation-registry-6.0.csv`; evidence status is linked
#' in `inst/validation/claim-evidence-matrix-6.0.csv`.
#'
#' @references
#' Sandve, G. K. et al. (2013). Ten simple rules for reproducible computational
#' research. *PLoS Computational Biology*, 9(10), e1003285.
#'
#' @seealso [ngeo_qc()], [ngeo_object_manifest()]
<% if (exists("example_call")) { %>
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
<% } %>
