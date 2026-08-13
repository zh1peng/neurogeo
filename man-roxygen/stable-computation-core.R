#' @section When to use and limitations:
#' Use these interfaces when an analysis is too large for an exact dense path or
#' when runtime capabilities must be checked explicitly. Do not treat reaching
#' an iteration limit as convergence, and do not interpret a stochastic or
#' truncated approximation without its error and seed diagnostics.
#'
#' @section Units and assumptions:
#' Streaming preserves input measurement units; covariance carries their product
#' units and regression coefficients follow response-per-predictor units.
#' Solvers assume finite aligned inputs, a declared tolerance and norm, stable
#' chunk ordering, deterministic seeds, and enforced memory/time budgets.
#'
#' @section Validation evidence:
#' Dense-reference, chunk-invariance, convergence, seed, and resource-budget
#' fixtures and their evidence outputs are declared in
#' `inst/spec/validation-registry-6.0.csv`.
#'
#' @references
#' Saad, Y. (2003). *Iterative Methods for Sparse Linear Systems*. SIAM.
#'
#' @seealso [ngeo_resource_budget()], [ngeo_solver_control()]
#' @examples
#' \dontrun{
#' <%= example_call %>
#' }
