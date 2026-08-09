args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "freeze-60-audit.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The 6.0 contract audit requires jsonlite.")
}
suppressPackageStartupMessages(library(neurogeo))

stable <- c(
  ngeo_bind_layers = "ngeo",
  ngeo_validate_layers = "ngeo_layer_index",
  ngeo_spatial_basis = "ngeo_spatial_basis",
  ngeo_basis_project = "ngeo_subject_features",
  ngeo_layer_coupling = "ngeo_subject_features",
  ngeo_exchangeability = "ngeo_exchangeability",
  ngeo_group_test = "ngeo_group_result"
)
experimental <- c(
  "ngeo_spatial_ordination", "ngeo_coregionalization", "ngeo_mgwr"
)
expected_formals <- list(
  ngeo_bind_layers = c("...", "metadata", "source_id", "conflicts", "storage", "budget"),
  ngeo_validate_layers = c("x", "unit", "layer", "required_layers", "complete", "require_consistent_measures"),
  ngeo_spatial_basis = c("x", "spatial_weights", "operator", "coordinates", "support", "n_modes", "components", "symmetrize", "tolerance", "budget"),
  ngeo_basis_project = c("x", "basis", "index", "layers", "bands", "center", "scale", "summaries", "chunk_rows", "chunk_layers"),
  ngeo_layer_coupling = c("x", "index", "pairs", "basis", "bands", "spatial_weights", "estimands", "lag_direction", "energy_floor", "null", "chunk_layers"),
  ngeo_exchangeability = c("unit_id", "scheme", "blocks", "schedule", "permutations", "seed", "budget"),
  ngeo_group_test = c("features", "data", "model", "test", "exchangeability", "family", "transform", "adjustment", "omnibus", "missing", "retain_null", "workers", "budget")
)
exports <- getNamespaceExports("neurogeo")
layer_selectors <- c(
  "ngeo_cortical_map", "ngeo_variogram_uncertainty",
  "ngeo_kriging_uncertainty", "ngeo_spin_null", "ngeo_moran_null",
  "ngeo_fit_variogram", "ngeo_kriging", "ngeo_getis_ord",
  "ngeo_correlogram", "ngeo_moran", "ngeo_geary", "ngeo_local_moran",
  "ngeo_variogram", "ngeo_parcellation_inference", "ngeo_stream_moran"
)
layer_selector_match <- vapply(layer_selectors, function(name) {
  arguments <- names(formals(getExportedValue("neurogeo", name)))
  "layer" %in% arguments && !"map" %in% arguments
}, logical(1))
signature_match <- vapply(names(stable), function(name) {
  identical(names(formals(getExportedValue("neurogeo", name))),
            expected_formals[[name]])
}, logical(1))
source_files <- c(
  "R/layers-45.R", "R/spatial-basis-45.R", "R/basis-projection-45.R",
  "R/layer-coupling-46.R", "R/exchangeability-47.R",
  "R/group-inference-47.R"
)
source <- paste(unlist(lapply(source_files, readLines, warn = FALSE)),
                collapse = "\n")
matches <- regmatches(source, gregexpr("ngeo_error_[a-z_]+", source))[[1L]]
error_classes <- sort(unique(matches))
group_facades <- intersect(exports, c("ngeo_group_test", "ngeo_spatial_features"))
checks <- list(
  stable_exports = all(names(stable) %in% exports),
  stable_count = length(stable) == 7L,
  conditional_entry_unexported = !"ngeo_spatial_features" %in% exports,
  experimental_exports_present = all(experimental %in% exports),
  signatures_frozen = all(signature_match),
  layer_selector_names = all(layer_selector_match),
  one_multilayer_group_facade = identical(group_facades, "ngeo_group_test"),
  base_error_contract = inherits(
    tryCatch(ngeo_exchangeability("only-one", permutations = 1L),
             error = identity),
    "ngeo_error"
  ),
  specifications_present = all(file.exists(file.path(
    "inst", "spec", c(
      "API-6.0.md", "NGCS-6.0.md", "migration-6.0.md",
      "validation-6.0.md"
    )
  )))
)
pass <- all(unlist(checks, use.names = FALSE))
report <- list(
  schema = "neurogeo/api-contract-6.0",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stable = lapply(names(stable), function(name) list(
    name = name, return_class = unname(stable[[name]]),
    formals = expected_formals[[name]], signature_match = signature_match[[name]]
  )),
  experimental = experimental,
  error_classes = error_classes,
  checks = checks,
  pass = pass
)
if (!pass) stop("The 6.0 API contract audit failed.", call. = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
