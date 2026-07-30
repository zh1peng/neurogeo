args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "space-graph-28-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Space graph validation requires jsonlite.")
}
if (!exists("ngeo_space_registry", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

space_a <- ngeo_space(
  "shared-name",
  kind = "surface",
  units = "mm",
  structure = "CORTEX_LEFT",
  template = "template-a",
  density = "32k",
  source_metadata = list(dimension = 3L)
)
space_b <- ngeo_space(
  "intermediate",
  kind = "surface",
  units = "mm",
  structure = "CORTEX_LEFT",
  template = "template-b",
  density = "32k",
  source_metadata = list(dimension = 3L)
)
space_c <- ngeo_space(
  "standard",
  kind = "surface",
  units = "mm",
  structure = "CORTEX_LEFT",
  template = "template-c",
  density = "32k",
  source_metadata = list(dimension = 3L)
)
space_d <- ngeo_space(
  "alternate",
  kind = "surface",
  units = "mm",
  structure = "CORTEX_LEFT",
  template = "template-d",
  density = "32k",
  source_metadata = list(dimension = 3L)
)
registry <- ngeo_space_registry(
  list(space_a, space_b, space_c, space_d),
  aliases = c(native = "shared-name", reference = "standard")
)

affine <- function(from, to, matrix, method) {
  ngeo_transform(
    from,
    to,
    "affine",
    method = method,
    parameters = list(matrix = matrix)
  )
}
first_matrix <- diag(4)
first_matrix[1:3, 4L] <- c(1, 2, 3)
second_matrix <- diag(c(1.1, 0.9, 1.2, 1))
second_matrix[1:3, 4L] <- c(-2, 1, 0)
first <- affine(space_a, space_b, first_matrix, "supplied-first")
second <- affine(space_b, space_c, second_matrix, "supplied-second")
graph <- ngeo_transform_graph(
  registry,
  list(first, second),
  edge_ids = c("a-b", "b-c")
)
path <- ngeo_transform_path(graph, "native", "reference")
direct_matrix <- second_matrix %*% first_matrix
composition_error <- max(
  abs(path$composed$parameters$matrix - direct_matrix)
)
provenance <- ngeo_transform_path_provenance(path)
if (composition_error > 1e-12 ||
    !identical(provenance$edge_hashes, path$edge_hashes) ||
    length(provenance$edge_hashes) != 2L) {
  stop("Direct affine path or provenance validation failed.")
}

points <- ngeo_points(
  cbind(x = c(0, 1), y = c(0, 1), z = c(0, 1)),
  values = cbind(signal = c(1, 2)),
  space = space_a
)
changed <- ngeo_apply_transform_path(points, path, authorize = TRUE)
homogeneous <- cbind(points$domain$coordinates, 1) %*% t(direct_matrix)
coordinate_reference <- homogeneous[, 1:3, drop = FALSE] /
  homogeneous[, 4L]
application_error <- max(
  abs(changed$domain$coordinates - coordinate_reference)
)
if (application_error > 1e-12) {
  stop("Authorized path application differs from direct affine reference.")
}

ab <- first
bc <- second
ad_matrix <- diag(4)
ad_matrix[3L, 4L] <- 4
dc_matrix <- direct_matrix %*% solve(ad_matrix)
ad <- affine(space_a, space_d, ad_matrix, "supplied-alternate-1")
dc <- affine(space_d, space_c, dc_matrix, "supplied-alternate-2")
ambiguous_graph <- ngeo_transform_graph(
  registry,
  list(ab, bc, ad, dc),
  edge_ids = c("a-b", "b-c", "a-d", "d-c")
)
ambiguity_rejected <- inherits(tryCatch(
  {
    ngeo_transform_path(ambiguous_graph, "native", "reference")
    NULL
  },
  error = identity
), "ngeo_error_transform_ambiguity")
selected <- ngeo_transform_path(
  ambiguous_graph,
  "native",
  "reference",
  selection = c("a-d", "d-c")
)
if (!ambiguity_rejected ||
    max(abs(selected$composed$parameters$matrix - direct_matrix)) > 1e-12) {
  stop("Ambiguity selection validation failed.")
}

cycle_matrix <- solve(direct_matrix)
cycle <- affine(space_c, space_a, cycle_matrix, "supplied-cycle")
cycle_graph <- ngeo_transform_graph(
  registry,
  list(first, second, cycle),
  edge_ids = c("a-b", "b-c", "c-a")
)
cycle_diagnostics <- ngeo_transform_graph_diagnostics(cycle_graph)
if (length(cycle_diagnostics$cycle_space_hashes) != 3L) {
  stop("Directed cycle was not diagnosed.")
}

mismatch_space <- ngeo_space(
  "mismatch",
  kind = "surface",
  units = "m",
  structure = "CORTEX_RIGHT",
  template = "template-x",
  density = "164k",
  source_metadata = list(dimension = 2L)
)
mismatch_audit <- ngeo_space_audit(space_a, mismatch_space)
required_mismatch <- c("units", "dimension", "structure")
if (attr(mismatch_audit, "compatible") ||
    !all(required_mismatch %in%
         mismatch_audit$field[
           mismatch_audit$severity == "incompatible"
         ])) {
  stop("Space mismatch audit failed.")
}

lossy_graph <- ngeo_transform_graph(
  registry,
  first,
  edge_ids = "a-b",
  invertible = TRUE,
  lossy = TRUE
)
lossy_inverse_rejected <- inherits(tryCatch(
  {
    ngeo_transform_path(
      lossy_graph, space_b, space_a, allow_inverse = TRUE
    )
    NULL
  },
  error = identity
), "ngeo_error_transform_path")

mutated <- graph
mutated$transforms[["a-b"]]$parameters$matrix[1L, 4L] <- 100
mutation_rejected <- inherits(tryCatch(
  {
    ngeo_validate_transform_graph(mutated)
    NULL
  },
  error = identity
), "ngeo_error_transform_graph_mutation")

same_name_conflict <- ngeo_space(
  "shared-name",
  kind = "volume",
  units = "mm",
  resolution = c(2, 2, 2)
)
ambiguous_registry <- ngeo_space_registry(list(space_a, same_name_conflict))
name_ambiguity_rejected <- inherits(tryCatch(
  {
    ngeo_resolve_space(ambiguous_registry, "shared-name")
    NULL
  },
  error = identity
), "ngeo_error_space_ambiguity")
if (!lossy_inverse_rejected || !mutation_rejected ||
    !name_ambiguity_rejected) {
  stop("Transform-graph adversarial boundary validation failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  direct_reference = list(
    affine_composition_maximum_error = composition_error,
    affine_application_maximum_error = application_error,
    traversed_edges = provenance$edge_ids,
    edge_hashes = provenance$edge_hashes,
    path_hash = provenance$path_hash
  ),
  graph_diagnostics = list(
    cycle_spaces = length(cycle_diagnostics$cycle_space_hashes),
    ambiguous_path_rejected = ambiguity_rejected,
    explicit_selection_matches_reference = TRUE,
    incompatible_fields = mismatch_audit$field[
      mismatch_audit$severity == "incompatible"
    ]
  ),
  adversarial = list(
    lossy_inverse_rejected = lossy_inverse_rejected,
    graph_mutation_rejected = mutation_rejected,
    ambiguous_name_rejected = name_ambiguity_rejected,
    automatic_registration = FALSE,
    automatic_resampling = FALSE
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
