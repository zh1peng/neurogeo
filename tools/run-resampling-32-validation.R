args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "resampling-32-validation.json")
required <- c("digest", "jsonlite", "Matrix")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Resampling 3.2 validation requires: ",
       paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
rejected_as <- function(expression, class) {
  inherits(
    tryCatch({
      force(expression)
      NULL
    }, error = identity),
    class
  )
}
make_path <- function(
    source_space,
    target_space = source_space,
    affine = diag(4),
    type = "affine",
    lossy = FALSE) {
  same <- identical(
    ngeo_space_hash(source_space),
    ngeo_space_hash(target_space)
  )
  registry <- ngeo_space_registry(
    if (same) list(source_space) else
      list(source_space, target_space)
  )
  if (same) {
    graph <- ngeo_transform_graph(registry)
  } else {
    transform <- ngeo_transform(
      source_space,
      target_space,
      type,
      method = "supplied validation transform",
      interpolation = if (type == "affine") "none" else "linear",
      parameters = if (type == "affine") {
        list(matrix = affine)
      } else {
        list(reference = "supplied-warp.nii.gz")
      }
    )
    graph <- ngeo_transform_graph(
      registry,
      transform,
      edge_ids = "supplied",
      lossy = lossy
    )
  }
  ngeo_transform_path(
    graph,
    ngeo_space_hash(source_space),
    ngeo_space_hash(target_space)
  )
}
volume <- function(
    space,
    affine = diag(4),
    values = NULL,
    semantics = "intensive",
    dimensions = c(2L, 2L, 2L)) {
  ngeo_volume(
    values = values,
    dim = dimensions,
    affine = affine,
    measures = if (is.null(values)) NULL else
      ngeo_measure(spatial_semantics = semantics),
    space = space,
    index_base = "zero"
  )
}
surface <- function(space, shift = 0, values = NULL) {
  coordinates <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      1, 1, 0,
      0, 1, 0
    ),
    ncol = 3L,
    byrow = TRUE
  )
  coordinates[, 1L] <- coordinates[, 1L] + shift
  ngeo_surface(
    coordinates,
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE),
    values = values,
    measures = if (is.null(values)) NULL else
      ngeo_measure(spatial_semantics = "intensive"),
    space = space
  )
}
mib <- function(x) as.numeric(x) / 1024^2

started <- Sys.time()

native <- ngeo_space("native", kind = "volume")
standard <- ngeo_space("standard", kind = "volume")
translation <- diag(4)
translation[1L, 4L] <- 1
source <- volume(
  native, values = array(seq_len(8), dim = c(2, 2, 2))
)
target <- volume(standard, affine = translation)
path <- make_path(native, standard, translation)

method_results <- list()
for (method in c("nearest", "linear", "overlap")) {
  plan <- ngeo_resampling_plan(
    source, target, path, method = method
  )
  result <- ngeo_resample(plan, authorize = TRUE)
  exact <- isTRUE(all.equal(
    as.matrix(result$support_map$operator), diag(8),
    tolerance = 1e-12
  )) && isTRUE(all.equal(result$data$values, source$values))
  assert(exact, paste(method, "volume reference differs."))
  assert(
    identical(
      result$diagnostics$joint_hash,
      result$data$provenance$resampling$joint_hash
    ),
    paste(method, "joint provenance differs.")
  )
  method_results[[method]] <- list(
    passed = TRUE,
    nonzero = result$diagnostics$nonzero,
    conservative = result$diagnostics$conservative
  )
}

surface_native <- ngeo_space(
  "native-surface", kind = "surface",
  structure = "CORTEX_LEFT"
)
surface_standard <- ngeo_space(
  "standard-surface", kind = "surface",
  structure = "CORTEX_LEFT"
)
surface_affine <- diag(4)
surface_affine[1L, 4L] <- 2
surface_source <- surface(
  surface_native, values = cbind(signal = 1:4)
)
surface_target <- surface(surface_standard, shift = 2)
surface_path <- make_path(
  surface_native, surface_standard, surface_affine
)
for (method in c("nearest", "barycentric")) {
  result <- ngeo_resample(
    ngeo_resampling_plan(
      surface_source,
      surface_target,
      surface_path,
      method = method
    ),
    authorize = TRUE
  )
  assert(
    isTRUE(all.equal(
      as.matrix(result$support_map$operator), diag(4),
      tolerance = 1e-12
    )),
    paste(method, "surface reference differs.")
  )
  method_results[[paste0("surface_", method)]] <- list(
    passed = TRUE,
    nonzero = result$diagnostics$nonzero
  )
}

same_space <- ngeo_space("coverage-grid", kind = "volume")
extensive_source <- volume(
  same_space,
  values = array(rep(1, 8), dim = c(2, 2, 2)),
  semantics = "extensive"
)
shifted <- diag(4)
shifted[1L, 4L] <- 0.25
shifted_target <- volume(same_space, affine = shifted)
identity_path <- make_path(same_space)
strict_plan <- ngeo_resampling_plan(
  extensive_source,
  shifted_target,
  identity_path,
  method = "linear",
  coverage = "drop",
  missing = "drop",
  conservation = "strict"
)
conservation_rejected <- rejected_as(
  ngeo_resample(strict_plan, authorize = TRUE),
  "ngeo_error_conservation"
)
assert(conservation_rejected,
       "Non-unit extensive allocation was not rejected.")
normalized_plan <- ngeo_resampling_plan(
  extensive_source,
  shifted_target,
  identity_path,
  method = "linear",
  coverage = "drop",
  missing = "drop",
  conservation = "normalize"
)
normalized <- ngeo_resample(
  normalized_plan, authorize = TRUE
)
extensive_conserved <- isTRUE(all.equal(
  sum(normalized$data$values), 8, tolerance = 1e-12
))
assert(extensive_conserved,
       "Authorized extensive normalization was not conservative.")

intensive_source <- volume(
  same_space,
  values = array(rep(7, 8), dim = c(2, 2, 2)),
  semantics = "intensive"
)
intensive <- ngeo_resample(
  ngeo_resampling_plan(
    intensive_source,
    shifted_target,
    identity_path,
    method = "linear",
    coverage = "drop",
    missing = "drop"
  ),
  authorize = TRUE
)
intensive_preserved <- all(
  intensive$data$values[is.finite(intensive$data$values)] == 7
)
assert(intensive_preserved,
       "Intensive support normalization changed a constant field.")

weight_variance <- Matrix::sparseMatrix(
  i = seq_len(8), j = seq_len(8),
  x = rep(0.01, 8), dims = c(8, 8)
)
uncertain_plan <- ngeo_resampling_plan(
  extensive_source,
  volume(same_space),
  identity_path,
  method = "nearest",
  uncertainty = "value_and_mapping",
  weight_variance = weight_variance
)
uncertain <- ngeo_resample(
  uncertain_plan,
  value_variance = rep(0.04, 8),
  authorize = TRUE
)
uncertainty_reference <- isTRUE(all.equal(
  as.numeric(uncertain$variance),
  rep(0.05, 8),
  tolerance = 1e-12
))
assert(uncertainty_reference,
       "Value-and-mapping variance reference differs.")

unauthorized_rejected <- rejected_as(
  ngeo_resample(ngeo_resampling_plan(
    source, target, path, method = "nearest"
  )),
  "ngeo_error_authorization"
)
lossy_path <- make_path(native, standard, translation, lossy = TRUE)
lossy_rejected <- rejected_as(
  ngeo_resampling_plan(source, target, lossy_path),
  "ngeo_error_resampling_path"
)
warp_path <- make_path(native, standard, type = "warp")
non_affine_rejected <- rejected_as(
  ngeo_resampling_plan(source, target, warp_path),
  "ngeo_error_resampling_path"
)
changed_path <- make_path(native, standard, translation)
changed_path$composed$parameters$matrix[1L, 4L] <- 99
path_mutation_rejected <- rejected_as(
  ngeo_resampling_plan(source, target, changed_path),
  "ngeo_error_transform_path_mutation"
)
mutated <- ngeo_resampling_plan(
  source, target, path, method = "nearest"
)
mutated$method <- "linear"
mutation_rejected <- rejected_as(
  ngeo_validate_resampling_plan(mutated),
  "ngeo_error_resampling_plan_mutation"
)
constrained <- ngeo_resampling_plan(
  source,
  target,
  path,
  method = "linear",
  budget = ngeo_resource_budget(
    memory_bytes = 100,
    materialized_elements = 10
  )
)
resource_rejected <- rejected_as(
  ngeo_build_resampling_map(constrained, authorize = TRUE),
  "ngeo_error_resource"
)
assert(all(c(
  unauthorized_rejected, lossy_rejected, non_affine_rejected,
  path_mutation_rejected, mutation_rejected, resource_rejected
)), "One or more adversarial plan gates failed.")

atomic_path <- tempfile(fileext = ".txt")
atomic <- ngeo_resample(
  ngeo_resampling_plan(
    source, target, path, method = "nearest"
  ),
  authorize = TRUE,
  output_path = atomic_path,
  writer = function(data, path) {
    writeLines(as.character(data$values[, 1L]), path)
  }
)$output
assert(
  inherits(atomic, "ngeo_atomic_output") &&
    file.exists(atomic_path) &&
    identical(
      atomic$sha256,
      digest::digest(
        atomic_path, algo = "sha256",
        file = TRUE, serialize = FALSE
      )
    ),
  "Atomic resampling output did not verify."
)

large_space <- ngeo_space("large-grid", kind = "volume")
large_dimensions <- c(50L, 50L, 40L)
large_n <- prod(large_dimensions)
large_source <- volume(
  large_space,
  values = as.numeric(seq_len(large_n)),
  dimensions = large_dimensions
)
large_target <- volume(
  large_space, dimensions = large_dimensions
)
large_plan <- ngeo_resampling_plan(
  large_source,
  large_target,
  make_path(large_space),
  method = "nearest",
  budget = ngeo_resource_budget(
    memory_bytes = 256 * 1024^2,
    materialized_elements = 1e6
  )
)
large_timing <- system.time(
  large_result <- ngeo_resample(
    large_plan, authorize = TRUE
  )
)
large_result_mib <- mib(object.size(large_result))
large_gate <- nrow(large_result$data$domain$elements) == large_n &&
  length(large_result$support_map$operator@x) == large_n &&
  isTRUE(all.equal(
    large_result$data$values[
      c(1L, 50000L, large_n), 1L
    ],
    c(1, 50000, large_n)
  )) &&
  large_result_mib < 256 &&
  unname(large_timing[["elapsed"]]) < 30
assert(large_gate, "100k resampling gate exceeded its contract.")

corpus <- neurogeo:::.ngeo_conformance_manifest(version = "3.2")
corpus_verified <- identical(corpus$corpus_version, "3.2") &&
  identical(corpus$schema, "NGCS-conformance-corpus-4") &&
  length(corpus$specifications) == 18L &&
  length(corpus$fixtures) == 1L
assert(corpus_verified, "NGCS 3.2 corpus failed verification.")
plan_manifest <- ngeo_object_manifest(plan)
result_manifest <- ngeo_object_manifest(result)
schema_registered <- identical(
  plan_manifest$object_schema, "ngcs/resampling-plan"
) && identical(
  result_manifest$object_schema, "ngcs/resampling-result"
) && {
  ngeo_validate(plan)
  ngeo_validate(result)
  TRUE
}
manifest_verified <- identical(
  plan_manifest$specification, "NGCS 3.2"
) && identical(
  result_manifest$specification, "NGCS 3.2"
) && ngeo_validate_manifest(plan_manifest, plan)$valid &&
  ngeo_validate_manifest(result_manifest, result)$valid
assert(schema_registered && manifest_verified,
       "NGCS 3.2 schema or manifest gate failed.")

report <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  started_at_utc = format(
    started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(packageVersion("neurogeo")),
  specification = "NGCS 3.2",
  validation = "passed",
  reference_methods = method_results,
  semantics = list(
    intensive_constant_preserved = intensive_preserved,
    strict_extensive_rejected = conservation_rejected,
    normalized_extensive_conserved = extensive_conserved
  ),
  uncertainty = list(
    value_and_mapping_reference = uncertainty_reference,
    target_variance = as.numeric(uncertain$variance)
  ),
  adversarial = list(
    authorization_required = unauthorized_rejected,
    lossy_path_rejected = lossy_rejected,
    non_affine_path_rejected = non_affine_rejected,
    path_mutation_rejected = path_mutation_rejected,
    plan_mutation_rejected = mutation_rejected,
    resource_overrun_rejected = resource_rejected
  ),
  provenance_and_output = list(
    registration_estimated = FALSE,
    implicit_resampling = FALSE,
    joint_hash_verified = TRUE,
    atomic_output_verified = TRUE
  ),
  large_gate = list(
    source_elements = large_n,
    target_elements = large_n,
    nonzero = length(large_result$support_map$operator@x),
    dense_operator_materialized = FALSE,
    resident_result_mib = large_result_mib,
    resident_limit_mib = 256,
    elapsed_seconds = unname(large_timing[["elapsed"]]),
    elapsed_limit_seconds = 30
  ),
  conformance_corpus = "3.2",
  schema_and_manifest = list(
    schemas_registered = schema_registered,
    canonical_manifests_verified = manifest_verified,
    plan_manifest_sha256 = plan_manifest$canonical_sha256,
    result_manifest_sha256 = result_manifest$canonical_sha256
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
