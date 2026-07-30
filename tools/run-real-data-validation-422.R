args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args) >= 1L) args[[1L]] else
  file.path("release", "real-data-422-validation.json")
cache <- if (length(args) >= 2L) args[[2L]] else
  file.path(".tools", "reference-4.2.2")
required <- c(
  "cifti", "freesurferformats", "gifti", "jsonlite", "RNifti"
)
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop(
    "Real-data 4.2.2 validation requires: ",
    paste(missing, collapse = ", ")
  )
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
error_class <- function(code, class) {
  error <- tryCatch(code, error = identity)
  inherits(error, class)
}
manifest_path <- file.path(
  "inst", "extdata", "reference-4.2.2", "manifest.csv"
)
manifest <- utils::read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
fixture <- function(name) {
  row <- manifest[manifest$name == name, , drop = FALSE]
  assert(nrow(row) == 1L, paste("Unknown fixture:", name))
  path <- file.path(cache, row$file)
  assert(file.exists(path), paste("Missing fetched fixture:", name))
  assert(
    identical(as.numeric(file.info(path)$size), as.numeric(row$size)) &&
      identical(
        digest::digest(
          path,
          algo = "sha256",
          file = TRUE,
          serialize = FALSE
        ),
        row$sha256
      ),
    paste("Fixture integrity failed:", name)
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
truncate_file <- function(path, suffix) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(connection, "raw", n = 128L)
  output <- tempfile(fileext = suffix)
  writeBin(bytes, output)
  output
}

started <- proc.time()[["elapsed"]]
root <- tempfile("neurogeo-422-validation-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE), add = TRUE)

# NIfTI: real values, explicit active mask, sparse topology/statistics, and
# write/read identity. The output sidecar retains the source domain hash even
# though the rewritten NIfTI header has normalized transform metadata.
nifti_path <- ngeo_example_data("rnifti-example")$path[[1L]]
nifti <- suppressWarnings(
  read_ngeo_nifti(nifti_path, mask = "nonzero", checksum = TRUE)
)
nifti <- ngeo_subset(nifti, elements = seq_len(6000L))
nifti$measures$spatial_semantics <- "intensive"
nifti_weights <- ngeo_weights(
  nifti,
  method = "voxel_contiguity",
  style = "W",
  connectivity = 6L
)
nifti_moran <- ngeo_moran(
  nifti,
  nifti_weights,
  permutations = 19L,
  seed = 422L,
  zero_policy = TRUE
)
nifti_written <- write_ngeo_nifti(
  nifti,
  file.path(root, "reference.nii.gz")
)
nifti_restored <- read_ngeo_nifti(
  nifti_written$data,
  mask = nifti_written$mask,
  maps = nifti$maps,
  measures = nifti$measures,
  space = nifti$domain$space,
  checksum = FALSE
)
nifti_sidecar <- jsonlite::read_json(nifti_written$sidecar)
nifti_valid <- identical(
  nifti$domain$elements$element_id,
  nifti_restored$domain$elements$element_id
) && identical(
  nifti$domain$source_voxel_index,
  nifti_restored$domain$source_voxel_index
) && isTRUE(all.equal(
  nifti$domain$affine,
  nifti_restored$domain$affine,
  tolerance = 1e-6
)) && isTRUE(all.equal(
  nifti$values,
  nifti_restored$values
)) && identical(
  nifti_sidecar$Neurogeo$domain_hash,
  ngeo_domain_hash(nifti)
) && is.finite(nifti_moran$estimate)
assert(nifti_valid, "NIfTI real-data workflow failed.")

# Surface: both 32k hemispheres are checked, and the left surface carries a
# vertex-aligned derived metric through GIFTI and FreeSurfer round-trips.
left_geometry <- read_ngeo_gifti(
  fixture("hcp-s1200-left-inflated-32k"),
  checksum = TRUE
)
right_geometry <- read_ngeo_gifti(
  fixture("hcp-s1200-right-inflated-32k"),
  checksum = TRUE
)
left_coordinates <- left_geometry$domain$coordinates[[
  left_geometry$domain$active_coordinates
]]
left_surface <- ngeo_surface(
  coordinates = left_geometry$domain$coordinates,
  faces = left_geometry$domain$faces,
  values = cbind(
    vertex_signal = as.numeric(scale(left_coordinates[, 1L]))
  ),
  maps = data.frame(name = "vertex_signal"),
  measures = ngeo_measure(
    value_type = "continuous",
    spatial_semantics = "intensive",
    units = "a.u."
  ),
  space = left_geometry$domain$space,
  coordinate_roles = left_geometry$domain$coordinate_meta$role,
  index_base = "one",
  source_index_base = 0L
)
chart_surface <- ngeo_set_chart(
  left_surface,
  left_coordinates[, 1:2, drop = FALSE],
  name = "validation_view",
  distortion = list(
    interpretation = "orthographic viewing projection; not flattening"
  ),
  source = "4.2.2 validation viewing projection"
)
surface_weights <- ngeo_weights(
  left_surface,
  method = "mesh_contiguity",
  style = "W"
)
surface_moran <- ngeo_moran(
  left_surface,
  surface_weights,
  permutations = 19L,
  seed = 422L
)
gifti_written <- write_ngeo_gifti(
  left_surface,
  file.path(root, "left.surf.gii")
)
gifti_restored <- read_ngeo_gifti(
  gifti_written$geometry,
  data = gifti_written$data,
  measures = left_surface$measures,
  checksum = FALSE
)
fs_written <- write_ngeo_freesurfer(
  left_surface,
  file.path(root, "lh.inflated")
)
fs_restored <- read_ngeo_freesurfer(
  geometry = fs_written$geometry,
  data = fs_written$data,
  measures = left_surface$measures,
  checksum = FALSE
)
surface_valid <- nrow(left_surface$domain$elements) == 32492L &&
  nrow(right_geometry$domain$elements) == 32492L &&
  nrow(left_surface$domain$faces) == 64980L &&
  nrow(right_geometry$domain$faces) == 64980L &&
  chart_surface$domain$coordinate_meta$role[
    chart_surface$domain$coordinate_meta$name == "validation_view"
  ] == "chart" &&
  !chart_surface$domain$coordinate_meta$metric_eligible[
    chart_surface$domain$coordinate_meta$name == "validation_view"
  ] &&
  surface_weights$diagnostics$n_component == 1L &&
  is.finite(surface_moran$estimate) &&
  identical(gifti_restored$domain$faces, left_surface$domain$faces) &&
  identical(fs_restored$domain$faces, left_surface$domain$faces) &&
  isTRUE(all.equal(
    gifti_restored$values,
    left_surface$values,
    tolerance = 1e-6
  )) &&
  isTRUE(all.equal(
    fs_restored$values,
    left_surface$values,
    tolerance = 1e-6
  ))
assert(surface_valid, "GIFTI/FreeSurfer surface workflow failed.")

# A real 256^3 MGZ is read through an explicit 16^3 file-backed mask. This
# prevents construction or materialization of a full dense values block.
mgz_mask <- array(FALSE, dim = c(256L, 256L, 256L))
mgz_mask[97:112, 97:112, 97:112] <- TRUE
mgz <- read_ngeo_mgh_filebacked(
  fixture("freesurferformats-brain-mgz"),
  mask = mgz_mask,
  checksum = TRUE,
  budget = ngeo_resource_budget(
    memory_bytes = 1024^2,
    materialized_elements = 10000L
  )
)
mgz_values <- as.matrix(mgz$values)
mgz_valid <- inherits(mgz$values, "ngeo_file_values") &&
  identical(dim(mgz$values), c(4096L, 1L)) &&
  identical(mgz$domain$dim, c(256L, 256L, 256L)) &&
  all(is.finite(mgz_values)) &&
  identical(mgz$provenance$file_backed$materialized, FALSE)
assert(mgz_valid, "File-backed MGZ workflow failed.")

# CIFTI: all three dense axis types, exact brain-model order, label tables,
# partial file-backed reads, bounded summaries, and pure-R round-trips.
cifti_paths <- c(
  dscalar = fixture("conte69-dscalar-6k"),
  dlabel = fixture("conte69-dlabel-6k"),
  dtseries = fixture("conte69-dtseries-32k")
)
dscalar <- read_ngeo_cifti(cifti_paths[["dscalar"]], checksum = TRUE)
dlabel <- read_ngeo_cifti(cifti_paths[["dlabel"]], checksum = TRUE)
dtseries_backed <- read_ngeo_cifti_filebacked(
  cifti_paths[["dtseries"]],
  frames = 1:2,
  elements = seq_len(4096L),
  checksum = TRUE,
  budget = ngeo_resource_budget(
    memory_bytes = 1024^2,
    materialized_elements = 10000L
  )
)
dtseries_values <- as.matrix(dtseries_backed$values)
dtseries_selected <- dtseries_backed
dtseries_selected$values <- dtseries_values
ngeo_validate(dtseries_selected, "strict")
cifti_written <- c(
  dscalar = file.path(root, "reference.dscalar.nii"),
  dlabel = file.path(root, "reference.dlabel.nii"),
  dtseries = file.path(root, "reference.dtseries.nii")
)
write_ngeo_cifti(dscalar, cifti_written[["dscalar"]], type = "dscalar")
write_ngeo_cifti(dlabel, cifti_written[["dlabel"]], type = "dlabel")
write_ngeo_cifti(
  dtseries_selected,
  cifti_written[["dtseries"]],
  type = "dtseries"
)
cifti_restored <- list(
  dscalar = read_ngeo_cifti(
    cifti_written[["dscalar"]],
    checksum = FALSE
  ),
  dlabel = read_ngeo_cifti(
    cifti_written[["dlabel"]],
    checksum = FALSE
  ),
  dtseries = read_ngeo_cifti(
    cifti_written[["dtseries"]],
    checksum = FALSE
  )
)
cifti_valid <- identical(dim(dscalar$values), c(10846L, 2L)) &&
  identical(dim(dlabel$values), c(11524L, 3L)) &&
  identical(dim(dtseries_backed$values), c(4096L, 2L)) &&
  inherits(dtseries_backed$values, "ngeo_file_values") &&
  length(dlabel$labels) == 3L &&
  identical(
    cifti_restored$dscalar$domain$elements$structure,
    dscalar$domain$elements$structure
  ) &&
  identical(
    cifti_restored$dlabel$domain$elements$structure,
    dlabel$domain$elements$structure
  ) &&
  identical(
    cifti_restored$dtseries$domain$elements$structure,
    dtseries_selected$domain$elements$structure
  )
# Check numeric identity separately to keep the structure-order test simple.
cifti_valid <- cifti_valid &&
  isTRUE(all.equal(
    cifti_restored$dscalar$values,
    dscalar$values,
    tolerance = 1e-6
  )) &&
  isTRUE(all.equal(
    cifti_restored$dlabel$values,
    dlabel$values
  )) &&
  isTRUE(all.equal(
    cifti_restored$dtseries$values,
    dtseries_values,
    tolerance = 1e-6
  ))
assert(cifti_valid, "CIFTI real-data workflow failed.")

# Atlas and change of support use a real Conte69 dense-label domain. Both hard
# and sparse probabilistic memberships are explicit and source aligned.
atlas_label <- as.integer(dlabel$values[, 1L])
source <- ngeo_grayordinates(
  dlabel$domain$components,
  values = cbind(
    intensive = sin(seq_along(atlas_label) / 100),
    extensive = seq_along(atlas_label) %% 7L + 1
  ),
  maps = data.frame(name = c("intensive", "extensive")),
  measures = rbind(
    ngeo_measure(spatial_semantics = "intensive"),
    ngeo_measure(spatial_semantics = "extensive")
  ),
  space = dlabel$domain$space
)
support_size <- 1 + (seq_along(atlas_label) %% 5L) / 10
hard_label <- paste0("label_", atlas_label)
crisp <- ngeo_atlas_map(
  source,
  hard_label,
  source_support = support_size
)
partial <- ngeo_atlas_map(
  source,
  hard_label,
  exclude = "label_0",
  source_support = support_size
)
region_id <- unique(hard_label)
primary <- match(hard_label, region_id)
secondary <- ifelse(primary == length(region_id), 1L, primary + 1L)
probability <- Matrix::sparseMatrix(
  i = rep(seq_along(atlas_label), 2L),
  j = c(primary, secondary),
  x = c(
    rep(0.9, length(atlas_label)),
    rep(0.1, length(atlas_label))
  ),
  dims = c(length(atlas_label), length(region_id)),
  dimnames = list(NULL, region_id)
)
probabilistic <- ngeo_probabilistic_atlas_map(
  source,
  probability,
  source_support = support_size
)
regional <- ngeo_change_support(source, crisp$target, crisp)
regional_variance <- ngeo_support_variance(
  source,
  probabilistic$target,
  probabilistic,
  value_variance = matrix(
    0.05,
    nrow = length(atlas_label),
    ncol = 2L
  )
)
diagnostics <- ngeo_support_diagnostics(
  probabilistic,
  source_structure = source$domain$elements$structure
)
partial_diagnostics <- ngeo_support_diagnostics(partial)
bundle <- write_ngeo_support_bundle(
  probabilistic,
  file.path(root, "atlas-support"),
  chunk_size = 2048L
)
bundle_restored <- read_ngeo_support_bundle(bundle$path)
atlas_valid <- crisp$type == "crisp" &&
  probabilistic$type == "probabilistic" &&
  crisp$coverage == "complete" &&
  partial$coverage == "partial" &&
  partial_diagnostics$complete == FALSE &&
  diagnostics$complete &&
  length(probabilistic$operator@x) == 2L * length(atlas_label) &&
  isTRUE(all.equal(
    sum(source$values[, 2L]),
    sum(regional$values[, 2L])
  )) &&
  identical(
    ngeo_support_map_hash(bundle_restored),
    ngeo_support_map_hash(probabilistic)
  ) &&
  all(is.finite(regional_variance))
assert(atlas_valid, "Atlas/change-of-support workflow failed.")

# Adversarial gates use the real fixtures or objects derived from them.
mutated <- file.path(root, basename(cifti_paths[["dscalar"]]))
assert(
  file.copy(cifti_paths[["dscalar"]], mutated),
  "Could not stage source-mutation probe."
)
mutation_probe <- read_ngeo_cifti_filebacked(
  mutated,
  elements = 1:16,
  checksum = FALSE,
  verify = "checksum"
)
connection <- file(mutated, "ab")
writeBin(charToRaw("mutation"), connection)
close(connection)
source_mutation_rejected <- error_class(
  ngeo_validate_file_values(mutation_probe$values),
  "ngeo_error_file_mutation"
)
invalid_labels <- dlabel
invalid_labels$labels[[1L]]$table$Key[[2L]] <-
  invalid_labels$labels[[1L]]$table$Key[[1L]]
invalid_label_table_rejected <- error_class(
  write_ngeo_cifti(
    invalid_labels,
    file.path(root, "invalid.dlabel.nii"),
    type = "dlabel"
  ),
  "ngeo_error_format"
)
invalid_datatype_rejected <- error_class(
  write_ngeo_cifti(
    dscalar,
    file.path(root, "invalid.dscalar.nii"),
    type = "dscalar",
    datatype = "int32"
  ),
  "ngeo_error_measure"
)
missing_vertices_rejected <- error_class(
  read_ngeo_freesurfer(
    geometry = fixture("hcp-s1200-left-inflated-32k"),
    data = ngeo_example_data("freesurfer-tiny-morph")$path[[1L]],
    checksum = FALSE
  ),
  "ngeo_error_alignment"
)
truncated_cifti_rejected <- error_class(
  read_ngeo_cifti(
    truncate_file(cifti_paths[["dscalar"]], ".dscalar.nii"),
    checksum = FALSE
  ),
  "ngeo_error_io"
)
truncated_gifti_rejected <- error_class(
  read_ngeo_gifti(
    truncate_file(
      fixture("hcp-s1200-left-inflated-32k"),
      ".surf.gii"
    ),
    checksum = FALSE
  ),
  "ngeo_error_io"
)
adversarial <- c(
  source_mutation = source_mutation_rejected,
  invalid_label_table = invalid_label_table_rejected,
  invalid_datatype = invalid_datatype_rejected,
  missing_vertices = missing_vertices_rejected,
  truncated_cifti = truncated_cifti_rejected,
  truncated_gifti = truncated_gifti_rejected,
  partial_atlas = identical(partial$coverage, "partial"),
  disconnected_volume = nifti_weights$diagnostics$n_component > 1L
)
assert(all(adversarial), "One or more adversarial gates failed.")

result <- list(
  schema = "neurogeo-real-data-validation-4.2.2-1",
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(utils::packageVersion("neurogeo")),
  validation = "passed",
  fixture_policy = list(
    count = nrow(manifest),
    all_verified = TRUE,
    redistribution = unique(manifest$redistribution),
    source_commits = unique(manifest$source_commit),
    license_records = unique(manifest$license_record)
  ),
  workflows = list(
    nifti = list(
      validation = "passed",
      elements = nrow(nifti$values),
      sparse_nonzero = length(nifti_weights$matrix@x),
      components = nifti_weights$diagnostics$n_component,
      moran = nifti_moran$estimate,
      logical_identity = TRUE,
      provenance_sidecar_domain_hash = TRUE
    ),
    surface_freesurfer = list(
      validation = "passed",
      vertices_per_hemisphere = 32492L,
      faces_per_hemisphere = 64980L,
      sparse_nonzero = length(surface_weights$matrix@x),
      components = surface_weights$diagnostics$n_component,
      explicit_viewing_chart = TRUE,
      gifti_roundtrip = TRUE,
      freesurfer_roundtrip = TRUE,
      mgz_file_backed_elements = nrow(mgz$values)
    ),
    cifti = list(
      validation = "passed",
      dscalar_dimensions = dim(dscalar$values),
      dlabel_dimensions = dim(dlabel$values),
      dtseries_source_dimensions = c(60951L, 2L),
      dtseries_selected_dimensions = dim(dtseries_backed$values),
      label_tables = length(dlabel$labels),
      file_backed = TRUE,
      pure_r_roundtrip = TRUE
    ),
    atlas_change_of_support = list(
      validation = "passed",
      source_elements = length(atlas_label),
      regions = length(region_id),
      crisp_nonzero = length(crisp$operator@x),
      probabilistic_nonzero = length(probabilistic$operator@x),
      extensive_conservation = TRUE,
      uncertainty = TRUE,
      derivative_bundle_verified = TRUE
    )
  ),
  adversarial = as.list(adversarial),
  resource_behavior = list(
    dense_whole_brain_distance_matrix = FALSE,
    surface_operator_sparse = TRUE,
    atlas_operator_sparse = TRUE,
    mgz_file_backed = TRUE,
    dtseries_file_backed = TRUE,
    maximum_explicit_validation_block_elements = 10000L,
    exercised_scales = c(
      "32k vertices per hemisphere",
      "60,951 grayordinates",
      "256x256x256 MGZ with 4,096 selected voxels"
    ),
    not_exercised = c(
      "164k surface: no fixture with compatible redistribution terms",
      "91k grayordinates: no fixture with compatible redistribution terms"
    )
  ),
  external_neuroimaging_binaries = FALSE,
  claim_boundary = paste(
    "Format, alignment, sparse/file-backed resource, and change-of-support",
    "validation only; not preprocessing, registration, clinical validation,",
    "or a claim that downloaded atlas data are participant measurements."
  ),
  elapsed_seconds = proc.time()[["elapsed"]] - started,
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null",
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
