args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args) >= 1L) args[[1L]] else
  file.path("release", "cartography-43-validation.json")
cache <- if (length(args) >= 2L) args[[2L]] else
  file.path(".tools", "reference-4.2.2")
required <- c("cifti", "freesurferformats", "gifti", "jsonlite")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop(
    "Cartography 4.3 validation requires: ",
    paste(missing, collapse = ", ")
  )
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
started <- proc.time()[["elapsed"]]
root <- tempfile("neurogeo-cartography-43-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE), add = TRUE)

# A disk with interior vertices exercises the sparse harmonic solve and exact
# topology invariants without pretending to cut a closed cortical surface.
nx <- 40L
ny <- 40L
id <- matrix(seq_len(nx * ny), nrow = nx, ncol = ny)
coordinates <- as.matrix(expand.grid(
  x = seq(-1, 1, length.out = nx),
  y = seq(-1, 1, length.out = ny)
))
coordinates <- cbind(
  coordinates,
  z = 0.08 * sin(3 * coordinates[, 1L]) *
    cos(2 * coordinates[, 2L])
)
faces <- matrix(integer(), nrow = 2L * (nx - 1L) * (ny - 1L), ncol = 3L)
row <- 1L
for (j in seq_len(ny - 1L)) {
  for (i in seq_len(nx - 1L)) {
    a <- id[i, j]
    b <- id[i + 1L, j]
    c <- id[i + 1L, j + 1L]
    d <- id[i, j + 1L]
    faces[row, ] <- c(a, b, c)
    faces[row + 1L, ] <- c(a, c, d)
    row <- row + 2L
  }
}
boundary <- c(
  id[, 1L],
  id[nx, 2:ny],
  rev(id[seq_len(nx - 1L), ny]),
  rev(id[1L, 2:(ny - 1L)])
)
disk <- ngeo_surface(
  coordinates,
  faces,
  values = cbind(signal = coordinates[, 3L]),
  measures = ngeo_measure(spatial_semantics = "intensive")
)
harmonic <- ngeo_flatten_surface(
  disk,
  "harmonic",
  boundary = boundary,
  name = "harmonic"
)
harmonic_metadata <- harmonic$domain$charts$harmonic
harmonic_valid <- harmonic_metadata$invariants$disk &&
  harmonic_metadata$invariants$euler_characteristic == 1L &&
  harmonic_metadata$invariants$connected_components == 1L &&
  harmonic_metadata$distortion_summary$folded_faces == 0L &&
  all(is.finite(harmonic$domain$coordinates$harmonic))
assert(harmonic_valid, "Harmonic disk validation failed.")

# A 164k-class mesh validates bounded chart/map exchange without producing a
# very large vector graphic. No dense vertex-by-vertex object is constructed.
scale_started <- proc.time()[["elapsed"]]
large_n <- 405L
large_id <- matrix(
  seq_len(large_n * large_n),
  nrow = large_n,
  ncol = large_n
)
large_coordinates <- as.matrix(expand.grid(
  x = seq(-1, 1, length.out = large_n),
  y = seq(-1, 1, length.out = large_n)
))
large_coordinates <- cbind(large_coordinates, z = 0)
large_a <- as.vector(large_id[-large_n, -large_n])
large_b <- as.vector(large_id[-1L, -large_n])
large_c <- as.vector(large_id[-1L, -1L])
large_d <- as.vector(large_id[-large_n, -1L])
large_faces <- rbind(
  cbind(large_a, large_b, large_c),
  cbind(large_a, large_c, large_d)
)
large <- ngeo_surface(
  large_coordinates,
  large_faces,
  values = cbind(signal = sin(large_coordinates[, 1L] * pi)),
  measures = ngeo_measure(spatial_semantics = "intensive")
)
large <- ngeo_flatten_surface(
  large,
  "imported",
  coordinates = large_coordinates[, 1:2, drop = FALSE],
  name = "flat"
)
large_map <- ngeo_cortical_map(large, "signal", chart = "flat")
large_exchange <- ngeo_cortical_map_data(large_map)
large_elapsed <- proc.time()[["elapsed"]] - scale_started
large_bytes <- as.numeric(utils::object.size(large_map))
large_valid <- nrow(large_exchange$vertices) == 164025L &&
  nrow(large_exchange$faces) == 326432L &&
  large$domain$charts$flat$distortion_summary$folded_faces == 0L &&
  identical(
    large_exchange$vertices$source_vertex,
    seq_len(164025L)
  ) &&
  large_elapsed < 45 &&
  large_bytes < 300 * 1024^2
assert(large_valid, "164k cartography scale validation failed.")
rm(
  large,
  large_map,
  large_exchange,
  large_coordinates,
  large_faces,
  large_a,
  large_b,
  large_c,
  large_d,
  large_id
)
invisible(gc())

# Real HCP 32k surface: only a viewing projection is created because the
# closed surface is not cut and no registration is estimated.
hcp_left_path <- file.path(
  cache,
  "S1200.L.inflated_MSMAll.32k_fs_LR.surf.gii"
)
hcp_right_path <- file.path(
  cache,
  "S1200.R.inflated_MSMAll.32k_fs_LR.surf.gii"
)
dtseries_path <- file.path(
  cache,
  "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii"
)
assert(
  all(file.exists(c(hcp_left_path, hcp_right_path, dtseries_path))),
  "Fetch the 4.2.2 reference cache first."
)
dtseries <- read_ngeo_cifti(dtseries_path, checksum = TRUE)
make_hcp <- function(path, component_name) {
  geometry <- read_ngeo_gifti(path, checksum = TRUE)
  component <- dtseries$domain$components[[component_name]]
  metric <- rep.int(NA_real_, component$surface_vertex_count)
  metric[component$internal_vertex_index] <-
    dtseries$values[component$global_rows, 1L]
  result <- ngeo_surface(
    geometry$domain$coordinates,
    geometry$domain$faces,
    values = cbind(cifti_vertex_value = metric),
    maps = data.frame(name = "cifti_vertex_value"),
    measures = ngeo_measure(
      value_type = "continuous",
      spatial_semantics = "intensive"
    ),
    space = geometry$domain$space,
    coordinate_roles = geometry$domain$coordinate_meta$role,
    index_base = "one",
    source_index_base = 0L
  )
  ngeo_project_surface(result, "pca", name = "pca_view")
}
hcp <- make_hcp(hcp_left_path, "cortex_left")
hcp_right <- make_hcp(hcp_right_path, "cortex_right")
centered <- sweep(
  hcp$domain$coordinates$pca_view,
  2L,
  colMeans(hcp$domain$coordinates$pca_view),
  "-"
)
angle <- atan2(centered[, 2L], centered[, 1L])
atlas <- paste0(
  "sector_",
  1L + floor(((angle + pi) %% (2 * pi)) / (2 * pi) * 8L)
)
continuous <- ngeo_cortical_map(
  hcp,
  "cifti_vertex_value",
  chart = "pca_view",
  atlas = atlas
)
right_continuous <- ngeo_cortical_map(
  hcp_right,
  "cifti_vertex_value",
  chart = "pca_view"
)
categorical <- ngeo_cortical_map(
  hcp,
  values = atlas,
  chart = "pca_view",
  atlas = atlas,
  palette = "Set 3"
)
exchange <- ngeo_cortical_map_data(continuous)
right_exchange <- ngeo_cortical_map_data(right_continuous)
layout <- ngeo_cortical_layout(
  continuous,
  right_continuous,
  categorical,
  ncol = 3L,
  labels = c(
    "left real CIFTI vertex metric",
    "right real CIFTI vertex metric",
    "left arbitrary aligned atlas"
  )
)
svg <- file.path(root, "hcp-32k-cartography.svg")
grDevices::svg(svg, width = 18, height = 6)
plot(layout, show_boundaries = TRUE)
grDevices::dev.off()

hcp_valid <- nrow(exchange$vertices) == 32492L &&
  nrow(exchange$faces) == 64980L &&
  nrow(right_exchange$vertices) == 32492L &&
  nrow(right_exchange$faces) == 64980L &&
  nrow(exchange$boundaries) > 0L &&
  hcp$domain$charts$pca_view$kind == "view_projection" &&
  !hcp$domain$charts$pca_view$is_metric_flattening &&
  identical(
    exchange$vertices$element_id,
    hcp$domain$elements$element_id
  ) &&
  identical(exchange$faces$source_face, seq_len(64980L)) &&
  identical(
    right_exchange$vertices$element_id,
    hcp_right$domain$elements$element_id
  ) &&
  sum(is.finite(hcp$values[, 1L])) == 30424L &&
  sum(is.finite(hcp_right$values[, 1L])) == 30527L &&
  file.info(svg)$size > 100000L
assert(hcp_valid, "Real 32k cartography workflow failed.")

# Closed surfaces must fail before output; explicit spherical seams and
# arbitrary imported vertex charts remain available.
closed_rejected <- inherits(
  tryCatch(
    ngeo_flatten_surface(
      hcp,
      "harmonic",
      boundary = seq_len(100L)
    ),
    error = identity
  ),
  "ngeo_error_topology"
)
missing_seam_rejected <- inherits(
  tryCatch(
    ngeo_project_surface(hcp, "spherical"),
    error = identity
  ),
  "ngeo_error_chart"
)
atlas_alignment_rejected <- inherits(
  tryCatch(
    ngeo_cortical_map(
      hcp,
      chart = "pca_view",
      atlas = c("A", "B")
    ),
    error = identity
  ),
  "ngeo_error_alignment"
)
sphere <- ngeo_project_surface(
  hcp,
  "spherical",
  seam = 0,
  name = "sphere_view"
)
seam_valid <- identical(sphere$domain$charts$sphere_view$seam, 0) &&
  sphere$domain$charts$sphere_view$kind == "view_projection"
assert(
  closed_rejected && missing_seam_rejected &&
    atlas_alignment_rejected && seam_valid,
  "Closed-surface or seam boundary validation failed."
)

elapsed <- proc.time()[["elapsed"]] - started
assert(elapsed < 120, "Cartography validation exceeded 120 seconds.")
result <- list(
  schema = "neurogeo-cortical-cartography-validation-4.3-1",
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(utils::packageVersion("neurogeo")),
  validation = "passed",
  harmonic = list(
    vertices = nrow(harmonic$domain$elements),
    faces = nrow(harmonic$domain$faces),
    boundary_vertices = length(boundary),
    euler_characteristic =
      harmonic_metadata$invariants$euler_characteristic,
    folded_faces = harmonic_metadata$distortion_summary$folded_faces,
    sparse_solver = TRUE
  ),
  scale_164k = list(
    vertices = 164025L,
    faces = 326432L,
    map_object_bytes = large_bytes,
    elapsed_seconds = large_elapsed,
    semantic_exchange_only = TRUE,
    dense_all_pairs_object = FALSE
  ),
  real_surface = list(
    source_surfaces = c(
      basename(hcp_left_path),
      basename(hcp_right_path)
    ),
    source_values = basename(dtseries_path),
    left_vertices = nrow(exchange$vertices),
    right_vertices = nrow(right_exchange$vertices),
    left_faces = nrow(exchange$faces),
    right_faces = nrow(right_exchange$faces),
    left_cifti_vertices = sum(is.finite(hcp$values[, 1L])),
    right_cifti_vertices = sum(is.finite(hcp_right$values[, 1L])),
    atlas_regions = length(unique(atlas)),
    atlas_kind = "explicit aligned eight-sector stress fixture",
    atlas_boundary_edges = nrow(exchange$boundaries),
    source_vertex_mapping = TRUE,
    source_face_mapping = TRUE,
    pca_is_view_projection = TRUE,
    svg_rendered = TRUE,
    svg_bytes = unname(file.info(svg)$size)
  ),
  methods = list(
    imported_parameterization = TRUE,
    harmonic_disk_parameterization = TRUE,
    orthographic_view = TRUE,
    pca_view = TRUE,
    spherical_view_with_explicit_seam = TRUE,
    automatic_cutting = FALSE,
    registration_estimation = FALSE
  ),
  adversarial = list(
    closed_surface_rejected = closed_rejected,
    missing_spherical_seam_rejected = missing_seam_rejected,
    atlas_alignment_rejected = atlas_alignment_rejected
  ),
  external_neuroimaging_binaries = FALSE,
  elapsed_seconds = elapsed,
  platform = R.version$platform,
  r_version = R.version.string,
  claim_boundary = paste(
    "Rendering and chart validation only; PCA, orthographic, and spherical",
    "outputs are viewing projections, not metric flattening or anatomical",
    "registration. Harmonic flattening applies only to an explicit disk."
  )
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
