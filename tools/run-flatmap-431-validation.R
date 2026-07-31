args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args) >= 1L) args[[1L]] else
  file.path("release", "flatmap-431-validation.json")
core_cache <- if (length(args) >= 2L) args[[2L]] else
  file.path(".tools", "reference-4.2.2")
flat_cache <- if (length(args) >= 3L) args[[3L]] else
  file.path(".tools", "reference-flatmap")
figure_dir <- if (length(args) >= 4L) args[[4L]] else
  file.path("release", "flatmap-431")

required <- c("cifti", "gifti", "jsonlite")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop(
    "Flatmap 4.3.1 validation requires: ",
    paste(missing, collapse = ", ")
  )
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

vertex_curvature <- function(surface) {
  coordinates <-
    surface$domain$coordinates[[surface$domain$active_coordinates]]
  faces <- surface$domain$faces
  first <- coordinates[faces[, 1L], , drop = FALSE]
  second <- coordinates[faces[, 2L], , drop = FALSE]
  third <- coordinates[faces[, 3L], , drop = FALSE]
  face_normal <- cbind(
    (second[, 2L] - first[, 2L]) *
      (third[, 3L] - first[, 3L]) -
      (second[, 3L] - first[, 3L]) *
        (third[, 2L] - first[, 2L]),
    (second[, 3L] - first[, 3L]) *
      (third[, 1L] - first[, 1L]) -
      (second[, 1L] - first[, 1L]) *
        (third[, 3L] - first[, 3L]),
    (second[, 1L] - first[, 1L]) *
      (third[, 2L] - first[, 2L]) -
      (second[, 2L] - first[, 2L]) *
        (third[, 1L] - first[, 1L])
  )
  normal <- matrix(0, nrow = nrow(coordinates), ncol = 3L)
  for (corner in seq_len(3L)) {
    contribution <- rowsum(
      face_normal,
      faces[, corner],
      reorder = FALSE
    )
    index <- as.integer(rownames(contribution))
    normal[index, ] <- normal[index, , drop = FALSE] + contribution
  }
  magnitude <- sqrt(rowSums(normal^2))
  normal[magnitude > 0, ] <- normal[magnitude > 0, , drop = FALSE] /
    magnitude[magnitude > 0]

  adjacency <- ngeo_adjacency(surface, method = "mesh")
  degree <- Matrix::rowSums(adjacency)
  neighbour_mean <- Matrix::Diagonal(x = 1 / degree) %*%
    adjacency %*% coordinates
  displacement <- as.matrix(neighbour_mean) - coordinates
  rowSums(displacement * normal)
}

dtseries_path <- file.path(
  core_cache,
  "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii"
)
assert(file.exists(dtseries_path), "The 4.2.2 Conte69 fixture is missing.")
dtseries <- read_ngeo_cifti(dtseries_path, checksum = TRUE)

make_hemisphere <- function(hemisphere, component_name) {
  prefix <- paste0("S1200.", hemisphere)
  geometry_path <- file.path(
    flat_cache,
    paste0(prefix, ".midthickness_MSMAll.32k_fs_LR.surf.gii")
  )
  flat_path <- file.path(
    flat_cache,
    paste0(prefix, ".flat.32k_fs_LR.surf.gii")
  )
  mask_path <- file.path(
    flat_cache,
    paste0(hemisphere, ".atlasroi.32k_fs_LR.shape.gii")
  )
  label_path <- file.path(
    flat_cache,
    paste0(
      "schaefer-100_conte69_",
      if (hemisphere == "L") "lh" else "rh",
      ".label.gii"
    )
  )
  assert(
    all(file.exists(c(geometry_path, flat_path, mask_path, label_path))),
    paste("Flatmap fixtures are missing for hemisphere", hemisphere)
  )

  source <- read_ngeo_gifti(
    geometry = geometry_path,
    data = c(atlasroi = mask_path),
    labels = c(schaefer100 = label_path),
    checksum = TRUE
  )
  flat <- read_ngeo_gifti(flat_path, checksum = TRUE)
  component <- dtseries$domain$components[[component_name]]
  metric <- rep.int(NA_real_, component$surface_vertex_count)
  metric[component$internal_vertex_index] <-
    dtseries$values[component$global_rows, 1L]
  curvature <- vertex_curvature(source)
  included <- source$values[, "atlasroi"] > 0

  result <- ngeo_surface(
    coordinates = source$domain$coordinates,
    faces = source$domain$faces,
    values = cbind(
      vertex_metric = metric,
      curvature = curvature
    ),
    maps = data.frame(
      name = c("vertex_metric", "curvature"),
      stringsAsFactors = FALSE
    ),
    labels = source$labels,
    space = source$domain$space,
    coordinate_roles = source$domain$coordinate_meta$role,
    mask = included,
    index_base = "one",
    source_index_base = 0L
  )
  ngeo_flatten_surface(
    result,
    method = "imported",
    coordinates = flat,
    name = "flat"
  )
}

network_membership <- function(surface) {
  parcel <- ngeo_cortical_map(
    surface,
    chart = "flat",
    atlas = "schaefer100",
    fill = "atlas"
  )$vertices$atlas
  network <- sub(
    "^7Networks_[LR]H_([^_]+).*$",
    "\\1",
    parcel
  )
  network[parcel == "???" | is.na(parcel)] <- NA_character_
  network
}

started <- proc.time()[["elapsed"]]
left <- make_hemisphere("L", "cortex_left")
right <- make_hemisphere("R", "cortex_right")
limits <- range(
  c(left$values[, "vertex_metric"], right$values[, "vertex_metric"]),
  finite = TRUE
)
underlay_limits <- stats::quantile(
  c(left$values[, "curvature"], right$values[, "curvature"]),
  c(0.02, 0.98),
  na.rm = TRUE,
  names = FALSE
)

left_continuous <- ngeo_cortical_map(
  left,
  map = "vertex_metric",
  chart = "flat",
  atlas = "schaefer100",
  mask = left$domain$mask,
  underlay = "curvature",
  underlay_palette = "Grays",
  underlay_limits = underlay_limits,
  overlay_alpha = 0.78,
  palette = "viridis",
  limits = limits,
  na_color = NA_character_
)
right_continuous <- ngeo_cortical_map(
  right,
  map = "vertex_metric",
  chart = "flat",
  atlas = "schaefer100",
  mask = right$domain$mask,
  underlay = "curvature",
  underlay_palette = "Grays",
  underlay_limits = underlay_limits,
  overlay_alpha = 0.78,
  palette = "viridis",
  limits = limits,
  na_color = NA_character_
)

network_colors <- c(
  Vis = "#6A3D9A",
  SomMot = "#2B8CBE",
  DorsAttn = "#41AB5D",
  SalVentAttn = "#E6550D",
  Limbic = "#FDD0A2",
  Cont = "#E31A1C",
  Default = "#FFD92F"
)
left_network <- network_membership(left)
right_network <- network_membership(right)
left_atlas <- ngeo_cortical_map(
  left,
  chart = "flat",
  atlas = left_network,
  fill = "atlas",
  mask = left$domain$mask,
  underlay = "curvature",
  underlay_palette = "Grays",
  underlay_limits = underlay_limits,
  colors = network_colors,
  overlay_alpha = 0.82,
  na_color = NA_character_
)
right_atlas <- ngeo_cortical_map(
  right,
  chart = "flat",
  atlas = right_network,
  fill = "atlas",
  mask = right$domain$mask,
  underlay = "curvature",
  underlay_palette = "Grays",
  underlay_limits = underlay_limits,
  colors = network_colors,
  overlay_alpha = 0.82,
  na_color = NA_character_
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
continuous_figure <- file.path(figure_dir, "conte69-vertex-flatmap.png")
grDevices::png(
  continuous_figure,
  width = 2600,
  height = 1250,
  res = 180,
  bg = "white"
)
graphics::par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
plot(
  left_continuous,
  main = "Left hemisphere: vertex metric",
  boundary_lwd = 0.18,
  boundary_color = grDevices::adjustcolor("white", 0.38),
  outline_lwd = 1.4,
  show_legend = TRUE
)
plot(
  right_continuous,
  main = "Right hemisphere: vertex metric",
  boundary_lwd = 0.18,
  boundary_color = grDevices::adjustcolor("white", 0.38),
  outline_lwd = 1.4,
  show_legend = TRUE
)
grDevices::dev.off()

atlas_figure <- file.path(figure_dir, "conte69-atlas-flatmap.png")
grDevices::png(
  atlas_figure,
  width = 2600,
  height = 1250,
  res = 180,
  bg = "white"
)
graphics::par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
plot(
  left_atlas,
  main = "Left hemisphere: Schaefer 7 networks",
  boundary_lwd = 0.55,
  outline_lwd = 1.4,
  show_labels = TRUE,
  label_cex = 0.42,
  show_legend = FALSE
)
plot(
  right_atlas,
  main = "Right hemisphere: Schaefer 7 networks",
  boundary_lwd = 0.55,
  outline_lwd = 1.4,
  show_labels = TRUE,
  label_cex = 0.42,
  show_legend = FALSE
)
grDevices::dev.off()

left_data <- ngeo_cortical_map_data(left_continuous)
right_data <- ngeo_cortical_map_data(right_continuous)
left_chart <- left$domain$charts$flat$invariants
right_chart <- right$domain$charts$flat$invariants
elapsed <- proc.time()[["elapsed"]] - started
valid <- nrow(left_data$vertices) == 32492L &&
  nrow(right_data$vertices) == 32492L &&
  sum(left_data$faces$included) > 50000L &&
  sum(right_data$faces$included) > 50000L &&
  nrow(left_data$outline) > 0L &&
  nrow(right_data$outline) > 0L &&
  nrow(left_data$boundaries) > 0L &&
  nrow(right_data$boundaries) > 0L &&
  left_chart$topology_verified &&
  right_chart$topology_verified &&
  identical(left_chart$topology_relation, "face_subset") &&
  identical(right_chart$topology_relation, "face_subset") &&
  length(left_chart$source_face_in_chart) > 50000L &&
  length(right_chart$source_face_in_chart) > 50000L &&
  length(left_chart$source_face_in_chart) < nrow(left_data$faces) &&
  length(right_chart$source_face_in_chart) < nrow(right_data$faces) &&
  file.info(continuous_figure)$size > 100000L &&
  file.info(atlas_figure)$size > 100000L
assert(valid, "Real cortical flatmap validation failed.")

result <- list(
  schema = "neurogeo-flatmap-validation-4.3.1-1",
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(utils::packageVersion("neurogeo")),
  validation = "passed",
  source = list(
    surface = "HCP S1200 Conte69 32k midthickness and flat GIFTI",
    values = basename(dtseries_path),
    atlas = "Schaefer 2018 100-parcel, aggregated to 7 networks",
    bundled_in_package = FALSE
  ),
  left = list(
    vertices = nrow(left_data$vertices),
    faces = nrow(left_data$faces),
    charted_faces = length(left_chart$source_face_in_chart),
    included_faces = sum(left_data$faces$included),
    outline_edges = nrow(left_data$outline),
    atlas_boundary_edges = nrow(left_data$boundaries)
  ),
  right = list(
    vertices = nrow(right_data$vertices),
    faces = nrow(right_data$faces),
    charted_faces = length(right_chart$source_face_in_chart),
    included_faces = sum(right_data$faces$included),
    outline_edges = nrow(right_data$outline),
    atlas_boundary_edges = nrow(right_data$boundaries)
  ),
  features = list(
    topology_verified_flat_surface_binding = TRUE,
    mask_aware_rendering = TRUE,
    curvature_underlay = TRUE,
    continuous_vertex_overlay = TRUE,
    arbitrary_atlas_overlay = TRUE,
    atlas_boundaries = TRUE,
    atlas_labels = TRUE,
    cortical_outline = TRUE
  ),
  figures = list(
    continuous = normalizePath(
      continuous_figure,
      winslash = "/",
      mustWork = TRUE
    ),
    atlas = normalizePath(
      atlas_figure,
      winslash = "/",
      mustWork = TRUE
    )
  ),
  external_neuroimaging_binaries = FALSE,
  elapsed_seconds = elapsed,
  claim_boundary = paste(
    "The flat chart is imported from an existing registered surface and",
    "verified against ordered vertices and faces; neurogeo does not infer",
    "a cut, registration, or resampling."
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
