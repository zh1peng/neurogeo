# Shared surface-native neuroimaging data for the tutorial series.
#
# `auto` mode uses checksum-pinned Conte69/HCP geometry and published atlas
# memberships whenever the download-only tutorial fixtures are available.
# Package checks can use the explicitly marked synthetic fallback; the website
# sets `NEUROGEO_TUTORIAL_DATA_MODE=real`, so published pages never fall back.

.ngeo_tutorial_cache_version <- "conte69-foundation-v1"
.ngeo_tutorial_cache_name <- ".neurogeo_tutorial_process_cache"
if (!exists(
  .ngeo_tutorial_cache_name, envir = .GlobalEnv, inherits = FALSE
) || !is.environment(get(
  .ngeo_tutorial_cache_name, envir = .GlobalEnv, inherits = FALSE
))) {
  assign(
    .ngeo_tutorial_cache_name,
    new.env(parent = emptyenv()),
    envir = .GlobalEnv
  )
}
# The renderer sources this file into a fresh page environment. Keeping only
# tutorial fixtures and deterministic simulations in this explicitly named
# process cache avoids rebuilding the same 64k surface for every page.
.ngeo_tutorial_cache <- get(
  .ngeo_tutorial_cache_name, envir = .GlobalEnv, inherits = FALSE
)

.ngeo_tutorial_manifest_path <- function(version) {
  relative <- file.path("inst", "extdata", version, "manifest.csv")
  installed <- system.file(
    "extdata", version, "manifest.csv", package = "neurogeo"
  )
  candidate <- c(relative, installed)
  candidate <- candidate[nzchar(candidate) & file.exists(candidate)]
  if (!length(candidate)) return(NA_character_)
  normalizePath(candidate[[1L]], winslash = "/", mustWork = TRUE)
}

.ngeo_tutorial_cache_path <- function(kind) {
  if (identical(kind, "flatmap")) {
    environment <- Sys.getenv("NEUROGEO_TUTORIAL_FLATMAP_CACHE", "")
    option <- getOption("neurogeo.tutorial.flatmap_cache")
    fallback <- file.path(".tools", "reference-flatmap")
  } else {
    environment <- Sys.getenv("NEUROGEO_TUTORIAL_REFERENCE50_CACHE", "")
    option <- getOption("neurogeo.tutorial.reference50_cache")
    fallback <- file.path(".tools", "reference-5.0")
  }
  path <- if (nzchar(environment)) {
    environment
  } else if (!is.null(option)) {
    option
  } else {
    fallback
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.ngeo_tutorial_fixture_paths <- function(verify = TRUE) {
  manifest_431 <- .ngeo_tutorial_manifest_path("reference-4.3.1")
  manifest_50 <- .ngeo_tutorial_manifest_path("reference-5.0")
  if (anyNA(c(manifest_431, manifest_50))) return(NULL)
  cache_key <- paste(
    .ngeo_tutorial_cache_version,
    "fixtures",
    verify,
    .ngeo_tutorial_cache_path("flatmap"),
    .ngeo_tutorial_cache_path("reference50"),
    sep = ":"
  )
  if (exists(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE))
  }
  flat_manifest <- utils::read.csv(
    manifest_431, stringsAsFactors = FALSE, check.names = FALSE
  )
  atlas_manifest <- utils::read.csv(
    manifest_50, stringsAsFactors = FALSE, check.names = FALSE
  )
  flat_names <- c(
    "hcp-s1200-left-flat-32k", "hcp-s1200-right-flat-32k",
    "hcp-s1200-left-midthickness-32k",
    "hcp-s1200-right-midthickness-32k",
    "hcp-s1200-left-atlasroi-32k", "hcp-s1200-right-atlasroi-32k"
  )
  atlas_names <- c(
    "enigma-aparc-conte69", "enigma-schaefer100-conte69",
    "enigma-schaefer200-conte69", "enigma-schaefer300-conte69",
    "enigma-glasser360-conte69"
  )
  flat <- flat_manifest[match(flat_names, flat_manifest$name), , drop = FALSE]
  atlas <- atlas_manifest[match(atlas_names, atlas_manifest$name), , drop = FALSE]
  if (anyNA(c(flat$name, atlas$name))) return(NULL)
  flat$path <- file.path(.ngeo_tutorial_cache_path("flatmap"), flat$file)
  atlas$path <- file.path(.ngeo_tutorial_cache_path("reference50"), atlas$file)
  inventory <- rbind(
    flat[, c("name", "path", "size", "sha256")],
    atlas[, c("name", "path", "size", "sha256")]
  )
  if (any(!file.exists(inventory$path))) return(NULL)
  if (isTRUE(verify)) {
    size_ok <- as.numeric(file.info(inventory$path)$size) ==
      as.numeric(inventory$size)
    hash_ok <- vapply(seq_len(nrow(inventory)), function(i) {
      identical(
        digest::digest(
          inventory$path[[i]], algo = "sha256", file = TRUE,
          serialize = FALSE
        ),
        inventory$sha256[[i]]
      )
    }, logical(1))
    if (!all(size_ok & hash_ok)) return(NULL)
  }
  result <- stats::setNames(inventory$path, inventory$name)
  assign(cache_key, result, envir = .ngeo_tutorial_cache)
  result
}

.ngeo_tutorial_data_mode <- function(mode = "auto") {
  mode <- match.arg(mode, c("auto", "real", "synthetic"))
  requested <- Sys.getenv("NEUROGEO_TUTORIAL_DATA_MODE", "")
  if (identical(mode, "auto") && nzchar(requested)) {
    mode <- match.arg(requested, c("real", "synthetic"))
  }
  if (identical(mode, "synthetic")) return(mode)
  available <- requireNamespace("gifti", quietly = TRUE) &&
    !is.null(.ngeo_tutorial_fixture_paths())
  if (identical(mode, "real") && !available) {
    stop(
      paste(
        "Real tutorial mode requires verified HCP/Conte69 and ENIGMA",
        "fixtures. Run tools/fetch-reference-431.R and",
        "tools/fetch-reference-50.R before rendering."
      ),
      call. = FALSE
    )
  }
  if (identical(mode, "auto")) {
    if (available) "real" else "synthetic"
  } else {
    mode
  }
}

.ngeo_tutorial_normalize_rows <- function(x) {
  x / sqrt(rowSums(x^2))
}

.ngeo_tutorial_icosphere <- function(level = 4L) {
  level <- as.integer(level)
  phi <- (1 + sqrt(5)) / 2
  vertices <- rbind(
    c(-1, phi, 0), c(1, phi, 0), c(-1, -phi, 0), c(1, -phi, 0),
    c(0, -1, phi), c(0, 1, phi), c(0, -1, -phi), c(0, 1, -phi),
    c(phi, 0, -1), c(phi, 0, 1), c(-phi, 0, -1), c(-phi, 0, 1)
  )
  vertices <- .ngeo_tutorial_normalize_rows(vertices)
  faces <- matrix(c(
    1, 12, 6, 1, 6, 2, 1, 2, 8, 1, 8, 11, 1, 11, 12,
    2, 6, 10, 6, 12, 5, 12, 11, 3, 11, 8, 7, 8, 2, 9,
    4, 10, 5, 4, 5, 3, 4, 3, 7, 4, 7, 9, 4, 9, 10,
    5, 10, 6, 3, 5, 12, 7, 3, 11, 9, 7, 8, 10, 9, 2
  ), ncol = 3L, byrow = TRUE)

  for (iteration in seq_len(level)) {
    n_face <- nrow(faces)
    edge_from <- c(faces[, 1L], faces[, 2L], faces[, 3L])
    edge_to <- c(faces[, 2L], faces[, 3L], faces[, 1L])
    edge_low <- pmin.int(edge_from, edge_to)
    edge_high <- pmax.int(edge_from, edge_to)
    edge_key <- paste(edge_low, edge_high, sep = ":")
    keep <- !duplicated(edge_key)
    unique_low <- edge_low[keep]
    unique_high <- edge_high[keep]
    midpoint <- .ngeo_tutorial_normalize_rows(
      vertices[unique_low, , drop = FALSE] +
        vertices[unique_high, , drop = FALSE]
    )
    midpoint_index <- nrow(vertices) + match(edge_key, edge_key[keep])
    vertices <- rbind(vertices, midpoint)
    edge_12 <- midpoint_index[seq_len(n_face)]
    edge_23 <- midpoint_index[n_face + seq_len(n_face)]
    edge_31 <- midpoint_index[2L * n_face + seq_len(n_face)]
    faces <- rbind(
      cbind(faces[, 1L], edge_12, edge_31),
      cbind(faces[, 2L], edge_23, edge_12),
      cbind(faces[, 3L], edge_31, edge_23),
      cbind(edge_12, edge_23, edge_31)
    )
  }
  list(vertices = vertices, faces = faces)
}

.ngeo_tutorial_dk_regions <- function() {
  region <- c(
    "bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", "cuneus",
    "entorhinal", "fusiform", "inferiorparietal", "inferiortemporal",
    "isthmuscingulate", "lateraloccipital", "lateralorbitofrontal", "lingual",
    "medialorbitofrontal", "middletemporal", "parahippocampal", "paracentral",
    "parsopercularis", "parsorbitalis", "parstriangularis", "pericalcarine",
    "postcentral", "posteriorcingulate", "precentral", "precuneus",
    "rostralanteriorcingulate", "rostralmiddlefrontal", "superiorfrontal",
    "superiorparietal", "superiortemporal", "supramarginal", "frontalpole",
    "temporalpole", "transversetemporal", "insula"
  )
  lobe <- rep("frontal", length(region))
  lobe[region %in% c(
    "bankssts", "entorhinal", "fusiform", "inferiortemporal",
    "middletemporal", "parahippocampal", "superiortemporal",
    "temporalpole", "transversetemporal"
  )] <- "temporal"
  lobe[region %in% c(
    "inferiorparietal", "postcentral", "precuneus", "superiorparietal",
    "supramarginal"
  )] <- "parietal"
  lobe[region %in% c(
    "cuneus", "lateraloccipital", "lingual", "pericalcarine"
  )] <- "occipital"
  lobe[region %in% c(
    "caudalanteriorcingulate", "isthmuscingulate", "posteriorcingulate",
    "rostralanteriorcingulate"
  )] <- "cingulate"
  lobe[region == "insula"] <- "insula"
  data.frame(region = region, lobe = lobe, stringsAsFactors = FALSE)
}

.ngeo_tutorial_voronoi <- function(coordinates, hemi, n_total, name) {
  membership <- rep.int(NA_character_, nrow(coordinates))
  for (current_hemi in c("left", "right")) {
    candidate <- which(hemi == current_hemi)
    local <- coordinates[candidate, , drop = FALSE]
    local <- scale(local, center = TRUE, scale = apply(local, 2L, stats::sd))
    n_seed <- as.integer(n_total / 2L)
    closest <- rep.int(Inf, length(candidate))
    assignment <- integer(length(candidate))
    center <- which.min(rowSums(local^2))
    for (seed in seq_len(n_seed)) {
      selected <- if (seed == 1L) center else which.max(closest)
      distance <- rowSums((local - matrix(
        local[selected, ], nrow(local), 2L, byrow = TRUE
      ))^2)
      update <- distance < closest
      closest[update] <- distance[update]
      assignment[update] <- seed
    }
    prefix <- if (current_hemi == "left") "lh" else "rh"
    membership[candidate] <- sprintf(
      "%s_%s_%03d", prefix, name, assignment
    )
  }
  membership
}

.ngeo_tutorial_parcel_adjacency <- function(membership, faces, levels) {
  edges <- rbind(
    faces[, c(1L, 2L), drop = FALSE],
    faces[, c(2L, 3L), drop = FALSE],
    faces[, c(3L, 1L), drop = FALSE]
  )
  first <- membership[edges[, 1L]]
  second <- membership[edges[, 2L]]
  keep <- !is.na(first) & !is.na(second) & first != second
  first <- match(first[keep], levels)
  second <- match(second[keep], levels)
  adjacency <- matrix(FALSE, length(levels), length(levels),
                      dimnames = list(levels, levels))
  adjacency[cbind(first, second)] <- TRUE
  adjacency <- adjacency | t(adjacency)
  diag(adjacency) <- FALSE
  adjacency
}

.ngeo_tutorial_synthetic_core <- function() {
  cache_key <- paste(
    .ngeo_tutorial_cache_version, "surface_core_synthetic", sep = ":"
  )
  if (exists(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE))
  }

  sphere <- .ngeo_tutorial_icosphere(4L)
  unit <- sphere$vertices
  n_hemi <- nrow(unit)
  theta <- atan2(unit[, 3L], unit[, 2L])
  radial <- acos(pmax(-1, pmin(1, unit[, 1L]))) / pi
  direction_norm <- sqrt(unit[, 2L]^2 + unit[, 3L]^2)
  direction_norm[direction_norm < 1e-12] <- 1
  direction_y <- unit[, 2L] / direction_norm
  direction_z <- unit[, 3L] / direction_norm
  outline <- 1 + 0.07 * cos(theta) - 0.045 * cos(2 * theta) +
    0.025 * sin(5 * theta)
  local_flat <- cbind(
    78 * radial * direction_y * outline,
    55 * radial * direction_z * outline + 4 * (1 - radial)
  )

  make_hemi <- function(side) {
    anatomical <- cbind(
      x = side * (29 + 24 * unit[, 1L]),
      y = 74 * unit[, 2L],
      z = 54 * unit[, 3L]
    )
    flat <- local_flat
    if (side > 0) flat[, 1L] <- -flat[, 1L]
    flat[, 1L] <- flat[, 1L] + side * 88
    list(anatomical = anatomical, flat = flat)
  }
  left <- make_hemi(-1)
  right <- make_hemi(1)
  anatomical <- rbind(left$anatomical, right$anatomical)
  flat <- rbind(left$flat, right$flat)
  faces <- rbind(sphere$faces, sphere$faces + n_hemi)
  hemi <- rep(c("left", "right"), each = n_hemi)
  mask <- rep(radial < 0.86, 2L)
  sulc_one <- scale(
    sin(8 * theta + 2.5 * unit[, 1L]) +
      0.55 * cos(13 * theta - 4 * unit[, 1L]) +
      0.25 * sin(5 * unit[, 2L] * unit[, 3L] * pi)
  )[, 1L]
  sulc <- rep(sulc_one, 2L)

  support_counts <- c(
    DK68 = 68L,
    Schaefer100 = 100L,
    Schaefer200 = 200L,
    Schaefer300 = 300L,
    Glasser360 = 360L
  )
  supports <- lapply(names(support_counts), function(atlas) {
    .ngeo_tutorial_voronoi(flat, hemi, support_counts[[atlas]], atlas)
  })
  names(supports) <- names(support_counts)

  dk <- .ngeo_tutorial_dk_regions()
  dk_lookup <- c(
    stats::setNames(paste0("lh_", dk$region), sprintf("lh_DK68_%03d", 1:34)),
    stats::setNames(paste0("rh_", dk$region), sprintf("rh_DK68_%03d", 1:34))
  )
  supports$DK68 <- unname(dk_lookup[supports$DK68])
  label_resources <- lapply(supports, function(values) {
    list(values = values, table = NULL)
  })

  space <- neurogeo::ngeo_coordinate_space(
    "synthetic-ico4-atlas-sized-fallback",
    kind = "surface",
    unit = "mm",
    source_metadata = list(
      published_atlas_boundaries = FALSE,
      disclosure = paste(
        "Deterministic atlas-sized teaching partitions; not published",
        "DK, Schaefer, or Glasser boundaries."
      )
    )
  )
  template <- neurogeo::ngeo_surface(
    list(anatomical = anatomical),
    faces,
    labels = label_resources,
    coordinate_space = space,
    active_coordinates = "anatomical",
    coordinate_roles = "anatomical",
    mask = mask
  )
  template <- neurogeo::ngeo_flatten_surface(
    template, method = "imported", coordinates = flat, name = "flat"
  )
  vertex_area <- neurogeo::ngeo_vertex_area(template)

  regions <- do.call(rbind, lapply(c("left", "right"), function(side) {
    prefix <- if (side == "left") "lh" else "rh"
    data.frame(
      region_id = paste0(prefix, "_", dk$region),
      label = paste0(prefix, "_", dk$region),
      hemi = side,
      region = dk$region,
      lobe = dk$lobe,
      stringsAsFactors = FALSE
    )
  }))
  dk_membership <- supports$DK68
  centroids <- t(vapply(regions$region_id, function(id) {
    colMeans(flat[dk_membership == id, , drop = FALSE])
  }, numeric(2)))
  colnames(centroids) <- c("flat_x", "flat_y")
  support_size <- vapply(regions$region_id, function(id) {
    sum(vertex_area[dk_membership == id])
  }, numeric(1))
  adjacency <- .ngeo_tutorial_parcel_adjacency(
    dk_membership, faces, regions$region_id
  )

  result <- list(
    template = template,
    anatomical = anatomical,
    flat = flat,
    faces = faces,
    hemi = hemi,
    mask = mask,
    sulc = sulc,
    supports = supports,
    support_counts = support_counts,
    regions = regions,
    centroids = centroids,
    support_size = support_size,
    adjacency = adjacency,
    vertex_area = vertex_area,
    n_vertex_per_hemi = n_hemi,
    density = "synthetic ico4-like (2,562 vertices per hemisphere)",
    data_source = "synthetic_fallback",
    published_atlases = FALSE,
    support_note = paste(
      "Synthetic fallback: parcel counts match the named resolutions, but",
      "the deterministic contiguous boundaries are not published DK,",
      "Schaefer, or Glasser atlas boundaries."
    )
  )
  assign(cache_key, result, envir = .ngeo_tutorial_cache)
  result
}

.ngeo_tutorial_vertex_curvature <- function(surface) {
  geometry <- neurogeo::ngeo_spatial_base(surface)$geometry
  coordinates <- geometry$coordinates[[geometry$active_coordinates]]
  faces <- geometry$faces
  first <- coordinates[faces[, 1L], , drop = FALSE]
  second <- coordinates[faces[, 2L], , drop = FALSE]
  third <- coordinates[faces[, 3L], , drop = FALSE]
  face_normal <- cbind(
    (second[, 2L] - first[, 2L]) * (third[, 3L] - first[, 3L]) -
      (second[, 3L] - first[, 3L]) * (third[, 2L] - first[, 2L]),
    (second[, 3L] - first[, 3L]) * (third[, 1L] - first[, 1L]) -
      (second[, 1L] - first[, 1L]) * (third[, 3L] - first[, 3L]),
    (second[, 1L] - first[, 1L]) * (third[, 2L] - first[, 2L]) -
      (second[, 2L] - first[, 2L]) * (third[, 1L] - first[, 1L])
  )
  normal <- matrix(0, nrow = nrow(coordinates), ncol = 3L)
  for (corner in seq_len(3L)) {
    contribution <- rowsum(face_normal, faces[, corner], reorder = FALSE)
    index <- as.integer(rownames(contribution))
    normal[index, ] <- normal[index, , drop = FALSE] + contribution
  }
  magnitude <- sqrt(rowSums(normal^2))
  normal[magnitude > 0, ] <- normal[magnitude > 0, , drop = FALSE] /
    magnitude[magnitude > 0]
  adjacency <- neurogeo::ngeo_adjacency(surface, include_masked = TRUE)
  degree <- Matrix::rowSums(adjacency)
  neighbour_mean <- Matrix::Diagonal(x = 1 / pmax(degree, 1)) %*%
    adjacency %*% coordinates
  curvature <- rowSums((as.matrix(neighbour_mean) - coordinates) * normal)
  as.numeric(scale(curvature))
}

.ngeo_tutorial_read_labels <- function(path) {
  as.integer(scan(path, what = numeric(), sep = ",", quiet = TRUE))
}

.ngeo_tutorial_real_core <- function() {
  cache_key <- paste(
    .ngeo_tutorial_cache_version, "surface_core_real", sep = ":"
  )
  if (exists(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE))
  }
  fixture <- .ngeo_tutorial_fixture_paths()
  if (is.null(fixture)) {
    stop("Verified real cortical tutorial fixtures are unavailable.", call. = FALSE)
  }
  left <- neurogeo::read_ngeo_gifti(
    geometry = fixture[["hcp-s1200-left-midthickness-32k"]],
    data = c(atlasroi = fixture[["hcp-s1200-left-atlasroi-32k"]]),
    checksum = TRUE
  )
  right <- neurogeo::read_ngeo_gifti(
    geometry = fixture[["hcp-s1200-right-midthickness-32k"]],
    data = c(atlasroi = fixture[["hcp-s1200-right-atlasroi-32k"]]),
    checksum = TRUE
  )
  left_flat <- neurogeo::read_ngeo_gifti(
    fixture[["hcp-s1200-left-flat-32k"]], checksum = TRUE
  )
  right_flat <- neurogeo::read_ngeo_gifti(
    fixture[["hcp-s1200-right-flat-32k"]], checksum = TRUE
  )
  coordinates <- function(x) {
    geometry <- neurogeo::ngeo_spatial_base(x)$geometry
    geometry$coordinates[[geometry$active_coordinates]]
  }
  left_anatomical <- coordinates(left)
  right_anatomical <- coordinates(right)
  left_chart <- coordinates(left_flat)
  right_chart <- coordinates(right_flat)
  n_left <- nrow(left_anatomical)
  n_right <- nrow(right_anatomical)
  if (!identical(c(n_left, n_right), c(32492L, 32492L))) {
    stop("Conte69 tutorial surfaces must contain 32,492 vertices per hemisphere.")
  }
  anatomical <- rbind(left_anatomical, right_anatomical)
  source_faces <- rbind(
    neurogeo::ngeo_spatial_base(left)$geometry$faces,
    neurogeo::ngeo_spatial_base(right)$geometry$faces + n_left
  )
  if (!identical(nrow(source_faces), 129960L)) {
    stop("The bilateral Conte69 tutorial surface must contain 129,960 faces.")
  }
  chart_width <- max(
    diff(range(left_chart[, 1L])), diff(range(right_chart[, 1L]))
  )
  chart_gap <- 20
  left_chart[, 1L] <- left_chart[, 1L] - mean(range(left_chart[, 1L])) -
    (chart_width + chart_gap) / 2
  right_chart[, 1L] <- right_chart[, 1L] - mean(range(right_chart[, 1L])) +
    (chart_width + chart_gap) / 2
  left_chart[, 2L] <- left_chart[, 2L] - mean(range(left_chart[, 2L]))
  right_chart[, 2L] <- right_chart[, 2L] - mean(range(right_chart[, 2L]))
  flat <- rbind(left_chart, right_chart)
  flat_faces <- rbind(
    neurogeo::ngeo_spatial_base(left_flat)$geometry$faces,
    neurogeo::ngeo_spatial_base(right_flat)$geometry$faces + n_left
  )
  mask <- c(
    neurogeo::ngeo_values(left)[, "atlasroi"] > 0,
    neurogeo::ngeo_values(right)[, "atlasroi"] > 0
  )
  hemi <- rep(c("left", "right"), c(n_left, n_right))

  dk_numeric <- .ngeo_tutorial_read_labels(
    fixture[["enigma-aparc-conte69"]]
  )
  schaefer100 <- .ngeo_tutorial_read_labels(
    fixture[["enigma-schaefer100-conte69"]]
  )
  schaefer200 <- .ngeo_tutorial_read_labels(
    fixture[["enigma-schaefer200-conte69"]]
  )
  schaefer300 <- .ngeo_tutorial_read_labels(
    fixture[["enigma-schaefer300-conte69"]]
  )
  glasser360 <- .ngeo_tutorial_read_labels(
    fixture[["enigma-glasser360-conte69"]]
  )
  label_length <- vapply(
    list(dk_numeric, schaefer100, schaefer200, schaefer300, glasser360),
    length,
    integer(1)
  )
  if (any(label_length != nrow(anatomical))) {
    stop("A Conte69 atlas label vector is not vertex aligned.", call. = FALSE)
  }
  dk <- .ngeo_tutorial_dk_regions()
  canonical_regions <- do.call(rbind, lapply(c("left", "right"), function(side) {
    prefix <- if (side == "left") "lh" else "rh"
    data.frame(
      region_id = paste0(prefix, "_", dk$region),
      label = paste0(prefix, "_", dk$region),
      hemi = side,
      region = dk$region,
      lobe = dk$lobe,
      stringsAsFactors = FALSE
    )
  }))
  dk_codes <- c(1:34, 36:69)
  dk_labels <- canonical_regions$region_id[match(dk_numeric, dk_codes)]
  atlas_labels <- function(values, name) {
    ifelse(values == 0L, NA_character_, sprintf("%s_%03d", name, values))
  }
  # The pinned ENIGMA Glasser vector assigns code 180 to 63 right-hemisphere
  # vertices even though code 180 is the final left-hemisphere parcel. Those
  # scattered right-side entries are excluded before labeling, leaving the
  # declared 1:180 left and 181:360 right atlas with exactly 360 parcels.
  right_code_180 <- seq.int(n_left + 1L, n_left + n_right)[
    glasser360[(n_left + 1L):(n_left + n_right)] == 180L
  ]
  if (!length(right_code_180)) {
    stop("The expected Glasser right-hemisphere code-180 audit cell is absent.")
  }
  glasser360[right_code_180] <- 0L
  supports <- list(
    DK68 = dk_labels,
    Schaefer100 = atlas_labels(schaefer100, "Schaefer100"),
    Schaefer200 = atlas_labels(schaefer200, "Schaefer200"),
    Schaefer300 = atlas_labels(schaefer300, "Schaefer300"),
    Glasser360 = atlas_labels(glasser360, "Glasser360")
  )
  support_counts <- vapply(
    supports,
    function(value) length(unique(value[!is.na(value)])),
    integer(1)
  )
  expected_counts <- c(
    DK68 = 68L, Schaefer100 = 100L, Schaefer200 = 200L,
    Schaefer300 = 300L, Glasser360 = 360L
  )
  if (!identical(support_counts, expected_counts)) {
    stop("The real tutorial atlas family has unexpected parcel counts.")
  }
  labels <- lapply(supports, function(value) {
    list(values = value, table = NULL)
  })
  space <- neurogeo::ngeo_coordinate_space(
    space_id = "HCP-S1200-Conte69-32k-bilateral-tutorial",
    kind = "surface",
    unit = "mm",
    template = "Conte69",
    density = "32k",
    source_metadata = list(
      geometry = "HCP S1200 midthickness and registered flat surfaces",
      atlases = "ENIGMA Toolbox vertex-aligned atlas tables",
      checksum_pinned = TRUE,
      published_atlas_boundaries = TRUE
    )
  )
  template <- neurogeo::ngeo_surface(
    list(anatomical = anatomical),
    source_faces,
    labels = labels,
    coordinate_space = space,
    active_coordinates = "anatomical",
    coordinate_roles = "anatomical",
    mask = mask,
    index_base = "one",
    source_index_base = 0L
  )
  flat_template <- neurogeo::ngeo_surface(
    list(flat = flat),
    flat_faces,
    coordinate_space = space,
    active_coordinates = "flat",
    coordinate_roles = "chart",
    index_base = "one",
    source_index_base = 0L
  )
  template <- neurogeo::ngeo_flatten_surface(
    template,
    method = "imported",
    coordinates = flat_template,
    name = "flat"
  )
  vertex_area <- neurogeo::ngeo_vertex_area(template)
  partition <- neurogeo::ngeo_partition(template, "DK68")
  region_ids <- partition$parcellation$region_id
  regions <- canonical_regions[
    match(region_ids, canonical_regions$region_id), , drop = FALSE
  ]
  membership <- partition$membership
  centroids <- t(vapply(region_ids, function(id) {
    keep <- !is.na(membership) & membership == id & mask
    vapply(seq_len(ncol(anatomical)), function(column) {
      stats::weighted.mean(anatomical[keep, column], vertex_area[keep])
    }, numeric(1))
  }, numeric(3)))
  colnames(centroids) <- c("x", "y", "z")
  support_size <- vapply(region_ids, function(id) {
    keep <- !is.na(membership) & membership == id & mask
    sum(vertex_area[keep])
  }, numeric(1))
  adjacency <- as.matrix(neurogeo::ngeo_region_adjacency(template, partition))
  dimnames(adjacency) <- list(region_ids, region_ids)
  underlay <- .ngeo_tutorial_vertex_curvature(template)
  result <- list(
    template = template,
    flat_template = flat_template,
    anatomical = anatomical,
    flat = flat,
    faces = source_faces,
    flat_faces = flat_faces,
    hemi = hemi,
    mask = mask,
    sulc = underlay,
    supports = supports,
    support_counts = support_counts,
    regions = regions,
    centroids = centroids,
    support_size = support_size,
    adjacency = adjacency,
    vertex_area = vertex_area,
    n_vertex_per_hemi = 32492L,
    density = "Conte69 32k (32,492 vertices per hemisphere)",
    data_source = "real_conte69",
    published_atlases = TRUE,
    support_note = paste(
      "Published DK, Schaefer, and Glasser atlas memberships aligned to the",
      "checksum-pinned bilateral Conte69 32k surface."
    ),
    fixture_paths = fixture,
    atlas_corrections = list(
      Glasser360_right_code_180_excluded = length(right_code_180)
    )
  )
  assign(cache_key, result, envir = .ngeo_tutorial_cache)
  result
}

.ngeo_tutorial_surface_core <- function(mode = "auto") {
  mode <- .ngeo_tutorial_data_mode(mode)
  if (identical(mode, "real")) {
    .ngeo_tutorial_real_core()
  } else {
    .ngeo_tutorial_synthetic_core()
  }
}

.ngeo_tutorial_new_surface <- function(values, layers = NULL, measures = NULL,
                                       mode = "auto") {
  core <- .ngeo_tutorial_surface_core(mode)
  surface <- neurogeo::ngeo_surface(
    list(anatomical = core$anatomical),
    core$faces,
    values = values,
    layers = layers,
    measures = measures,
    labels = lapply(core$supports, function(x) list(values = x, table = NULL)),
    coordinate_space = neurogeo::ngeo_spatial_base(core$template)$coordinate_space,
    active_coordinates = "anatomical",
    coordinate_roles = "anatomical",
    mask = core$mask,
    source_index_base = if (identical(core$data_source, "real_conte69")) 0L else 1L
  )
  neurogeo::ngeo_flatten_surface(
    surface,
    method = "imported",
    coordinates = if (!is.null(core$flat_template)) core$flat_template else core$flat,
    name = "flat"
  )
}

ngeo_tutorial_dk_case_control <- function(n_per_group = 100L,
                                           seed = 20260810L,
                                           mode = "auto") {
  n_per_group <- as.integer(n_per_group)
  seed <- as.integer(seed)
  stopifnot(
    length(n_per_group) == 1L, n_per_group > 1L,
    length(seed) == 1L, !is.na(seed)
  )
  mode <- .ngeo_tutorial_data_mode(mode)
  cache_key <- paste(
    .ngeo_tutorial_cache_version,
    "dk_case_control",
    mode,
    n_per_group,
    seed,
    sep = ":"
  )
  if (exists(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE))
  }
  core <- .ngeo_tutorial_surface_core(mode)
  regions <- core$regions
  n_region <- nrow(regions)

  set.seed(seed)
  n_subject <- 2L * n_per_group
  group <- factor(rep(c("HC", "SCZ"), each = n_per_group),
                  levels = c("HC", "SCZ"))
  design <- data.frame(
    subject_id = sprintf("sub-%03d", seq_len(n_subject)),
    group = group,
    age = round(pmin(65, pmax(18, stats::rnorm(
      n_subject, mean = ifelse(group == "SCZ", 38, 36), sd = 10
    ))), 1),
    sex = factor(sample(c("female", "male"), n_subject, replace = TRUE)),
    site = factor(rep(c("site-a", "site-b"), length.out = n_subject)),
    stringsAsFactors = FALSE
  )

  lobe_mean <- c(
    frontal = 2.66, temporal = 2.72, parietal = 2.60,
    occipital = 2.54, cingulate = 2.69, insula = 2.75
  )
  baseline <- unname(lobe_mean[regions$lobe])
  truth <- rep(0, n_region)
  primary <- regions$region %in% c(
    "superiortemporal", "insula", "caudalanteriorcingulate",
    "rostralanteriorcingulate", "medialorbitofrontal"
  )
  truth[primary] <- -0.12
  neighbor <- as.logical(core$adjacency %*% primary) & !primary
  truth[neighbor] <- -0.045
  anterior_posterior_gradient <-
    0.03 * as.numeric(scale(core$centroids[, 2L]))
  truth <- truth + anterior_posterior_gradient
  truth_components <- data.frame(
    region_id = regions$region_id,
    primary = primary,
    neighbor = neighbor,
    anterior_posterior_gradient = anterior_posterior_gradient,
    stringsAsFactors = FALSE
  )

  degree <- rowSums(core$adjacency)
  normalized <- core$adjacency /
    sqrt(outer(pmax(degree, 1), pmax(degree, 1)))
  covariance <- solve(diag(n_region) - 0.55 * normalized)
  covariance <- covariance / mean(diag(covariance)) * 0.055^2
  spatial_chol <- chol(covariance)
  values <- matrix(NA_real_, n_region, n_subject,
                   dimnames = list(regions$region_id, design$subject_id))
  for (i in seq_len(n_subject)) {
    spatial_noise <- drop(stats::rnorm(n_region) %*% spatial_chol)
    values[, i] <- baseline +
      (design$age[[i]] - 40) * -0.003 +
      ifelse(design$sex[[i]] == "male", -0.025, 0) +
      ifelse(design$site[[i]] == "site-b", 0.018, 0) +
      as.numeric(design$group[[i]] == "SCZ") * truth +
      stats::rnorm(1L, sd = 0.045) + spatial_noise
  }
  layers <- data.frame(
    layer_id = design$subject_id,
    name = design$subject_id,
    measure_id = "cortical_thickness",
    subject_id = design$subject_id,
    group = design$group,
    age = design$age,
    sex = design$sex,
    site = design$site,
    stringsAsFactors = FALSE
  )
  cohort <- neurogeo::ngeo_parcellation(
    regions,
    values = values,
    centroid = core$centroids,
    support_size = core$support_size,
    adjacency = core$adjacency,
    layers = layers,
    measures = neurogeo::ngeo_measure(
      measure_id = "cortical_thickness",
      name = "cortical thickness",
      value_type = "continuous",
      support_behavior = "intensive",
      unit = "mm"
    ),
    coordinate_space = neurogeo::ngeo_spatial_base(core$template)$coordinate_space
  )
  group_mean <- sapply(levels(group), function(level) {
    rowMeans(values[, group == level, drop = FALSE])
  })
  difference <- neurogeo::ngeo_parcellation(
    regions,
    values = cbind(SCZ_minus_HC = group_mean[, "SCZ"] - group_mean[, "HC"]),
    centroid = core$centroids,
    support_size = core$support_size,
    adjacency = core$adjacency,
    measures = neurogeo::ngeo_measure(
      measure_id = "case_control_difference",
      name = "SCZ minus HC cortical thickness",
      support_behavior = "intensive",
      unit = "mm"
    ),
    coordinate_space = neurogeo::ngeo_spatial_base(cohort)$coordinate_space
  )
  result <- list(
    cohort = cohort,
    design = design,
    regions = regions,
    adjacency = core$adjacency,
    centroids = core$centroids,
    support_size = core$support_size,
    truth = stats::setNames(truth, regions$region_id),
    truth_components = truth_components,
    group_mean = group_mean,
    difference = difference,
    surface = core$template,
    atlas = core$supports$DK68,
    supports = core$supports,
    underlay = core$sulc,
    mask = core$mask,
    data_source = core$data_source,
    published_atlases = core$published_atlases,
    support_note = core$support_note,
    seed = seed
  )
  assign(cache_key, result, envir = .ngeo_tutorial_cache)
  result
}

ngeo_tutorial_vertex_case_control <- function(n_per_group = 5L,
                                               seed = 20260811L,
                                               mode = "auto") {
  n_per_group <- as.integer(n_per_group)
  seed <- as.integer(seed)
  stopifnot(
    length(n_per_group) == 1L, n_per_group > 1L,
    length(seed) == 1L, !is.na(seed)
  )
  mode <- .ngeo_tutorial_data_mode(mode)
  cache_key <- paste(
    .ngeo_tutorial_cache_version,
    "vertex_case_control",
    mode,
    n_per_group,
    seed,
    sep = ":"
  )
  if (exists(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .ngeo_tutorial_cache, inherits = FALSE))
  }
  core <- .ngeo_tutorial_surface_core(mode)
  dk <- ngeo_tutorial_dk_case_control(
    n_per_group = 100L, seed = 20260810L, mode = mode
  )
  n_subject <- 2L * n_per_group
  group <- factor(rep(c("HC", "SCZ"), each = n_per_group),
                  levels = c("HC", "SCZ"))
  subject_id <- sprintf("vertex-sub-%02d", seq_len(n_subject))
  dk_index <- match(core$supports$DK68, dk$regions$region_id)
  baseline <- c(
    frontal = 2.66, temporal = 2.72, parietal = 2.60,
    occipital = 2.54, cingulate = 2.69, insula = 2.75
  )[dk$regions$lobe]
  vertex_baseline <- baseline[dk_index]
  vertex_baseline[is.na(vertex_baseline)] <- 2.62
  vertex_truth <- dk$truth[dk_index]
  vertex_truth[is.na(vertex_truth)] <- 0
  vertex_primary <- dk$truth_components$primary[dk_index]
  vertex_primary[is.na(vertex_primary)] <- FALSE
  vertex_neighbor <- dk$truth_components$neighbor[dk_index]
  vertex_neighbor[is.na(vertex_neighbor)] <- FALSE
  unit <- core$anatomical
  unit[, 1L] <- abs(unit[, 1L])
  unit <- scale(unit, center = TRUE, scale = apply(unit, 2L, stats::sd))

  set.seed(seed)
  values <- matrix(NA_real_, nrow(core$anatomical), n_subject,
                   dimnames = list(NULL, subject_id))
  for (i in seq_len(n_subject)) {
    coefficient <- stats::rnorm(6L, sd = c(0.035, 0.03, 0.03, 0.02, 0.02, 0.02))
    smooth_noise <-
      coefficient[[1L]] * sin(unit[, 2L]) +
      coefficient[[2L]] * cos(unit[, 3L]) +
      coefficient[[3L]] * sin(2 * unit[, 2L] + unit[, 3L]) +
      coefficient[[4L]] * cos(3 * unit[, 3L]) +
      coefficient[[5L]] * sin(unit[, 1L] - 2 * unit[, 2L]) +
      coefficient[[6L]] * core$sulc
    values[, i] <- vertex_baseline + smooth_noise +
      stats::rnorm(nrow(values), sd = 0.025) +
      as.numeric(group[[i]] == "SCZ") * vertex_truth
  }
  layers <- data.frame(
    layer_id = subject_id,
    name = subject_id,
    measure_id = "vertex_thickness",
    subject_id = subject_id,
    group = group,
    stringsAsFactors = FALSE
  )
  surface <- .ngeo_tutorial_new_surface(
    values,
    layers = layers,
    measures = neurogeo::ngeo_measure(
      measure_id = "vertex_thickness",
      name = "vertex cortical thickness",
      support_behavior = "intensive",
      unit = "mm"
    ),
    mode = mode
  )
  group_mean <- sapply(levels(group), function(level) {
    rowMeans(values[, group == level, drop = FALSE])
  })
  difference <- .ngeo_tutorial_new_surface(
    cbind(SCZ_minus_HC = group_mean[, "SCZ"] - group_mean[, "HC"]),
    measures = neurogeo::ngeo_measure(
      measure_id = "vertex_case_control_difference",
      name = "vertex SCZ minus HC cortical thickness",
      support_behavior = "intensive",
      unit = "mm"
    ),
    mode = mode
  )
  result <- list(
    surface = surface,
    difference = difference,
    group_mean = group_mean,
    group = group,
    subject_id = subject_id,
    truth = vertex_truth,
    primary = vertex_primary,
    neighbor = vertex_neighbor,
    supports = core$supports,
    vertex_area = core$vertex_area,
    underlay = core$sulc,
    mask = core$mask,
    density = core$density,
    n_vertex_per_hemi = core$n_vertex_per_hemi,
    data_source = core$data_source,
    published_atlases = core$published_atlases,
    support_note = core$support_note,
    atlas_corrections = core$atlas_corrections,
    seed = seed
  )
  assign(cache_key, result, envir = .ngeo_tutorial_cache)
  result
}

ngeo_tutorial_surface_map <- function(template, values, name = "surface map",
                                      unit = "mm", mode = "auto") {
  stopifnot(
    inherits(template, "ngeo_surface"),
    length(values) == nrow(neurogeo::ngeo_values(template))
  )
  .ngeo_tutorial_new_surface(
    stats::setNames(data.frame(as.numeric(values)), name),
    measures = neurogeo::ngeo_measure(
      name = name, support_behavior = "intensive", unit = unit
    ),
    mode = mode
  )
}

ngeo_tutorial_support_maps <- function(x) {
  core <- .ngeo_tutorial_surface_core()
  surface <- if (is.list(x) && !inherits(x, "ngeo")) x$surface else x
  stopifnot(inherits(surface, "ngeo_surface"))
  lapply(core$supports, function(labels) {
    neurogeo::ngeo_atlas_map(
      surface,
      labels,
      source_support = core$vertex_area
    )
  })
}

ngeo_tutorial_dk_multilayer <- function(dk = ngeo_tutorial_dk_case_control(),
                                        seed = 20260812L) {
  thickness <- neurogeo::ngeo_values(dk$cohort)
  design <- dk$design
  n_subject <- ncol(thickness)
  n_region <- nrow(thickness)
  set.seed(seed)
  standardized <- scale(thickness, center = TRUE, scale = TRUE)
  lobe_offset <- stats::setNames(
    c(0.05, 0.12, -0.04, -0.10, 0.02, 0.08),
    c("frontal", "temporal", "parietal", "occipital", "cingulate", "insula")
  )
  myelin <- matrix(NA_real_, n_region, n_subject)
  for (i in seq_len(n_subject)) {
    coupling <- if (design$group[[i]] == "SCZ") 0.28 else 0.48
    myelin[, i] <- 1.05 + unname(lobe_offset[dk$regions$lobe]) +
      coupling * standardized[, i] * 0.10 +
      stats::rnorm(n_region, sd = 0.045)
  }
  values <- matrix(NA_real_, n_region, 2L * n_subject)
  values[, seq(1L, 2L * n_subject, by = 2L)] <- thickness
  values[, seq(2L, 2L * n_subject, by = 2L)] <- myelin
  subject <- rep(design$subject_id, each = 2L)
  feature <- rep(c("thickness", "myelin"), n_subject)
  layers <- data.frame(
    layer_id = paste(subject, feature, sep = "_"),
    name = paste(subject, feature, sep = "_"),
    measure_id = rep(c("cortical_thickness", "myelin_proxy"), n_subject),
    subject_id = subject,
    feature = feature,
    group = rep(design$group, each = 2L),
    stringsAsFactors = FALSE
  )
  measures <- rbind(
    neurogeo::ngeo_measure(
      measure_id = "cortical_thickness", name = "cortical thickness",
      support_behavior = "intensive", unit = "mm"
    ),
    neurogeo::ngeo_measure(
      measure_id = "myelin_proxy", name = "simulated myelin proxy",
      support_behavior = "intensive", unit = "a.u."
    )
  )
  object <- neurogeo::ngeo_parcellation(
    dk$regions,
    values = values,
    centroid = dk$centroids,
    support_size = dk$support_size,
    adjacency = dk$adjacency,
    layers = layers,
    measures = measures,
    coordinate_space = neurogeo::ngeo_spatial_base(dk$cohort)$coordinate_space
  )
  myelin_mean <- sapply(levels(design$group), function(level) {
    rowMeans(myelin[, design$group == level, drop = FALSE])
  })
  myelin_difference <- neurogeo::ngeo_parcellation(
    dk$regions,
    values = cbind(SCZ_minus_HC = myelin_mean[, "SCZ"] - myelin_mean[, "HC"]),
    centroid = dk$centroids,
    support_size = dk$support_size,
    adjacency = dk$adjacency,
    measures = neurogeo::ngeo_measure(
      name = "SCZ minus HC myelin proxy",
      support_behavior = "intensive",
      unit = "a.u."
    ),
    coordinate_space = neurogeo::ngeo_spatial_base(dk$cohort)$coordinate_space
  )
  list(object = object, myelin_difference = myelin_difference, seed = seed)
}
