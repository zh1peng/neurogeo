# Shared simulated neuroimaging data for the tutorial series.
#
# This file is deliberately ordinary R code: readers can source it, inspect
# every assumption, and replace the simulator with their own cohort table.

ngeo_tutorial_dk_case_control <- function(n_per_group = 100L, seed = 20260810L) {
  if (!requireNamespace("ggseg", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) {
    stop("The DK tutorial simulator requires the suggested ggseg and sf packages.")
  }
  n_per_group <- as.integer(n_per_group)
  stopifnot(length(n_per_group) == 1L, n_per_group > 1L)

  atlas <- ggseg::dk()
  atlas_sf <- ggseg.formats::atlas_sf(atlas)
  region_data <- sf::st_drop_geometry(atlas_sf)
  region_data <- region_data[
    !is.na(region_data$label) & !is.na(region_data$region) &
      region_data$region != "corpus callosum",
    ,
    drop = FALSE
  ]
  regions <- unique(region_data[c("label", "hemi", "region", "lobe")])
  regions <- as.data.frame(regions, stringsAsFactors = FALSE)
  regions$region_id <- regions$label
  regions <- regions[c("region_id", "label", "hemi", "region", "lobe")]
  n_region <- nrow(regions)

  # The graph is derived from nearest parcels in each atlas view. It is a
  # reproducible teaching graph, not a replacement for subject-surface
  # geodesic adjacency in a scientific analysis.
  atlas_sf <- atlas_sf[atlas_sf$label %in% regions$label, , drop = FALSE]
  adjacency <- matrix(FALSE, n_region, n_region,
                      dimnames = list(regions$label, regions$label))
  centroid_sum <- matrix(0, n_region, 2L)
  centroid_n <- integer(n_region)
  for (view in unique(atlas_sf$view)) {
    view_sf <- atlas_sf[atlas_sf$view == view, , drop = FALSE]
    centers <- suppressWarnings(
      sf::st_coordinates(sf::st_centroid(view_sf))[, 1:2, drop = FALSE]
    )
    labels <- view_sf$label
    for (i in seq_along(labels)) {
      index <- match(labels[[i]], regions$label)
      centroid_sum[index, ] <- centroid_sum[index, ] + centers[i, ]
      centroid_n[[index]] <- centroid_n[[index]] + 1L
    }
    for (hemi in c("left", "right")) {
      keep <- which(regions$hemi[match(labels, regions$label)] == hemi)
      if (length(keep) < 2L) next
      distance <- as.matrix(stats::dist(centers[keep, , drop = FALSE]))
      diag(distance) <- Inf
      for (i in seq_along(keep)) {
        nearest <- order(distance[i, ])[seq_len(min(3L, length(keep) - 1L))]
        from <- match(labels[keep[[i]]], regions$label)
        to <- match(labels[keep[nearest]], regions$label)
        adjacency[from, to] <- TRUE
      }
    }
  }
  adjacency <- adjacency | t(adjacency)
  diag(adjacency) <- FALSE
  centroids <- centroid_sum / centroid_n
  colnames(centroids) <- c("atlas_x", "atlas_y")

  support_lookup <- ggseg.formats::atlas_vertices(atlas)
  support_size <- vapply(regions$label, function(label) {
    length(support_lookup$vertices[[match(label, support_lookup$label)]])
  }, integer(1))

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
  baseline[is.na(baseline)] <- 2.62
  truth <- rep(0, n_region)
  primary <- regions$region %in% c(
    "superior temporal", "insula", "caudal anterior cingulate",
    "rostral anterior cingulate", "medial orbitofrontal"
  )
  truth[primary] <- -0.12
  neighbor <- as.logical(adjacency %*% primary) & !primary
  truth[neighbor] <- -0.045

  degree <- rowSums(adjacency)
  normalized <- adjacency / sqrt(outer(pmax(degree, 1), pmax(degree, 1)))
  covariance <- solve(diag(n_region) - 0.55 * normalized)
  covariance <- covariance / mean(diag(covariance)) * 0.055^2
  spatial_chol <- chol(covariance)
  values <- matrix(NA_real_, n_region, n_subject,
                   dimnames = list(regions$label, design$subject_id))
  for (i in seq_len(n_subject)) {
    spatial_noise <- drop(stats::rnorm(n_region) %*% spatial_chol)
    values[, i] <- baseline +
      (design$age[[i]] - 40) * -0.003 +
      ifelse(design$sex[[i]] == "male", -0.025, 0) +
      ifelse(design$site[[i]] == "site-b", 0.018, 0) +
      ifelse(design$group[[i]] == "SCZ", truth, 0) +
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
    centroid = centroids,
    support_size = support_size,
    adjacency = adjacency,
    layers = layers,
    measures = neurogeo::ngeo_measure(
      measure_id = "cortical_thickness",
      name = "cortical thickness",
      value_type = "continuous",
      support_behavior = "intensive",
      unit = "mm"
    ),
    coordinate_space = neurogeo::ngeo_coordinate_space(
      "fsaverage-dk-teaching", kind = "surface", unit = "mm"
    )
  )
  group_mean <- sapply(levels(group), function(level) {
    rowMeans(values[, group == level, drop = FALSE])
  })
  difference <- neurogeo::ngeo_parcellation(
    regions,
    values = cbind(SCZ_minus_HC = group_mean[, "SCZ"] - group_mean[, "HC"]),
    centroid = centroids,
    support_size = support_size,
    adjacency = adjacency,
    measures = neurogeo::ngeo_measure(
      measure_id = "case_control_difference",
      name = "SCZ minus HC cortical thickness",
      support_behavior = "intensive",
      unit = "mm"
    ),
    coordinate_space = neurogeo::ngeo_spatial_base(cohort)$coordinate_space
  )

  list(
    cohort = cohort,
    design = design,
    regions = regions,
    adjacency = adjacency,
    centroids = centroids,
    support_size = support_size,
    truth = stats::setNames(truth, regions$label),
    group_mean = group_mean,
    difference = difference,
    seed = seed
  )
}

ngeo_tutorial_plot_dk <- function(x, layer = 1L, title = NULL,
                                  limits = NULL, midpoint = 0) {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("ggseg", quietly = TRUE)) {
    stop("DK brain plots require the suggested ggplot2 and ggseg packages.")
  }
  if (inherits(x, "ngeo")) {
    layer_table <- neurogeo::ngeo_layers(x)
    if (is.character(layer)) {
      selected <- which(layer_table$layer_id == layer | layer_table$name == layer)
    } else {
      selected <- as.integer(layer)
    }
    stopifnot(length(selected) == 1L)
    value <- neurogeo::ngeo_values(x)[, selected]
    elements <- neurogeo::ngeo_base_elements(x)
    data <- data.frame(
      hemi = elements$hemi,
      region = elements$region,
      label = elements$label,
      lobe = elements$lobe,
      value = as.numeric(value)
    )
    if (is.null(title)) title <- layer_table$name[[selected]]
  } else {
    stop("`x` must be a DK `ngeo` object from ngeo_tutorial_dk_case_control().")
  }
  value_range <- range(data$value, finite = TRUE)
  contrast_title <- grepl(
    "minus|contrast|effect|local Moran|减|差异",
    if (is.null(title)) "" else title, ignore.case = TRUE
  )
  diverging <- contrast_title ||
    (value_range[[1L]] < midpoint && value_range[[2L]] > midpoint)
  if (diverging && is.null(limits)) {
    maximum <- max(abs(value_range - midpoint))
    limits <- midpoint + c(-maximum, maximum)
  }
  plot <- ggplot2::ggplot(data, ggplot2::aes(fill = value)) +
    ggseg::geom_brain(atlas = ggseg::dk(), colour = "white", size = 0.08) +
    ggplot2::labs(title = title, fill = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  if (diverging) {
    suppressMessages(plot + ggplot2::scale_fill_gradient2(
      low = "#3B4CC0", mid = "#F7F7F7", high = "#B40426",
      midpoint = midpoint, limits = limits, na.value = "grey90"
    ))
  } else {
    suppressMessages(
      plot + ggplot2::scale_fill_viridis_c(limits = limits, na.value = "grey90")
    )
  }
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
    dk$regions, values = values, centroid = dk$centroids,
    support_size = dk$support_size, adjacency = dk$adjacency,
    layers = layers, measures = measures,
    coordinate_space = neurogeo::ngeo_spatial_base(dk$cohort)$coordinate_space
  )
  myelin_mean <- sapply(levels(design$group), function(level) {
    rowMeans(myelin[, design$group == level, drop = FALSE])
  })
  myelin_difference <- neurogeo::ngeo_parcellation(
    dk$regions,
    values = cbind(SCZ_minus_HC = myelin_mean[, "SCZ"] - myelin_mean[, "HC"]),
    centroid = dk$centroids, support_size = dk$support_size,
    adjacency = dk$adjacency,
    measures = neurogeo::ngeo_measure(
      name = "SCZ minus HC myelin proxy", support_behavior = "intensive",
      unit = "a.u."
    ),
    coordinate_space = neurogeo::ngeo_spatial_base(dk$cohort)$coordinate_space
  )
  list(object = object, myelin_difference = myelin_difference, seed = seed)
}

ngeo_tutorial_vertex_case_control <- function(n_per_group = 5L,
                                               seed = 20260811L) {
  n_per_group <- as.integer(n_per_group)
  stopifnot(length(n_per_group) == 1L, n_per_group > 1L)
  n_ap <- 24L
  n_si <- 14L
  u <- seq(-pi / 2, pi / 2, length.out = n_ap)
  v <- seq(-pi / 2, pi / 2, length.out = n_si)
  grid <- expand.grid(u = u, v = v)
  make_hemi <- function(side) {
    envelope <- sqrt(pmax(cos(grid$u), 0.05))
    anatomical <- cbind(
      x = side * (23 + 11 * cos(grid$v) * cos(grid$u)),
      y = 72 * sin(grid$u),
      z = 49 * sin(grid$v) * envelope
    )
    chart <- cbind(
      x = 76 * sin(grid$u) + ifelse(side < 0, -82, 82),
      y = 49 * sin(grid$v) * envelope
    )
    list(anatomical = anatomical, chart = chart)
  }
  left <- make_hemi(-1)
  right <- make_hemi(1)
  coordinates <- list(
    anatomical = rbind(left$anatomical, right$anatomical),
    flat = rbind(left$chart, right$chart)
  )
  face_one <- do.call(rbind, lapply(seq_len(n_si - 1L), function(j) {
    do.call(rbind, lapply(seq_len(n_ap - 1L), function(i) {
      a <- i + (j - 1L) * n_ap
      matrix(c(a, a + 1L, a + n_ap,
               a + 1L, a + n_ap + 1L, a + n_ap), ncol = 3L, byrow = TRUE)
    }))
  }))
  n_hemi_vertex <- nrow(grid)
  faces <- rbind(face_one, face_one + n_hemi_vertex)
  hemi <- rep(c("left", "right"), each = n_hemi_vertex)
  ap <- rep(grid$u, 2L)
  si <- rep(grid$v, 2L)

  set.seed(seed)
  n_subject <- 2L * n_per_group
  group <- factor(rep(c("HC", "SCZ"), each = n_per_group),
                  levels = c("HC", "SCZ"))
  subject_id <- sprintf("vertex-sub-%02d", seq_len(n_subject))
  effect <- -0.16 * exp(-((ap - 0.42)^2 / 0.12 + (si + 0.08)^2 / 0.30))
  values <- matrix(NA_real_, 2L * n_hemi_vertex, n_subject,
                   dimnames = list(NULL, subject_id))
  for (i in seq_len(n_subject)) {
    smooth_noise <- 0.035 * sin(2 * ap + stats::rnorm(1L)) +
      0.025 * cos(3 * si + stats::rnorm(1L))
    values[, i] <- 2.65 + smooth_noise + stats::rnorm(nrow(values), sd = 0.025) +
      ifelse(group[[i]] == "SCZ", effect, 0)
  }
  layers <- data.frame(
    layer_id = subject_id,
    name = subject_id,
    measure_id = "vertex_thickness",
    subject_id = subject_id,
    group = group,
    stringsAsFactors = FALSE
  )
  surface <- neurogeo::ngeo_surface(
    coordinates,
    faces,
    values = values,
    layers = layers,
    measures = neurogeo::ngeo_measure(
      measure_id = "vertex_thickness", name = "vertex cortical thickness",
      support_behavior = "intensive", unit = "mm"
    ),
    coordinate_space = neurogeo::ngeo_coordinate_space(
      "synthetic-bilateral-surface", kind = "surface", unit = "mm"
    ),
    active_coordinates = "anatomical",
    coordinate_roles = c("anatomical", "chart")
  )
  group_mean <- sapply(levels(group), function(level) {
    rowMeans(values[, group == level, drop = FALSE])
  })
  difference <- neurogeo::ngeo_surface(
    coordinates,
    faces,
    values = cbind(SCZ_minus_HC = group_mean[, "SCZ"] - group_mean[, "HC"]),
    measures = neurogeo::ngeo_measure(
      measure_id = "vertex_case_control_difference",
      name = "vertex SCZ minus HC cortical thickness",
      support_behavior = "intensive", unit = "mm"
    ),
    coordinate_space = neurogeo::ngeo_coordinate_space(
      "synthetic-bilateral-surface", kind = "surface", unit = "mm"
    ),
    active_coordinates = "anatomical",
    coordinate_roles = c("anatomical", "chart")
  )

  ordering <- ave(seq_along(ap), hemi, FUN = function(index) {
    order(order(ap[index] + 0.35 * si[index]))
  })
  support_counts <- c(DK68 = 68L, Schaefer100 = 100L,
                      Schaefer200 = 200L, Schaefer300 = 300L,
                      Glasser360 = 360L)
  supports <- lapply(support_counts, function(total) {
    per_hemi <- total / 2L
    local_rank <- ave(ap + 0.35 * si, hemi, FUN = rank, ties.method = "first")
    local_n <- ave(local_rank, hemi, FUN = length)
    parcel <- pmin(per_hemi, ceiling(local_rank / local_n * per_hemi))
    paste0(ifelse(hemi == "left", "L", "R"), "_", sprintf("%03d", parcel))
  })
  list(
    surface = surface,
    difference = difference,
    group_mean = group_mean,
    group = group,
    subject_id = subject_id,
    truth = effect,
    supports = supports,
    support_note = paste(
      "Atlas-sized synthetic partitions demonstrate operator mechanics only;",
      "they do not reproduce published anatomical boundaries."
    ),
    seed = seed
  )
}

ngeo_tutorial_surface_map <- function(template, values, name = "surface map",
                                      unit = "mm") {
  stopifnot(inherits(template, "ngeo_surface"),
            length(values) == nrow(neurogeo::ngeo_values(template)))
  geometry <- neurogeo::ngeo_spatial_base(template)$geometry
  neurogeo::ngeo_surface(
    geometry$coordinates,
    geometry$faces,
    values = stats::setNames(data.frame(as.numeric(values)), name),
    measures = neurogeo::ngeo_measure(
      name = name, support_behavior = "intensive", unit = unit
    ),
    coordinate_space = neurogeo::ngeo_spatial_base(template)$coordinate_space,
    active_coordinates = geometry$active_coordinates,
    coordinate_roles = geometry$coordinate_meta$role,
    mask = geometry$mask
  )
}
