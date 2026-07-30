.ngeo_cartography_surface <- function(x) {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "Cortical cartography requires an `ngeo_surface`.",
      "ngeo_error_capability"
    )
  }
  ngeo_validate(x, "strict")
  if (!nrow(x$domain$faces)) {
    .ngeo_abort(
      "Cortical cartography requires triangle faces.",
      "ngeo_error_topology"
    )
  }
  invisible(TRUE)
}

.ngeo_cartography_coordinates <- function(x, coordinates, dimension) {
  value <- if (is.character(coordinates) && length(coordinates) == 1L) {
    x$domain$coordinates[[coordinates]]
  } else {
    coordinates
  }
  if (!is.matrix(value) || !is.numeric(value) ||
      nrow(value) != nrow(x$domain$elements) ||
      ncol(value) != dimension || anyNA(value) ||
      any(!is.finite(value))) {
    .ngeo_abort(
      sprintf(
        "`coordinates` must resolve to a finite n_vertex by %d matrix.",
        dimension
      ),
      "ngeo_error_alignment"
    )
  }
  value
}

.ngeo_cartography_edges <- function(faces) {
  directed <- rbind(
    faces[, c(1L, 2L), drop = FALSE],
    faces[, c(2L, 3L), drop = FALSE],
    faces[, c(3L, 1L), drop = FALSE]
  )
  edges <- t(apply(directed, 1L, sort))
  key <- paste(edges[, 1L], edges[, 2L], sep = ":")
  count <- table(key)
  unique_edges <- edges[!duplicated(key), , drop = FALSE]
  data.frame(
    from = unique_edges[, 1L],
    to = unique_edges[, 2L],
    count = as.integer(count[key[!duplicated(key)]]),
    key = key[!duplicated(key)],
    stringsAsFactors = FALSE
  )
}

.ngeo_boundary_invariants <- function(x, boundary) {
  boundary <- .ngeo_as_integer(boundary, "boundary")
  n <- nrow(x$domain$elements)
  if (length(boundary) < 3L || any(boundary < 1L | boundary > n) ||
      anyDuplicated(boundary)) {
    .ngeo_abort(
      "`boundary` must be an ordered cycle of at least three unique vertices.",
      "ngeo_error_topology"
    )
  }
  edges <- .ngeo_cartography_edges(x$domain$faces)
  if (any(edges$count > 2L)) {
    .ngeo_abort(
      "Harmonic parameterization requires a manifold triangle mesh.",
      "ngeo_error_topology"
    )
  }
  boundary_edges <- edges[edges$count == 1L, , drop = FALSE]
  supplied <- cbind(boundary, c(boundary[-1L], boundary[[1L]]))
  supplied <- t(apply(supplied, 1L, sort))
  supplied_key <- paste(supplied[, 1L], supplied[, 2L], sep = ":")
  components <- ngeo_components(x)
  euler <- n - nrow(edges) + nrow(x$domain$faces)
  if (max(components) != 1L || euler != 1L ||
      nrow(boundary_edges) != length(boundary) ||
      !setequal(boundary_edges$key, supplied_key)) {
    .ngeo_abort(
      paste0(
        "Harmonic parameterization requires one connected disk mesh and ",
        "an explicit ordered loop matching its complete boundary."
      ),
      "ngeo_error_topology"
    )
  }
  list(
    boundary = boundary,
    euler_characteristic = euler,
    connected_components = max(components),
    boundary_edges = nrow(boundary_edges),
    manifold = TRUE,
    disk = TRUE
  )
}

.ngeo_triangle_area <- function(coordinates, faces) {
  first <- coordinates[faces[, 1L], , drop = FALSE]
  second <- coordinates[faces[, 2L], , drop = FALSE]
  third <- coordinates[faces[, 3L], , drop = FALSE]
  if (ncol(coordinates) == 2L) {
    return(0.5 * (
      (second[, 1L] - first[, 1L]) *
        (third[, 2L] - first[, 2L]) -
        (second[, 2L] - first[, 2L]) *
          (third[, 1L] - first[, 1L])
    ))
  }
  cross <- cbind(
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
  0.5 * sqrt(rowSums(cross^2))
}

.ngeo_triangle_angles <- function(coordinates, faces) {
  result <- matrix(NA_real_, nrow = nrow(faces), ncol = 3L)
  for (corner in seq_len(3L)) {
    other <- setdiff(seq_len(3L), corner)
    origin <- coordinates[faces[, corner], , drop = FALSE]
    first <- coordinates[faces[, other[[1L]]], , drop = FALSE] - origin
    second <- coordinates[faces[, other[[2L]]], , drop = FALSE] - origin
    cosine <- rowSums(first * second) /
      sqrt(rowSums(first^2) * rowSums(second^2))
    result[, corner] <- acos(pmax(-1, pmin(1, cosine)))
  }
  result
}

.ngeo_cartography_distortion <- function(x, chart, tolerance) {
  source <- x$domain$coordinates[[x$domain$active_coordinates]]
  if (ncol(source) == 2L) source <- cbind(source, 0)
  faces <- x$domain$faces
  source_area <- .ngeo_triangle_area(source, faces)
  if (any(!is.finite(source_area)) || any(source_area <= tolerance)) {
    .ngeo_abort(
      paste0(
        "Cartography distortion requires non-degenerate source triangles; ",
        "the active surface geometry contains a zero-area face."
      ),
      "ngeo_error_geometry"
    )
  }
  signed_chart_area <- .ngeo_triangle_area(chart, faces)
  nonzero <- abs(signed_chart_area) > tolerance
  orientation <- if (any(nonzero)) {
    sign(stats::median(signed_chart_area[nonzero]))
  } else {
    1
  }
  source_angle <- .ngeo_triangle_angles(source, faces)
  chart_angle <- .ngeo_triangle_angles(chart, faces)
  angle_error <- abs(chart_angle - source_angle)
  data.frame(
    source_face = seq_len(nrow(faces)),
    source_area = source_area,
    chart_signed_area = signed_chart_area,
    area_ratio = abs(signed_chart_area) / source_area,
    folded = !nonzero | sign(signed_chart_area) != orientation,
    maximum_angle_error = apply(angle_error, 1L, max),
    mean_angle_error = rowMeans(angle_error),
    stringsAsFactors = FALSE
  )
}

.ngeo_add_cartography_chart <- function(
    x,
    chart,
    name,
    method,
    kind,
    boundary = NULL,
    seam = NULL,
    invariants = list(),
    tolerance = 1e-12) {
  distortion <- .ngeo_cartography_distortion(x, chart, tolerance)
  distortion_summary <- list(
    folded_faces = sum(distortion$folded),
    finite_area_ratio = all(is.finite(distortion$area_ratio)),
    maximum_angle_error = max(distortion$maximum_angle_error)
  )
  result <- ngeo_set_chart(
    x,
    chart,
    name = name,
    distortion = distortion_summary,
    source = paste("neurogeo cortical cartography", method)
  )
  metadata <- result$domain$charts[[name]]
  metadata$method <- method
  metadata$kind <- kind
  metadata$is_metric_flattening <- identical(kind, "parameterization")
  metadata$boundary <- boundary
  metadata$seam <- seam
  metadata$tolerance <- tolerance
  metadata$source_vertex_id <- x$domain$elements$element_id
  metadata$source_face <- seq_len(nrow(x$domain$faces))
  metadata$invariants <- invariants
  metadata$distortion <- distortion
  metadata$distortion_summary <- distortion_summary
  result$domain$charts[[name]] <- metadata
  result$provenance$operations <- c(
    result$provenance$operations,
    list(.ngeo_operation(
      "ngeo_cartography_chart",
      list(
        name = name,
        method = method,
        kind = kind,
        boundary = boundary,
        seam = seam,
        tolerance = tolerance,
        folded_faces = sum(distortion$folded)
      )
    ))
  )
  ngeo_validate(result, "strict")
  result
}

#' Parameterize a cortical surface on a two-dimensional chart
#'
#' Imported charts preserve caller-supplied coordinates. Harmonic charts use
#' a Tutte-style uniform Laplacian and require one connected disk mesh plus an
#' explicit ordered boundary loop. This function never cuts a closed mesh or
#' estimates registration.
#'
#' @param x An `ngeo_surface`.
#' @param method Imported coordinates or harmonic disk parameterization.
#' @param coordinates Required finite vertex-by-two coordinates for imported
#'   charts.
#' @param boundary Required ordered boundary loop for harmonic charts.
#' @param name New chart coordinate-set name.
#' @param tolerance Degenerate-face tolerance.
#' @return `x` with an auditable two-dimensional chart.
#' @examples
#' surface <- ngeo_surface(
#'   matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
#'          ncol = 3, byrow = TRUE),
#'   matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 3))
#' )
#' flat <- ngeo_flatten_surface(
#'   surface, "harmonic", boundary = c(1, 2, 3, 4)
#' )
#' ngeo_chart_distortion(flat, "flat")
#' @export
ngeo_flatten_surface <- function(
    x,
    method = c("imported", "harmonic"),
    coordinates = NULL,
    boundary = NULL,
    name = "flat",
    tolerance = 1e-12) {
  .ngeo_cartography_surface(x)
  method <- match.arg(method)
  .ngeo_assert_scalar_character(name, "name")
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0) {
    .ngeo_abort(
      "`tolerance` must be one positive finite number.",
      "ngeo_error_argument"
    )
  }
  if (identical(method, "imported")) {
    chart <- .ngeo_cartography_coordinates(x, coordinates, 2L)
    invariants <- list(
      imported = TRUE,
      topology_assumed = FALSE
    )
  } else {
    invariants <- .ngeo_boundary_invariants(x, boundary)
    boundary <- invariants$boundary
    source <- x$domain$coordinates[[x$domain$active_coordinates]]
    if (ncol(source) == 2L) source <- cbind(source, 0)
    next_boundary <- c(boundary[-1L], boundary[[1L]])
    segment <- sqrt(rowSums(
      (source[next_boundary, , drop = FALSE] -
        source[boundary, , drop = FALSE])^2
    ))
    if (any(segment <= tolerance)) {
      .ngeo_abort(
        "Boundary edges must have positive source length.",
        "ngeo_error_geometry"
      )
    }
    angle <- 2 * pi * c(0, cumsum(segment)[-length(segment)]) /
      sum(segment)
    chart <- matrix(NA_real_, nrow = nrow(source), ncol = 2L)
    chart[boundary, ] <- cbind(cos(angle), sin(angle))
    interior <- setdiff(seq_len(nrow(source)), boundary)
    if (length(interior)) {
      adjacency <- ngeo_adjacency(x, method = "mesh")
      laplacian <- Matrix::Diagonal(x = Matrix::rowSums(adjacency)) -
        adjacency
      solution <- Matrix::solve(
        laplacian[interior, interior, drop = FALSE],
        -laplacian[interior, boundary, drop = FALSE] %*%
          chart[boundary, , drop = FALSE]
      )
      chart[interior, ] <- as.matrix(solution)
    }
    if (any(!is.finite(chart))) {
      .ngeo_abort(
        "Harmonic parameterization did not produce finite coordinates.",
        "ngeo_error_geometry"
      )
    }
  }
  .ngeo_add_cartography_chart(
    x,
    chart,
    name,
    method,
    kind = "parameterization",
    boundary = boundary,
    invariants = invariants,
    tolerance = tolerance
  )
}

#' Project a cortical surface for planar viewing
#'
#' Viewing projections are explicitly non-metric. Spherical projection
#' requires a caller-supplied seam longitude and does not estimate a spherical
#' registration.
#'
#' @param x An `ngeo_surface`.
#' @param method Orthographic, PCA, or spherical viewing projection.
#' @param coordinates Optional 3D coordinate-set name or matrix.
#' @param view Orthographic axes.
#' @param seam Explicit seam longitude in radians for spherical projection.
#' @param name New chart name.
#' @param tolerance Degenerate-face tolerance.
#' @return `x` with a non-metric viewing chart.
#' @examples
#' surface <- ngeo_surface(
#'   matrix(c(0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1),
#'          ncol = 3, byrow = TRUE),
#'   matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE)
#' )
#' viewed <- ngeo_project_surface(surface, "orthographic", view = "xy")
#' viewed$domain$charts$view$kind
#' @export
ngeo_project_surface <- function(
    x,
    method = c("orthographic", "pca", "spherical"),
    coordinates = NULL,
    view = c("xy", "xz", "yz"),
    seam = NULL,
    name = "view",
    tolerance = 1e-12) {
  .ngeo_cartography_surface(x)
  method <- match.arg(method)
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0) {
    .ngeo_abort(
      "`tolerance` must be one positive finite number.",
      "ngeo_error_argument"
    )
  }
  source <- if (is.null(coordinates)) {
    x$domain$coordinates[[x$domain$active_coordinates]]
  } else {
    .ngeo_cartography_coordinates(x, coordinates, 3L)
  }
  if (ncol(source) == 2L) source <- cbind(source, 0)
  invariants <- list(metric = FALSE, registration_estimated = FALSE)
  chart <- switch(
    method,
    orthographic = {
      view <- match.arg(view)
      axes <- switch(view, xy = c(1L, 2L), xz = c(1L, 3L), yz = c(2L, 3L))
      invariants$view <- view
      source[, axes, drop = FALSE]
    },
    pca = {
      fit <- stats::prcomp(source, center = TRUE, scale. = FALSE)
      loading <- fit$rotation[, 1:2, drop = FALSE]
      for (i in seq_len(2L)) {
        anchor <- which.max(abs(loading[, i]))
        if (loading[anchor, i] < 0) loading[, i] <- -loading[, i]
      }
      invariants$loading <- loading
      invariants$center <- fit$center
      scale(source, center = fit$center, scale = FALSE) %*% loading
    },
    spherical = {
      if (!is.numeric(seam) || length(seam) != 1L ||
          is.na(seam) || !is.finite(seam)) {
        .ngeo_abort(
          "Spherical viewing projection requires explicit finite `seam` radians.",
          "ngeo_error_chart"
        )
      }
      centered <- sweep(source, 2L, colMeans(source), "-")
      invariants$center <- colMeans(source)
      radius <- sqrt(rowSums(centered^2))
      if (any(radius <= tolerance)) {
        .ngeo_abort(
          "Spherical coordinates must have positive radius.",
          "ngeo_error_geometry"
        )
      }
      unit <- centered / radius
      longitude <- ((atan2(unit[, 2L], unit[, 1L]) - seam + pi) %%
        (2 * pi)) - pi
      latitude <- asin(pmax(-1, pmin(1, unit[, 3L])))
      face_longitude <- matrix(longitude[x$domain$faces], ncol = 3L)
      edges <- .ngeo_cartography_edges(x$domain$faces)
      invariants$seam_edges <- edges[
        abs(
          longitude[edges$from] -
            longitude[edges$to]
        ) > pi,
        c("from", "to"),
        drop = FALSE
      ]
      invariants$seam_faces <- which(
        apply(face_longitude, 1L, function(value) diff(range(value))) > pi
      )
      cbind(longitude = longitude, latitude = latitude)
    }
  )
  .ngeo_add_cartography_chart(
    x,
    as.matrix(chart),
    name,
    method,
    kind = "view_projection",
    seam = seam,
    invariants = invariants,
    tolerance = tolerance
  )
}

.ngeo_cortical_vertex_values <- function(x, map, values) {
  if (!is.null(values)) {
    if (!is.atomic(values) || !is.null(dim(values)) ||
        length(values) != nrow(x$domain$elements)) {
      .ngeo_abort(
        "`values` must contain one value per surface vertex.",
        "ngeo_error_alignment"
      )
    }
    return(list(
      values = values,
      name = "vertex_value",
      type = if (is.numeric(values)) "continuous" else "categorical"
    ))
  }
  selected <- .ngeo_plot_map(x, map)
  index <- .ngeo_map_selection(x, map)
  measure <- x$measures[index, , drop = FALSE]
  categorical <- measure$value_type %in% c("label", "categorical") ||
    measure$spatial_semantics == "categorical" ||
    !is.numeric(selected$values)
  list(
    values = selected$values,
    name = selected$name,
    type = if (categorical) "categorical" else "continuous"
  )
}

.ngeo_face_mode <- function(values, faces) {
  apply(faces, 1L, function(index) {
    current <- values[index]
    current <- current[!is.na(current)]
    if (!length(current)) return(NA_character_)
    count <- table(as.character(current))
    names(count)[which.max(count)]
  })
}

.ngeo_cartography_palette <- function(n, palette) {
  tryCatch(
    grDevices::hcl.colors(n, palette),
    error = function(...) {
      .ngeo_abort(
        "`palette` must name an available HCL palette.",
        "ngeo_error_argument"
      )
    }
  )
}

.ngeo_cortical_colors <- function(values, type, palette, limits, na_color) {
  if (identical(type, "continuous")) {
    finite <- is.finite(values)
    limits <- limits %||% if (any(finite)) range(values[finite]) else c(0, 1)
    if (!is.numeric(limits) || length(limits) != 2L ||
        any(!is.finite(limits)) || limits[[1L]] > limits[[2L]]) {
      .ngeo_abort(
        "`limits` must be two ordered finite values.",
        "ngeo_error_argument"
      )
    }
    colors <- rep.int(na_color, length(values))
    scale <- diff(limits)
    index <- if (scale == 0) {
      rep.int(128L, sum(finite))
    } else {
      1L + floor(255 * pmax(0, pmin(
        1,
        (values[finite] - limits[[1L]]) / scale
      )))
    }
    color_table <- .ngeo_cartography_palette(256L, palette)
    colors[finite] <- color_table[index]
    return(list(
      color = colors,
      legend = data.frame(
        value = limits,
        color = color_table[c(1L, 256L)],
        stringsAsFactors = FALSE
      ),
      limits = limits
    ))
  }
  level <- unique(as.character(values[!is.na(values)]))
  color <- .ngeo_cartography_palette(max(1L, length(level)), palette)
  lookup <- stats::setNames(color[seq_along(level)], level)
  result <- rep.int(na_color, length(values))
  keep <- !is.na(values)
  result[keep] <- lookup[as.character(values[keep])]
  list(
    color = unname(result),
    legend = data.frame(
      value = level,
      color = unname(lookup),
      stringsAsFactors = FALSE
    ),
    limits = NULL
  )
}

.ngeo_cortical_atlas <- function(x, atlas, chart) {
  if (is.null(atlas)) return(NULL)
  if (inherits(atlas, "ngeo_partition")) {
    .ngeo_validate_partition(atlas)
    chart_source_hash <- x$domain$charts[[chart]]$source_domain_hash
    compatible_hash <- atlas$base_domain_hash %in% c(
      ngeo_domain_hash(x),
      chart_source_hash
    )
    if (!compatible_hash ||
        length(atlas$membership) != nrow(x$domain$elements)) {
      .ngeo_abort(
        paste0(
          "Partition atlas must match the mapped domain or the exact source ",
          "domain from which the selected chart was added."
        ),
        "ngeo_error_domain_mismatch"
      )
    }
    return(atlas$membership)
  }
  if (!is.atomic(atlas) || !is.null(dim(atlas)) ||
      length(atlas) != nrow(x$domain$elements)) {
    .ngeo_abort(
      "`atlas` must be an aligned vector or `ngeo_partition`.",
      "ngeo_error_alignment"
    )
  }
  as.character(atlas)
}

#' Build an atlas-independent cortical map
#'
#' The map consumes any aligned vertex values and any aligned crisp atlas. It
#' uses the surface's own chart and never substitutes a fixed atlas drawing.
#'
#' @param x An `ngeo_surface` with a chart.
#' @param map One aligned map when `values` is not supplied.
#' @param values Optional arbitrary vertex-aligned values.
#' @param chart Chart name.
#' @param atlas Optional aligned labels or `ngeo_partition`.
#' @param palette HCL palette name.
#' @param limits Optional continuous color limits.
#' @param na_color Missing-value color.
#' @return An `ngeo_cortical_map`.
#' @examples
#' surface <- ngeo_surface(
#'   matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
#'          ncol = 3, byrow = TRUE),
#'   matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 3))
#' )
#' surface <- ngeo_flatten_surface(
#'   surface, "harmonic", boundary = c(1, 2, 3, 4)
#' )
#' map <- ngeo_cortical_map(surface, atlas = c("A", "A", "B", "B"))
#' ngeo_cortical_map_data(map)$faces
#' @export
ngeo_cortical_map <- function(
    x,
    map = 1L,
    values = NULL,
    chart = NULL,
    atlas = NULL,
    palette = "viridis",
    limits = NULL,
    na_color = "grey85") {
  .ngeo_cartography_surface(x)
  .ngeo_assert_scalar_character(palette, "palette")
  .ngeo_assert_scalar_character(na_color, "na_color")
  tryCatch(
    grDevices::col2rgb(na_color),
    error = function(...) {
      .ngeo_abort(
        "`na_color` must be a valid R color.",
        "ngeo_error_argument"
      )
    }
  )
  chart_info <- .ngeo_chart_coordinates(x, chart)
  chart <- chart_info$name
  value <- .ngeo_cortical_vertex_values(x, map, values)
  faces <- x$domain$faces
  face_value <- if (identical(value$type, "continuous")) {
    matrix_value <- matrix(value$values[faces], ncol = 3L)
    matrix_value[!is.finite(matrix_value)] <- NA_real_
    result <- rowMeans(matrix_value, na.rm = TRUE)
    result[rowSums(is.finite(matrix_value)) == 0L] <- NA_real_
    result
  } else {
    .ngeo_face_mode(value$values, faces)
  }
  color <- .ngeo_cortical_colors(
    face_value,
    value$type,
    palette,
    limits,
    na_color
  )
  atlas_value <- .ngeo_cortical_atlas(x, atlas, chart)
  boundary <- data.frame()
  if (!is.null(atlas_value)) {
    edges <- .ngeo_cartography_edges(faces)
    left <- atlas_value[edges$from]
    right <- atlas_value[edges$to]
    keep <- (is.na(left) != is.na(right)) |
      (!is.na(left) & !is.na(right) & left != right)
    boundary <- data.frame(
      from = edges$from[keep],
      to = edges$to[keep],
      left = left[keep],
      right = right[keep],
      stringsAsFactors = FALSE
    )
  }
  coordinates <- chart_info$coordinates
  atlas_column <- atlas_value %||%
    rep.int(NA_character_, nrow(coordinates))
  vertices <- data.frame(
    source_vertex = seq_len(nrow(coordinates)),
    element_id = x$domain$elements$element_id,
    x = coordinates[, 1L],
    y = coordinates[, 2L],
    value = value$values,
    atlas = atlas_column,
    stringsAsFactors = FALSE
  )
  face_data <- data.frame(
    source_face = seq_len(nrow(faces)),
    vertex_1 = faces[, 1L],
    vertex_2 = faces[, 2L],
    vertex_3 = faces[, 3L],
    value = face_value,
    color = color$color,
    stringsAsFactors = FALSE
  )
  result <- list(
    source_domain_hash = ngeo_domain_hash(x),
    chart = chart,
    chart_metadata = x$domain$charts[[chart]],
    coordinates = coordinates,
    faces = faces,
    vertices = vertices,
    face_data = face_data,
    boundaries = boundary,
    legend = color$legend,
    limits = color$limits,
    map_name = value$name,
    value_type = value$type,
    palette = palette,
    na_color = na_color,
    provenance = list(
      source_domain_hash = ngeo_domain_hash(x),
      chart_source_domain_hash =
        x$domain$charts[[chart]]$source_domain_hash,
      seam_faces =
        x$domain$charts[[chart]]$invariants$seam_faces %||% integer(),
      source_vertex = seq_len(nrow(coordinates)),
      source_face = seq_len(nrow(faces))
    )
  )
  class(result) <- "ngeo_cortical_map"
  result
}

#' Return exchangeable cortical plotting data
#'
#' @param x An `ngeo_cortical_map`.
#' @return Vertices, faces, atlas boundaries, legend, and chart metadata.
#' @export
ngeo_cortical_map_data <- function(x) {
  if (!inherits(x, "ngeo_cortical_map")) {
    .ngeo_abort(
      "`x` must be an `ngeo_cortical_map`.",
      "ngeo_error_argument"
    )
  }
  list(
    vertices = x$vertices,
    faces = x$face_data,
    boundaries = x$boundaries,
    legend = x$legend,
    metadata = list(
      source_domain_hash = x$source_domain_hash,
      chart = x$chart,
      chart_metadata = x$chart_metadata,
      map_name = x$map_name,
      value_type = x$value_type,
      limits = x$limits,
      palette = x$palette,
      na_color = x$na_color,
      provenance = x$provenance
    )
  )
}

#' Combine cortical maps into an auditable panel layout
#'
#' @param ... `ngeo_cortical_map` objects or one list of them.
#' @param ncol Number of panel columns.
#' @param labels Optional panel labels.
#' @return An `ngeo_cortical_layout`.
#' @export
ngeo_cortical_layout <- function(..., ncol = NULL, labels = NULL) {
  maps <- list(...)
  if (length(maps) == 1L && is.list(maps[[1L]]) &&
      !inherits(maps[[1L]], "ngeo_cortical_map")) {
    maps <- maps[[1L]]
  }
  if (!length(maps) ||
      any(!vapply(maps, inherits, logical(1), "ngeo_cortical_map"))) {
    .ngeo_abort(
      "A cortical layout requires one or more cortical maps.",
      "ngeo_error_argument"
    )
  }
  ncol <- ncol %||% ceiling(sqrt(length(maps)))
  ncol <- .ngeo_as_integer(ncol, "ncol")
  if (length(ncol) != 1L || ncol < 1L) {
    .ngeo_abort("`ncol` must be positive.", "ngeo_error_argument")
  }
  labels <- labels %||% vapply(maps, `[[`, character(1), "map_name")
  if (!is.character(labels) || length(labels) != length(maps)) {
    .ngeo_abort(
      "`labels` must align with cortical maps.",
      "ngeo_error_alignment"
    )
  }
  structure(
    list(
      maps = maps,
      ncol = ncol,
      nrow = as.integer(ceiling(length(maps) / ncol)),
      labels = labels
    ),
    class = "ngeo_cortical_layout"
  )
}

#' @export
print.ngeo_cortical_map <- function(x, ...) {
  cat(
    "<ngeo_cortical_map>\n",
    "  chart: ", x$chart, "\n",
    "  map: ", x$map_name, " (", x$value_type, ")\n",
    "  vertices: ", nrow(x$vertices), "\n",
    "  faces: ", nrow(x$face_data), "\n",
    "  atlas boundaries: ", nrow(x$boundaries), "\n",
    sep = ""
  )
  invisible(x)
}

#' Plot an atlas-independent cortical map
#'
#' @param x An `ngeo_cortical_map`.
#' @param show_mesh Draw every triangle edge.
#' @param show_boundaries Draw aligned atlas boundary edges.
#' @param boundary_color Atlas boundary color.
#' @param boundary_lwd Atlas boundary line width.
#' @param main Plot title.
#' @param axes Draw coordinate axes.
#' @param show_legend Draw the value legend.
#' @param legend_position Position passed to [graphics::legend()].
#' @param legend_cex Legend text scaling.
#' @param xlim,ylim Plot limits. Spherical views default to the complete
#'   longitude/latitude range.
#' @param ... Additional arguments passed to [graphics::plot()].
#' @return `x`, invisibly.
#' @export
plot.ngeo_cortical_map <- function(
    x,
    show_mesh = FALSE,
    show_boundaries = TRUE,
    boundary_color = "grey10",
    boundary_lwd = 0.7,
    main = x$map_name,
    axes = FALSE,
    show_legend = TRUE,
    legend_position = "topright",
    legend_cex = 0.75,
    xlim = NULL,
    ylim = NULL,
    ...) {
  coordinates <- x$coordinates
  faces <- x$faces
  spherical <- identical(x$chart_metadata$method, "spherical")
  if (is.null(xlim)) {
    xlim <- if (spherical) c(-pi, pi) else range(coordinates[, 1L])
  }
  if (is.null(ylim)) {
    ylim <- if (spherical) c(-pi / 2, pi / 2) else
      range(coordinates[, 2L])
  }
  graphics::plot(
    coordinates,
    type = "n",
    asp = 1,
    xlab = "",
    ylab = "",
    axes = axes,
    main = main,
    xlim = xlim,
    ylim = ylim,
    ...
  )
  seam_faces <- x$chart_metadata$invariants$seam_faces %||% integer()
  regular_faces <- if (length(seam_faces)) {
    setdiff(seq_len(nrow(faces)), seam_faces)
  } else {
    seq_len(nrow(faces))
  }
  draw_faces <- function(index, shift = NULL) {
    if (!length(index)) return(invisible(NULL))
    current <- faces[index, , drop = FALSE]
    face_x <- matrix(coordinates[current, 1L], ncol = 3L)
    if (identical(shift, "positive")) {
      face_x[face_x < 0] <- face_x[face_x < 0] + 2 * pi
    } else if (identical(shift, "negative")) {
      face_x[face_x > 0] <- face_x[face_x > 0] - 2 * pi
    }
    face_y <- matrix(coordinates[current, 2L], ncol = 3L)
    graphics::polygon(
      as.vector(t(cbind(face_x, NA_real_))),
      as.vector(t(cbind(face_y, NA_real_))),
      col = x$face_data$color[index],
      border = if (isTRUE(show_mesh)) "grey70" else NA
    )
    invisible(NULL)
  }
  draw_faces(regular_faces)
  if (length(seam_faces)) {
    draw_faces(seam_faces, "positive")
    draw_faces(seam_faces, "negative")
  }
  if (isTRUE(show_boundaries) && nrow(x$boundaries)) {
    boundary <- x$boundaries
    from_x <- coordinates[boundary$from, 1L]
    to_x <- coordinates[boundary$to, 1L]
    crossing <- spherical & abs(from_x - to_x) > pi
    draw_segments <- function(index, shift = NULL) {
      if (!length(index)) return(invisible(NULL))
      x0 <- from_x[index]
      x1 <- to_x[index]
      if (identical(shift, "positive")) {
        x0[x0 < 0] <- x0[x0 < 0] + 2 * pi
        x1[x1 < 0] <- x1[x1 < 0] + 2 * pi
      } else if (identical(shift, "negative")) {
        x0[x0 > 0] <- x0[x0 > 0] - 2 * pi
        x1[x1 > 0] <- x1[x1 > 0] - 2 * pi
      }
      graphics::segments(
        x0,
        coordinates[boundary$from[index], 2L],
        x1,
        coordinates[boundary$to[index], 2L],
        col = boundary_color,
        lwd = boundary_lwd
      )
      invisible(NULL)
    }
    draw_segments(which(!crossing))
    draw_segments(which(crossing), "positive")
    draw_segments(which(crossing), "negative")
  }
  if (isTRUE(show_legend) && nrow(x$legend)) {
    legend_label <- if (identical(x$value_type, "continuous")) {
      format(signif(x$legend$value, 5L), trim = TRUE)
    } else {
      as.character(x$legend$value)
    }
    legend_color <- x$legend$color
    if (any(x$face_data$color == x$na_color)) {
      legend_label <- c(legend_label, "NA")
      legend_color <- c(legend_color, x$na_color)
    }
    graphics::legend(
      legend_position,
      legend = legend_label,
      fill = legend_color,
      title = x$map_name,
      bty = "n",
      cex = legend_cex
    )
  }
  invisible(x)
}

#' @export
print.ngeo_cortical_layout <- function(x, ...) {
  cat(
    "<ngeo_cortical_layout>\n",
    "  panels: ", length(x$maps), "\n",
    "  arrangement: ", x$nrow, " x ", x$ncol, "\n",
    sep = ""
  )
  invisible(x)
}

#' Plot a cortical multi-panel layout
#'
#' @param x An `ngeo_cortical_layout`.
#' @param ... Arguments passed to each cortical-map plot method.
#' @return `x`, invisibly.
#' @export
plot.ngeo_cortical_layout <- function(x, ...) {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mfrow = c(x$nrow, x$ncol))
  for (i in seq_along(x$maps)) {
    plot(x$maps[[i]], main = x$labels[[i]], ...)
  }
  invisible(x)
}
