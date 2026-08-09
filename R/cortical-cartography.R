.ngeo_cartography_surface <- function(x) {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "Cortical cartography requires an `ngeo_surface`.",
      "ngeo_error_capability"
    )
  }
  ngeo_validate(x, "strict")
  if (!nrow(x$base$geometry$faces)) {
    .ngeo_abort(
      "Cortical cartography requires triangle faces.",
      "ngeo_error_topology"
    )
  }
  invisible(TRUE)
}

.ngeo_cartography_coordinates <- function(x, coordinates, dimension) {
  value <- if (is.character(coordinates) && length(coordinates) == 1L) {
    x$base$geometry$coordinates[[coordinates]]
  } else {
    coordinates
  }
  if (!is.matrix(value) || !is.numeric(value) ||
      nrow(value) != nrow(x$base$elements) ||
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

.ngeo_cartography_imported_coordinates <- function(x, coordinates) {
  if (!inherits(coordinates, "ngeo_surface")) {
    return(list(
      coordinates = .ngeo_cartography_coordinates(x, coordinates, 2L),
      invariants = list(
        imported = TRUE,
        topology_assumed = FALSE,
        topology_verified = FALSE,
        source_kind = "matrix"
      )
    ))
  }

  .ngeo_cartography_surface(coordinates)
  if (nrow(coordinates$base$elements) != nrow(x$base$elements) ||
      !identical(
        coordinates$base$elements$element_id,
        x$base$elements$element_id
      ) ||
      !identical(
        coordinates$base$elements$source_index,
        x$base$elements$source_index
      )) {
    .ngeo_abort(
      paste0(
        "An imported flat surface must have the same ordered source vertices ",
        "as `x`."
      ),
      "ngeo_error_alignment"
    )
  }
  source_face <- x$base$geometry$faces
  imported_face <- coordinates$base$geometry$faces
  face_key <- function(face) {
    first <- pmin.int(face[, 1L], face[, 2L], face[, 3L])
    third <- pmax.int(face[, 1L], face[, 2L], face[, 3L])
    second <- rowSums(face) - first - third
    paste(first, second, third, sep = ":")
  }
  source_key <- face_key(source_face)
  imported_key <- face_key(imported_face)
  source_face_in_chart <- match(imported_key, source_key)
  if (anyNA(source_face_in_chart) || anyDuplicated(imported_key)) {
    .ngeo_abort(
      paste0(
        "Every imported flat-surface face must map uniquely to a source ",
        "surface face."
      ),
      "ngeo_error_alignment"
    )
  }
  topology_relation <- if (
    length(source_face_in_chart) == nrow(source_face) &&
      identical(source_face_in_chart, seq_len(nrow(source_face)))
  ) {
    "identical"
  } else {
    "face_subset"
  }

  available <- coordinates$base$geometry$coordinate_meta
  candidate <- available$name[
    available$role == "chart" & available$dimension == 2L
  ]
  name <- if (length(candidate) == 1L) {
    candidate[[1L]]
  } else {
    active <- coordinates$base$geometry$active_coordinates
    if (ncol(coordinates$base$geometry$coordinates[[active]]) == 2L) {
      active
    } else {
      .ngeo_abort(
        paste0(
          "The imported surface must have exactly one two-dimensional chart ",
          "or a two-dimensional active coordinate set."
        ),
        "ngeo_error_chart"
      )
    }
  }

  list(
    coordinates = coordinates$base$geometry$coordinates[[name]],
    invariants = list(
      imported = TRUE,
      topology_assumed = FALSE,
      topology_verified = TRUE,
      topology_relation = topology_relation,
      source_face_in_chart = source_face_in_chart,
      source_kind = "ngeo_surface",
      source_chart = name,
      imported_surface_base_hash = base_hash(coordinates)
    )
  )
}

.ngeo_cartography_edges <- function(faces) {
  directed <- rbind(
    faces[, c(1L, 2L), drop = FALSE],
    faces[, c(2L, 3L), drop = FALSE],
    faces[, c(3L, 1L), drop = FALSE]
  )
  from <- pmin.int(directed[, 1L], directed[, 2L])
  to <- pmax.int(directed[, 1L], directed[, 2L])
  key <- paste(from, to, sep = ":")
  keep <- !duplicated(key)
  unique_key <- key[keep]
  group <- match(key, unique_key)
  data.frame(
    from = from[keep],
    to = to[keep],
    count = tabulate(group, nbins = length(unique_key)),
    key = unique_key,
    stringsAsFactors = FALSE
  )
}

.ngeo_boundary_invariants <- function(x, boundary) {
  boundary <- .ngeo_as_integer(boundary, "boundary")
  n <- nrow(x$base$elements)
  if (length(boundary) < 3L || any(boundary < 1L | boundary > n) ||
      anyDuplicated(boundary)) {
    .ngeo_abort(
      "`boundary` must be an ordered cycle of at least three unique vertices.",
      "ngeo_error_topology"
    )
  }
  edges <- .ngeo_cartography_edges(x$base$geometry$faces)
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
  euler <- n - nrow(edges) + nrow(x$base$geometry$faces)
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
  source <- x$base$geometry$coordinates[[x$base$geometry$active_coordinates]]
  if (ncol(source) == 2L) source <- cbind(source, 0)
  faces <- x$base$geometry$faces
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
  source_face_in_chart <- invariants$source_face_in_chart %||%
    seq_len(nrow(distortion))
  distortion$charted <- seq_len(nrow(distortion)) %in%
    source_face_in_chart
  charted <- distortion$charted
  finite_angle <- distortion$maximum_angle_error[
    charted & is.finite(distortion$maximum_angle_error)
  ]
  distortion_summary <- list(
    charted_faces = sum(charted),
    folded_faces = sum(distortion$folded[charted]),
    finite_area_ratio = all(is.finite(distortion$area_ratio[charted])),
    maximum_angle_error = if (length(finite_angle)) {
      max(finite_angle)
    } else {
      NA_real_
    }
  )
  result <- ngeo_set_chart(
    x,
    chart,
    name = name,
    distortion = distortion_summary,
    source = paste("neurogeo cortical cartography", method)
  )
  metadata <- result$base$charts[[name]]
  metadata$method <- method
  metadata$kind <- kind
  metadata$is_metric_flattening <- identical(kind, "parameterization")
  metadata$boundary <- boundary
  metadata$seam <- seam
  metadata$tolerance <- tolerance
  metadata$source_vertex_id <- x$base$elements$element_id
  metadata$source_face <- seq_len(nrow(x$base$geometry$faces))
  metadata$invariants <- invariants
  metadata$distortion <- distortion
  metadata$distortion_summary <- distortion_summary
  result$base$charts[[name]] <- metadata
  result$history$operations <- c(
    result$history$operations,
    list(.ngeo_operation(
      "ngeo_cartography_chart",
      list(
        name = name,
        method = method,
        kind = kind,
        boundary = boundary,
        seam = seam,
        tolerance = tolerance,
        charted_faces = sum(charted),
        folded_faces = sum(distortion$folded[charted])
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
#' @param coordinates For imported charts, either a finite vertex-by-two
#'   coordinate matrix or an aligned `ngeo_surface` carrying a flat chart.
#'   A surface input verifies ordered source vertices and layers every imported
#'   face to the source topology; a registered flat surface may contain a
#'   verified subset of source faces around its cut or medial wall.
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
    imported <- .ngeo_cartography_imported_coordinates(x, coordinates)
    chart <- imported$coordinates
    invariants <- imported$invariants
  } else {
    invariants <- .ngeo_boundary_invariants(x, boundary)
    boundary <- invariants$boundary
    source <- x$base$geometry$coordinates[[x$base$geometry$active_coordinates]]
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
#' Viewing projections are explicitly non-distance_method. Spherical projection
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
#' @return `x` with a non-distance_method viewing chart.
#' @examples
#' surface <- ngeo_surface(
#'   matrix(c(0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1),
#'          ncol = 3, byrow = TRUE),
#'   matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE)
#' )
#' viewed <- ngeo_project_surface(surface, "orthographic", view = "xy")
#' viewed$base$charts$view$kind
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
    x$base$geometry$coordinates[[x$base$geometry$active_coordinates]]
  } else {
    .ngeo_cartography_coordinates(x, coordinates, 3L)
  }
  if (ncol(source) == 2L) source <- cbind(source, 0)
  invariants <- list(distance_method = FALSE, registration_estimated = FALSE)
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
      face_longitude <- matrix(longitude[x$base$geometry$faces], ncol = 3L)
      edges <- .ngeo_cartography_edges(x$base$geometry$faces)
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

.ngeo_cortical_vertex_values <- function(x, layer, values) {
  if (!is.null(values)) {
    if (!is.atomic(values) || !is.null(dim(values)) ||
        length(values) != nrow(x$base$elements)) {
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
  selected <- .ngeo_plot_map(x, layer)
  index <- .ngeo_layer_selection(x, layer)
  measure <- .ngeo_measures_for_layers(x, index)
  categorical <- measure$value_type %in% c("label", "categorical") ||
    measure$support_behavior == "categorical" ||
    !is.numeric(selected$values)
  list(
    values = selected$values,
    name = selected$name,
    type = if (categorical) "categorical" else "continuous"
  )
}

.ngeo_face_mode <- function(values, faces) {
  first <- as.character(values[faces[, 1L]])
  second <- as.character(values[faces[, 2L]])
  third <- as.character(values[faces[, 3L]])
  result <- rep.int(NA_character_, nrow(faces))

  pair <- !is.na(first) & !is.na(second) & first == second
  result[pair] <- first[pair]
  unresolved <- is.na(result)
  pair <- unresolved & !is.na(first) & !is.na(third) & first == third
  result[pair] <- first[pair]
  unresolved <- is.na(result)
  pair <- unresolved & !is.na(second) & !is.na(third) & second == third
  result[pair] <- second[pair]

  unresolved <- is.na(result)
  if (any(unresolved)) {
    sentinel <- "\U0010FFFF"
    lexical <- pmin(
      ifelse(is.na(first[unresolved]), sentinel, first[unresolved]),
      ifelse(is.na(second[unresolved]), sentinel, second[unresolved]),
      ifelse(is.na(third[unresolved]), sentinel, third[unresolved])
    )
    lexical[lexical == sentinel] <- NA_character_
    result[unresolved] <- lexical
  }
  result
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

.ngeo_cortical_colors <- function(
    values,
    type,
    palette,
    limits,
    na_color,
    colors = NULL) {
  if (identical(type, "continuous")) {
    if (!is.null(colors)) {
      .ngeo_abort(
        "`colors` can only be used for categorical cortical layers.",
        "ngeo_error_argument"
      )
    }
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
  if (is.null(colors)) {
    color <- .ngeo_cartography_palette(max(1L, length(level)), palette)
    lookup <- stats::setNames(color[seq_along(level)], level)
  } else {
    if (!is.character(colors) || !length(colors) || anyNA(colors)) {
      .ngeo_abort(
        "`colors` must be a non-missing character palette.",
        "ngeo_error_argument"
      )
    }
    tryCatch(
      grDevices::col2rgb(colors),
      error = function(...) {
        .ngeo_abort(
          "`colors` contains an invalid R color.",
          "ngeo_error_argument"
        )
      }
    )
    if (is.null(names(colors)) || any(!nzchar(names(colors)))) {
      if (length(colors) < length(level)) {
        .ngeo_abort(
          "`colors` must provide one color per categorical level.",
          "ngeo_error_alignment"
        )
      }
      lookup <- stats::setNames(colors[seq_along(level)], level)
    } else {
      missing <- setdiff(level, names(colors))
      if (length(missing)) {
        .ngeo_abort(
          paste0(
            "`colors` is missing categorical levels: ",
            paste(missing, collapse = ", "),
            "."
          ),
          "ngeo_error_alignment"
        )
      }
      lookup <- colors[level]
    }
  }
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

.ngeo_cortical_label_table <- function(table, membership) {
  if (is.null(table) || !is.data.frame(table) || !nrow(table)) {
    return(list(
      membership = as.character(membership),
      colors = NULL
    ))
  }
  lower <- tolower(names(table))
  key_index <- match("key", lower)
  label_index <- match("label", lower)
  if (is.na(key_index) || is.na(label_index)) {
    return(list(
      membership = as.character(membership),
      colors = NULL
    ))
  }

  key <- as.character(table[[key_index]])
  label <- as.character(table[[label_index]])
  lookup <- stats::setNames(label, key)
  translated <- unname(lookup[as.character(membership)])
  missing <- is.na(translated) & !is.na(membership)
  translated[missing] <- as.character(membership[missing])

  channel <- match(c("red", "green", "blue", "alpha"), lower)
  colors <- NULL
  if (!anyNA(channel)) {
    rgba <- suppressWarnings(
      apply(table[, channel, drop = FALSE], 2L, as.numeric)
    )
    if (is.matrix(rgba) && nrow(rgba) == nrow(table) &&
        all(is.finite(rgba))) {
      maximum <- if (max(rgba) > 1) 255 else 1
      colors <- grDevices::rgb(
        rgba[, 1L],
        rgba[, 2L],
        rgba[, 3L],
        rgba[, 4L],
        maxColorValue = maximum
      )
      colors <- stats::setNames(colors, label)
    }
  }
  list(membership = translated, colors = colors)
}

.ngeo_cortical_atlas <- function(x, atlas, chart) {
  if (is.null(atlas)) return(NULL)
  if (is.character(atlas) && length(atlas) == 1L &&
      atlas %in% names(x$base$labels)) {
    source <- x$base$labels[[atlas]]
    if (!is.list(source) || is.null(source$values) ||
        length(source$values) != nrow(x$base$elements)) {
      .ngeo_abort(
        sprintf("Label source `%s` is not vertex aligned.", atlas),
        "ngeo_error_alignment"
      )
    }
    decoded <- .ngeo_cortical_label_table(source$table, source$values)
    return(list(
      membership = decoded$membership,
      colors = decoded$colors,
      table = source$table,
      name = atlas,
      source = "labels"
    ))
  }
  if (inherits(atlas, "ngeo_partition")) {
    .ngeo_validate_partition(atlas)
    chart_source_hash <- x$base$charts[[chart]]$source_base_hash
    compatible_hash <- atlas$source_base_hash %in% c(
      base_hash(x),
      chart_source_hash
    )
    if (!compatible_hash ||
        length(atlas$membership) != nrow(x$base$elements)) {
      .ngeo_abort(
        paste0(
          "Partition atlas must match the mapped base or the exact source ",
          "base from which the selected chart was added."
        ),
        "ngeo_error_base_mismatch"
      )
    }
    return(list(
      membership = atlas$membership,
      colors = NULL,
      table = NULL,
      name = "atlas",
      source = "ngeo_partition"
    ))
  }
  if (!is.atomic(atlas) || !is.null(dim(atlas)) ||
      length(atlas) != nrow(x$base$elements)) {
    .ngeo_abort(
      "`atlas` must be an aligned vector or `ngeo_partition`.",
      "ngeo_error_alignment"
    )
  }
  list(
    membership = as.character(atlas),
    colors = NULL,
    table = NULL,
    name = "atlas",
    source = "vector"
  )
}

.ngeo_cortical_mask <- function(x, mask) {
  mask <- mask %||% x$base$geometry$mask
  if (!is.logical(mask) || length(mask) != nrow(x$base$elements) ||
      anyNA(mask)) {
    .ngeo_abort(
      "`mask` must be a non-missing logical vector with one item per vertex.",
      "ngeo_error_alignment"
    )
  }
  mask
}

.ngeo_cortical_underlay <- function(x, underlay) {
  if (is.null(underlay)) return(NULL)
  if (is.numeric(underlay) &&
      length(underlay) == nrow(x$base$elements)) {
    value <- underlay
    name <- "underlay"
  } else {
    selected <- .ngeo_plot_map(x, underlay)
    value <- selected$values
    name <- selected$name
  }
  if (!is.numeric(value) || length(value) != nrow(x$base$elements)) {
    .ngeo_abort(
      "`underlay` must resolve to one numeric value per surface vertex.",
      "ngeo_error_alignment"
    )
  }
  list(values = value, name = name)
}

.ngeo_cortical_label_positions <- function(coordinates, atlas, mask) {
  if (is.null(atlas)) return(data.frame())
  keep <- mask & !is.na(atlas)
  level <- unique(as.character(atlas[keep]))
  if (!length(level)) return(data.frame())
  result <- lapply(level, function(current) {
    candidate <- which(keep & as.character(atlas) == current)
    center <- c(
      stats::median(coordinates[candidate, 1L]),
      stats::median(coordinates[candidate, 2L])
    )
    distance <- (coordinates[candidate, 1L] - center[[1L]])^2 +
      (coordinates[candidate, 2L] - center[[2L]])^2
    anchor <- candidate[[which.min(distance)]]
    data.frame(
      label = current,
      x = coordinates[anchor, 1L],
      y = coordinates[anchor, 2L],
      source_vertex = anchor,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, result)
}

#' Build an atlas-independent cortical map
#'
#' The map consumes any aligned vertex values and any aligned crisp atlas. It
#' uses the surface's own chart and never substitutes a fixed atlas drawing.
#'
#' @param x An `ngeo_surface` with a chart.
#' @param layer One aligned layer when `values` is not supplied.
#' @param values Optional arbitrary vertex-aligned values.
#' @param chart Chart name.
#' @param atlas Optional aligned labels or `ngeo_partition`.
#' @param palette HCL palette name.
#' @param limits Optional continuous color limits.
#' @param na_color Missing-value color.
#' @param fill Whether face colors represent vertex values or atlas membership.
#' @param mask Optional logical vertex mask. By default the base mask is used.
#'   A face is visible only when all three vertices are included.
#' @param underlay Optional numeric vertex vector or map selector used as a
#'   grayscale or colored anatomical underlay.
#' @param underlay_palette HCL palette for the underlay.
#' @param underlay_limits Optional underlay color limits.
#' @param colors Optional categorical colors. Named colors are matched to
#'   category labels. Label-table colors are used automatically for
#'   `fill = "atlas"` when available.
#' @param overlay_alpha Opacity of the value or atlas layer over the underlay.
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
    layer = 1L,
    values = NULL,
    chart = NULL,
    atlas = NULL,
    palette = "viridis",
    limits = NULL,
    na_color = "grey85",
    fill = c("values", "atlas"),
    mask = NULL,
    underlay = NULL,
    underlay_palette = "Grays",
    underlay_limits = NULL,
    colors = NULL,
    overlay_alpha = 1) {
  .ngeo_cartography_surface(x)
  .ngeo_assert_scalar_character(palette, "palette")
  .ngeo_assert_scalar_character(underlay_palette, "underlay_palette")
  if (!is.character(na_color) || length(na_color) != 1L) {
    .ngeo_abort(
      "`na_color` must be one valid R color or `NA`.",
      "ngeo_error_argument"
    )
  }
  if (!is.na(na_color)) {
    tryCatch(
      grDevices::col2rgb(na_color),
      error = function(...) {
        .ngeo_abort(
          "`na_color` must be one valid R color or `NA`.",
          "ngeo_error_argument"
        )
      }
    )
  }
  if (!is.numeric(overlay_alpha) || length(overlay_alpha) != 1L ||
      is.na(overlay_alpha) || !is.finite(overlay_alpha) ||
      overlay_alpha < 0 || overlay_alpha > 1) {
    .ngeo_abort(
      "`overlay_alpha` must be one number between zero and one.",
      "ngeo_error_argument"
    )
  }
  fill <- match.arg(fill)
  chart_info <- .ngeo_chart_coordinates(x, chart)
  chart <- chart_info$name
  atlas_info <- .ngeo_cortical_atlas(x, atlas, chart)
  value <- if (identical(fill, "atlas")) {
    if (is.null(atlas_info)) {
      .ngeo_abort(
        "`fill = \"atlas\"` requires an aligned `atlas`.",
        "ngeo_error_argument"
      )
    }
    if (is.null(colors)) colors <- atlas_info$colors
    list(
      values = atlas_info$membership,
      name = atlas_info$name,
      type = "categorical"
    )
  } else {
    .ngeo_cortical_vertex_values(x, layer, values)
  }
  faces <- x$base$geometry$faces
  mask <- .ngeo_cortical_mask(x, mask)
  charted_face <- rep.int(FALSE, nrow(faces))
  source_face_in_chart <-
    x$base$charts[[chart]]$invariants$source_face_in_chart %||%
    seq_len(nrow(faces))
  charted_face[source_face_in_chart] <- TRUE
  included_face <- rowSums(
    matrix(mask[faces], ncol = 3L)
  ) == 3L & charted_face
  if (!any(included_face)) {
    .ngeo_abort(
      "`mask` excludes every complete surface face.",
      "ngeo_error_coverage"
    )
  }
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
    na_color,
    colors
  )
  color$color[!included_face] <- NA_character_
  underlay_info <- .ngeo_cortical_underlay(x, underlay)
  underlay_value <- rep.int(NA_real_, nrow(faces))
  underlay_color <- rep.int(NA_character_, nrow(faces))
  if (!is.null(underlay_info)) {
    matrix_underlay <- matrix(underlay_info$values[faces], ncol = 3L)
    matrix_underlay[!is.finite(matrix_underlay)] <- NA_real_
    underlay_value <- rowMeans(matrix_underlay, na.rm = TRUE)
    underlay_value[rowSums(is.finite(matrix_underlay)) == 0L] <- NA_real_
    underlay_scale <- .ngeo_cortical_colors(
      underlay_value,
      "continuous",
      underlay_palette,
      underlay_limits,
      NA_character_
    )
    underlay_color <- underlay_scale$color
    underlay_color[!included_face] <- NA_character_
    underlay_limits <- underlay_scale$limits
  }
  atlas_value <- atlas_info$membership %||% NULL
  boundary <- data.frame()
  visible_edges <- .ngeo_cartography_edges(
    faces[included_face, , drop = FALSE]
  )
  if (!is.null(atlas_value)) {
    left <- atlas_value[visible_edges$from]
    right <- atlas_value[visible_edges$to]
    keep <- (is.na(left) != is.na(right)) |
      (!is.na(left) & !is.na(right) & left != right)
    boundary <- data.frame(
      from = visible_edges$from[keep],
      to = visible_edges$to[keep],
      left = left[keep],
      right = right[keep],
      stringsAsFactors = FALSE
    )
  }
  outline_edges <- visible_edges[visible_edges$count == 1L, , drop = FALSE]
  outline <- data.frame(
    from = outline_edges$from,
    to = outline_edges$to,
    stringsAsFactors = FALSE
  )
  coordinates <- chart_info$coordinates
  atlas_column <- atlas_value %||%
    rep.int(NA_character_, nrow(coordinates))
  vertices <- data.frame(
    source_vertex = seq_len(nrow(coordinates)),
    element_id = x$base$elements$element_id,
    x = coordinates[, 1L],
    y = coordinates[, 2L],
    value = value$values,
    atlas = atlas_column,
    included = mask,
    stringsAsFactors = FALSE
  )
  face_data <- data.frame(
    source_face = seq_len(nrow(faces)),
    vertex_1 = faces[, 1L],
    vertex_2 = faces[, 2L],
    vertex_3 = faces[, 3L],
    value = face_value,
    color = color$color,
    underlay_value = underlay_value,
    underlay_color = underlay_color,
    charted = charted_face,
    included = included_face,
    stringsAsFactors = FALSE
  )
  label_positions <- .ngeo_cortical_label_positions(
    coordinates,
    atlas_value,
    mask
  )
  result <- list(
    source_base_hash = base_hash(x),
    chart = chart,
    chart_metadata = x$base$charts[[chart]],
    coordinates = coordinates,
    faces = faces,
    vertices = vertices,
    face_data = face_data,
    boundaries = boundary,
    outline = outline,
    label_positions = label_positions,
    legend = color$legend,
    limits = color$limits,
    layer_name = value$name,
    value_type = value$type,
    fill = fill,
    palette = palette,
    na_color = na_color,
    colors = colors,
    mask = mask,
    underlay_name = underlay_info$name %||% NULL,
    underlay_palette = underlay_palette,
    underlay_limits = underlay_limits,
    overlay_alpha = overlay_alpha,
    history = list(
      source_base_hash = base_hash(x),
      chart_source_base_hash =
        x$base$charts[[chart]]$source_base_hash,
      seam_faces =
        x$base$charts[[chart]]$invariants$seam_faces %||% integer(),
      atlas_source = atlas_info$source %||% NULL,
      included_vertices = sum(mask),
      included_faces = sum(included_face),
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
    outline = x$outline,
    label_positions = x$label_positions,
    legend = x$legend,
    metadata = list(
      source_base_hash = x$source_base_hash,
      chart = x$chart,
      chart_metadata = x$chart_metadata,
      layer_name = x$layer_name,
      value_type = x$value_type,
      fill = x$fill,
      limits = x$limits,
      palette = x$palette,
      na_color = x$na_color,
      colors = x$colors,
      underlay_name = x$underlay_name,
      underlay_palette = x$underlay_palette,
      underlay_limits = x$underlay_limits,
      overlay_alpha = x$overlay_alpha,
      history = x$history
    )
  )
}

#' Combine cortical layers into an auditable panel layout
#'
#' @param ... `ngeo_cortical_map` objects or one list of them.
#' @param ncol Number of panel columns.
#' @param labels Optional panel labels.
#' @param shared_scale Whether every panel must use one continuous scale or
#'   one categorical color contract and one legend. Categorical layers with
#'   conflicting colors for the same label are rejected.
#' @return An `ngeo_cortical_layout`.
#' @export
ngeo_cortical_layout <- function(
    ...,
    ncol = NULL,
    labels = NULL,
    shared_scale = FALSE) {
  layers <- list(...)
  if (length(layers) == 1L && is.list(layers[[1L]]) &&
      !inherits(layers[[1L]], "ngeo_cortical_map")) {
    layers <- layers[[1L]]
  }
  if (!length(layers) ||
      any(!vapply(layers, inherits, logical(1), "ngeo_cortical_map"))) {
    .ngeo_abort(
      "A cortical layout requires one or more cortical layers.",
      "ngeo_error_argument"
    )
  }
  if (!is.logical(shared_scale) || length(shared_scale) != 1L ||
      is.na(shared_scale)) {
    .ngeo_abort(
      "`shared_scale` must be TRUE or FALSE.",
      "ngeo_error_argument"
    )
  }
  shared_legend <- NULL
  if (isTRUE(shared_scale)) {
    value_type <- unique(vapply(layers, `[[`, character(1), "value_type"))
    if (length(value_type) != 1L) {
      .ngeo_abort(
        "A shared cortical scale requires one common value type.",
        "ngeo_error_alignment"
      )
    }
    if (identical(value_type, "continuous")) {
      palette <- unique(vapply(layers, `[[`, character(1), "palette"))
      if (length(palette) != 1L) {
        .ngeo_abort(
          "A shared continuous scale requires one common palette.",
          "ngeo_error_alignment"
        )
      }
      finite <- unlist(lapply(layers, function(map) {
        value <- map$face_data$value[map$face_data$included]
        value[is.finite(value)]
      }), use.names = FALSE)
      limits <- if (length(finite)) range(finite) else c(0, 1)
      layers <- lapply(layers, function(map) {
        scale <- .ngeo_cortical_colors(
          map$face_data$value,
          "continuous",
          map$palette,
          limits,
          map$na_color
        )
        scale$color[!map$face_data$included] <- NA_character_
        map$face_data$color <- scale$color
        map$legend <- scale$legend
        map$limits <- scale$limits
        map
      })
      shared_legend <- layers[[1L]]$legend
    } else {
      entries <- do.call(rbind, lapply(layers, function(map) {
        data.frame(
          value = as.character(map$legend$value),
          color = as.character(map$legend$color),
          stringsAsFactors = FALSE
        )
      }))
      level <- unique(entries$value)
      color <- vapply(level, function(current) {
        candidate <- unique(entries$color[entries$value == current])
        if (length(candidate) != 1L) {
          .ngeo_abort(
            sprintf(
              "Categorical label `%s` has conflicting panel colors.",
              current
            ),
            "ngeo_error_alignment"
          )
        }
        candidate
      }, character(1))
      shared_legend <- data.frame(
        value = level,
        color = unname(color),
        stringsAsFactors = FALSE
      )
      lookup <- stats::setNames(shared_legend$color, shared_legend$value)
      layers <- lapply(layers, function(map) {
        scale <- .ngeo_cortical_colors(
          map$face_data$value,
          "categorical",
          map$palette,
          NULL,
          map$na_color,
          lookup
        )
        scale$color[!map$face_data$included] <- NA_character_
        map$face_data$color <- scale$color
        map$legend <- shared_legend
        map$colors <- lookup
        map
      })
    }
  }
  ncol <- ncol %||% ceiling(sqrt(length(layers)))
  ncol <- .ngeo_as_integer(ncol, "ncol")
  if (length(ncol) != 1L || ncol < 1L) {
    .ngeo_abort("`ncol` must be positive.", "ngeo_error_argument")
  }
  labels <- labels %||% vapply(layers, `[[`, character(1), "layer_name")
  if (!is.character(labels) || length(labels) != length(layers)) {
    .ngeo_abort(
      "`labels` must align with cortical layers.",
      "ngeo_error_alignment"
    )
  }
  structure(
    list(
      layers = layers,
      ncol = ncol,
      nrow = as.integer(ceiling(length(layers) / ncol)),
      labels = labels,
      shared_scale = shared_scale,
      legend = shared_legend,
      legend_title = if (length(unique(vapply(
        layers,
        `[[`,
        character(1),
        "layer_name"
      ))) == 1L) {
        layers[[1L]]$layer_name
      } else {
        "Shared scale"
      }
    ),
    class = "ngeo_cortical_layout"
  )
}

#' @export
print.ngeo_cortical_map <- function(x, ...) {
  cat(
    "<ngeo_cortical_map>\n",
    "  chart: ", x$chart, "\n",
    "  map: ", x$layer_name, " (", x$value_type, ")\n",
    "  vertices: ", nrow(x$vertices), "\n",
    "  visible faces: ", sum(x$face_data$included), " / ",
    nrow(x$face_data), "\n",
    "  outline edges: ", nrow(x$outline), "\n",
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
#' @param show_outline Draw the visible cortical-sheet outline.
#' @param outline_color Cortical outline color.
#' @param outline_lwd Cortical outline line width.
#' @param show_labels Draw atlas labels at vertex-constrained anchors.
#' @param label_regions Optional atlas labels to draw.
#' @param label_color,label_cex,label_font Atlas-label appearance.
#' @param main Plot title.
#' @param axes Draw coordinate axes.
#' @param show_legend Draw the value legend.
#' @param legend_position Categorical legend position, or `"bottom"` for a
#'   horizontal continuous color bar.
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
    show_outline = TRUE,
    outline_color = "grey10",
    outline_lwd = 1,
    show_labels = FALSE,
    label_regions = NULL,
    label_color = "grey10",
    label_cex = 0.65,
    label_font = 2,
    main = x$layer_name,
    axes = FALSE,
    show_legend = TRUE,
    legend_position = NULL,
    legend_cex = 0.75,
    xlim = NULL,
    ylim = NULL,
    ...) {
  coordinates <- x$coordinates
  faces <- x$faces
  spherical <- identical(x$chart_metadata$method, "spherical")
  legend_position <- legend_position %||%
    if (identical(x$value_type, "continuous")) "bottom" else "topright"
  if (is.null(xlim)) {
    xlim <- if (spherical) c(-pi, pi) else range(coordinates[, 1L])
  }
  if (is.null(ylim)) {
    ylim <- if (spherical) c(-pi / 2, pi / 2) else
      range(coordinates[, 2L])
  }
  if (isTRUE(show_legend) &&
      identical(x$value_type, "continuous") &&
      identical(legend_position, "bottom")) {
    ylim <- c(ylim[[1L]] - 0.16 * diff(ylim), ylim[[2L]])
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
  included_faces <- which(x$face_data$included)
  regular_faces <- if (length(seam_faces)) {
    intersect(setdiff(seq_len(nrow(faces)), seam_faces), included_faces)
  } else {
    included_faces
  }
  seam_faces <- intersect(seam_faces, included_faces)
  draw_faces <- function(index, colors, shift = NULL, mesh = FALSE) {
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
      col = colors[index],
      border = if (isTRUE(mesh)) "grey70" else NA
    )
    invisible(NULL)
  }
  if (any(!is.na(x$face_data$underlay_color[included_faces]))) {
    draw_faces(
      regular_faces,
      x$face_data$underlay_color,
      mesh = FALSE
    )
    if (length(seam_faces)) {
      draw_faces(
        seam_faces,
        x$face_data$underlay_color,
        "positive",
        mesh = FALSE
      )
      draw_faces(
        seam_faces,
        x$face_data$underlay_color,
        "negative",
        mesh = FALSE
      )
    }
  }
  overlay_color <- x$face_data$color
  keep_color <- !is.na(overlay_color)
  if (x$overlay_alpha < 1 && any(keep_color)) {
    overlay_color[keep_color] <- grDevices::adjustcolor(
      overlay_color[keep_color],
      alpha.f = x$overlay_alpha
    )
  }
  draw_faces(
    regular_faces,
    overlay_color,
    mesh = show_mesh
  )
  if (length(seam_faces)) {
    draw_faces(seam_faces, overlay_color, "positive", show_mesh)
    draw_faces(seam_faces, overlay_color, "negative", show_mesh)
  }
  draw_edges <- function(edge, color, line_width) {
    if (!nrow(edge)) return(invisible(NULL))
    from_x <- coordinates[edge$from, 1L]
    to_x <- coordinates[edge$to, 1L]
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
        coordinates[edge$from[index], 2L],
        x1,
        coordinates[edge$to[index], 2L],
        col = color,
        lwd = line_width
      )
      invisible(NULL)
    }
    draw_segments(which(!crossing))
    draw_segments(which(crossing), "positive")
    draw_segments(which(crossing), "negative")
    invisible(NULL)
  }
  if (isTRUE(show_boundaries) && nrow(x$boundaries)) {
    draw_edges(x$boundaries, boundary_color, boundary_lwd)
  }
  if (isTRUE(show_outline) && nrow(x$outline)) {
    draw_edges(x$outline, outline_color, outline_lwd)
  }
  if (isTRUE(show_labels) && nrow(x$label_positions)) {
    label <- x$label_positions
    if (!is.null(label_regions)) {
      if (!is.character(label_regions)) {
        .ngeo_abort(
          "`label_regions` must be a character vector.",
          "ngeo_error_argument"
        )
      }
      label <- label[label$label %in% label_regions, , drop = FALSE]
    }
    graphics::text(
      label$x,
      label$y,
      labels = label$label,
      col = label_color,
      cex = label_cex,
      font = label_font
    )
  }
  if (isTRUE(show_legend) && nrow(x$legend)) {
    if (identical(x$value_type, "continuous") &&
        identical(legend_position, "bottom")) {
      user <- graphics::par("usr")
      width <- diff(user[1:2])
      height <- diff(user[3:4])
      left <- user[[1L]] + 0.28 * width
      right <- user[[1L]] + 0.72 * width
      bottom <- user[[3L]] + 0.04 * height
      top <- user[[3L]] + 0.075 * height
      color_bar <- matrix(
        .ngeo_cartography_palette(256L, x$palette),
        nrow = 1L
      )
      graphics::rasterImage(
        grDevices::as.raster(color_bar),
        left,
        bottom,
        right,
        top,
        interpolate = TRUE
      )
      graphics::rect(left, bottom, right, top, border = "grey30")
      graphics::text(
        c(left, right),
        bottom - 0.018 * height,
        labels = format(signif(x$limits, 5L), trim = TRUE),
        adj = c(0.5, 1),
        cex = legend_cex
      )
      graphics::text(
        (left + right) / 2,
        top + 0.012 * height,
        labels = x$layer_name,
        adj = c(0.5, 0),
        cex = legend_cex
      )
    } else {
      legend_label <- if (identical(x$value_type, "continuous")) {
        format(signif(x$legend$value, 5L), trim = TRUE)
      } else {
        as.character(x$legend$value)
      }
      legend_color <- x$legend$color
      visible_color <- x$face_data$color[x$face_data$included]
      has_missing <- !is.na(x$na_color) &&
        any(!is.na(visible_color) & visible_color == x$na_color)
      if (has_missing) {
        legend_label <- c(legend_label, "NA")
        legend_color <- c(legend_color, x$na_color)
      }
      graphics::legend(
        legend_position,
        legend = legend_label,
        fill = legend_color,
        title = x$layer_name,
        bty = "n",
        cex = legend_cex
      )
    }
  }
  invisible(x)
}

#' @export
print.ngeo_cortical_layout <- function(x, ...) {
  cat(
    "<ngeo_cortical_layout>\n",
    "  panels: ", length(x$layers), "\n",
    "  arrangement: ", x$nrow, " x ", x$ncol, "\n",
    "  shared scale: ", isTRUE(x$shared_scale), "\n",
    sep = ""
  )
  invisible(x)
}

.ngeo_plot_cortical_layout_legend <- function(x, cex = 0.75) {
  map <- x$layers[[1L]]
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  if (identical(map$value_type, "continuous")) {
    left <- 0.25
    right <- 0.75
    bottom <- 0.43
    top <- 0.62
    color_bar <- matrix(
      .ngeo_cartography_palette(256L, map$palette),
      nrow = 1L
    )
    graphics::rasterImage(
      grDevices::as.raster(color_bar),
      left,
      bottom,
      right,
      top,
      interpolate = TRUE
    )
    graphics::rect(left, bottom, right, top, border = "grey30")
    graphics::text(
      c(left, right),
      bottom - 0.08,
      labels = format(signif(map$limits, 5L), trim = TRUE),
      cex = cex
    )
    graphics::text(
      0.5,
      top + 0.08,
      labels = x$legend_title,
      cex = cex
    )
  } else {
    graphics::legend(
      "center",
      legend = as.character(x$legend$value),
      fill = x$legend$color,
      title = x$legend_title,
      bty = "n",
      cex = cex,
      ncol = min(6L, nrow(x$legend))
    )
  }
  invisible(NULL)
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
  dots <- list(...)
  show_legend <- (is.null(dots$show_legend) ||
    isTRUE(dots$show_legend)) &&
    (!isTRUE(x$shared_scale) || nrow(x$legend) > 0L)
  dots$show_legend <- if (isTRUE(x$shared_scale)) FALSE else
    dots$show_legend
  if (isTRUE(x$shared_scale) && show_legend) {
    panel <- matrix(
      seq_len(x$nrow * x$ncol),
      nrow = x$nrow,
      ncol = x$ncol,
      byrow = TRUE
    )
    panel[panel > length(x$layers)] <- 0L
    graphics::layout(
      rbind(panel, rep.int(length(x$layers) + 1L, x$ncol)),
      heights = c(rep.int(1, x$nrow), 0.22)
    )
  } else {
    graphics::par(mfrow = c(x$nrow, x$ncol))
  }
  for (i in seq_along(x$layers)) {
    current <- dots
    current$x <- x$layers[[i]]
    current$main <- x$base$labels[[i]]
    do.call(graphics::plot, current)
  }
  if (isTRUE(x$shared_scale) && show_legend) {
    graphics::par(mar = rep.int(0, 4L))
    .ngeo_plot_cortical_layout_legend(
      x,
      cex = dots$legend_cex %||% 0.75
    )
  }
  invisible(x)
}
