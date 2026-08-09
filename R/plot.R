.ngeo_plot_map <- function(x, layer) {
  if (is.null(x$values)) {
    return(list(values = NULL, name = "geometry"))
  }
  index <- .ngeo_layer_selection(x, layer)
  if (length(index) != 1L) {
    .ngeo_abort(
      "`layer` must select exactly one layer for plotting.",
      "ngeo_error_argument"
    )
  }
  list(
    values = as.numeric(x$values[, index]),
    name = x$layers$name[[index]]
  )
}

.ngeo_plot_colors <- function(values, palette) {
  if (is.null(values)) {
    return(rep.int("grey35", 1L))
  }
  finite <- is.finite(values)
  colors <- rep.int("grey85", length(values))
  if (!any(finite)) {
    return(colors)
  }
  limits <- range(values[finite])
  if (limits[[1L]] == limits[[2L]]) {
    index <- rep.int(32L, sum(finite))
  } else {
    index <- 1L + floor(
      63 * (values[finite] - limits[[1L]]) /
        (limits[[2L]] - limits[[1L]])
    )
  }
  colors[finite] <- grDevices::hcl.colors(64L, palette)[index]
  colors
}

.ngeo_plot_surface_coordinates <- function(x, chart) {
  chart_names <- x$base$geometry$coordinate_meta$name[
    x$base$geometry$coordinate_meta$role == "chart"
  ]
  if (!is.null(chart) || length(chart_names)) {
    selected <- chart %||% chart_names[[1L]]
    return(.ngeo_chart_coordinates(x, selected)$coordinates)
  }
  coordinates <- x$base$geometry$coordinates[[x$base$geometry$active_coordinates]]
  if (ncol(coordinates) == 3L) {
    .ngeo_warn(
      "No chart is available; the surface diagnostic uses an XY projection.",
      "ngeo_warning_plot_projection"
    )
  }
  coordinates[, 1:2, drop = FALSE]
}

.ngeo_plot_surface <- function(x,
                               layer,
                               chart,
                               palette,
                               show_edges,
                               ...) {
  coordinates <- .ngeo_plot_surface_coordinates(x, chart)
  map_data <- .ngeo_plot_map(x, layer)
  colors <- .ngeo_plot_colors(map_data$values, palette)
  graphics::plot(
    coordinates,
    type = "n",
    asp = 1,
    xlab = "x",
    ylab = "y",
    main = map_data$name,
    ...
  )
  if (isTRUE(show_edges) && nrow(x$base$geometry$faces)) {
    edges <- .ngeo_surface_edges(x$base$geometry$faces)
    maximum <- getOption("neurogeo.max_plot_edges", 500000L)
    if (nrow(edges) > maximum) {
      .ngeo_abort(
        "Surface diagnostic exceeds the configured edge plotting limit.",
        "ngeo_error_resource"
      )
    }
    graphics::segments(
      coordinates[edges[, 1L], 1L],
      coordinates[edges[, 1L], 2L],
      coordinates[edges[, 2L], 1L],
      coordinates[edges[, 2L], 2L],
      col = "grey75"
    )
  }
  graphics::points(
    coordinates,
    pch = 21,
    bg = colors,
    col = colors,
    cex = if (nrow(coordinates) > 5000L) 0.25 else 0.8
  )
}

.ngeo_plot_volume <- function(x, layer, slice, palette, ...) {
  map_data <- .ngeo_plot_map(x, layer)
  if (is.null(map_data$values)) {
    values <- rep.int(1, nrow(x$base$elements))
  } else {
    values <- map_data$values
  }
  index <- x$base$geometry$voxel_index
  available <- sort(unique(index[, 3L]))
  slice <- slice %||% available[[ceiling(length(available) / 2)]]
  if (!is.numeric(slice) || length(slice) != 1L ||
      is.na(slice) || slice != floor(slice) ||
      !slice %in% available) {
    .ngeo_abort(
      "`slice` must select an active internal voxel k-index.",
      "ngeo_error_argument"
    )
  }
  image <- matrix(
    NA_real_,
    nrow = x$base$geometry$dim[[1L]],
    ncol = x$base$geometry$dim[[2L]]
  )
  selected <- which(index[, 3L] == slice)
  image[cbind(index[selected, 1L], index[selected, 2L])] <- values[selected]
  graphics::image(
    seq_len(nrow(image)),
    seq_len(ncol(image)),
    image,
    asp = 1,
    col = grDevices::hcl.colors(64L, palette),
    xlab = "i",
    ylab = "j",
    main = paste0(map_data$name, " (k = ", slice, ")"),
    ...
  )
}

.ngeo_plot_points <- function(x, layer, palette, ...) {
  coordinates <- x$base$geometry$coordinates
  if (ncol(coordinates) == 3L) {
    .ngeo_warn(
      "The point diagnostic uses an XY projection.",
      "ngeo_warning_plot_projection"
    )
  }
  map_data <- .ngeo_plot_map(x, layer)
  graphics::plot(
    coordinates[, 1:2, drop = FALSE],
    pch = 21,
    bg = .ngeo_plot_colors(map_data$values, palette),
    asp = 1,
    xlab = "x",
    ylab = "y",
    main = map_data$name,
    ...
  )
}

.ngeo_plot_grayordinates <- function(x, layer, palette, ...) {
  map_data <- .ngeo_plot_map(x, layer)
  values <- map_data$values %||% rep.int(1, nrow(x$base$elements))
  colors <- .ngeo_plot_colors(values, palette)
  graphics::plot(
    seq_along(values),
    values,
    pch = 21,
    bg = colors,
    col = colors,
    xlab = "Ordered grayordinate",
    ylab = if (is.null(map_data$values)) "component" else map_data$name,
    main = paste(map_data$name, "by component"),
    ...
  )
  boundaries <- cumsum(vapply(
    x$base$geometry$components,
    function(component) as.integer(component$n_element),
    integer(1)
  ))
  if (length(boundaries) > 1L) {
    graphics::abline(v = utils::head(boundaries, -1L) + 0.5, lty = 2)
  }
}

.ngeo_plot_regions <- function(x, layer, palette, ...) {
  map_data <- .ngeo_plot_map(x, layer)
  if (!is.null(x$base$geometry$centroid)) {
    coordinates <- x$base$geometry$centroid
    if (ncol(coordinates) == 3L) {
      .ngeo_warn(
        "The region diagnostic uses an XY centroid projection.",
        "ngeo_warning_plot_projection"
      )
    }
    graphics::plot(
      coordinates[, 1:2, drop = FALSE],
      pch = 21,
      bg = .ngeo_plot_colors(map_data$values, palette),
      asp = 1,
      xlab = "x",
      ylab = "y",
      main = map_data$name,
      ...
    )
  } else {
    values <- map_data$values %||% x$base$geometry$support_size
    if (is.null(values) || !any(is.finite(values))) {
      values <- rep.int(1, nrow(x$base$elements))
    }
    graphics::barplot(
      values,
      names.arg = x$base$elements$region_id,
      ylab = map_data$name,
      main = map_data$name,
      ...
    )
  }
}

#' Plot an NGCS dataset diagnostic
#'
#' @param x An `ngeo` dataset.
#' @param layer One layer name, ID, or index.
#' @param chart Optional surface chart.
#' @param slice Optional volume internal k-index.
#' @param palette Base HCL palette name.
#' @param show_edges Draw surface edges.
#' @param ... Additional base-graphics arguments.
#'
#' @return `x`, invisibly.
#' @export
plot.ngeo <- function(x,
                      layer = 1L,
                      chart = NULL,
                      slice = NULL,
                      palette = "viridis",
                      show_edges = TRUE,
                      ...) {
  ngeo_validate(x, "basic")
  switch(
    x$base$type,
    surface = .ngeo_plot_surface(
      x, layer, chart, palette, show_edges, ...
    ),
    volume = .ngeo_plot_volume(x, layer, slice, palette, ...),
    point = .ngeo_plot_points(x, layer, palette, ...),
    grayordinate = .ngeo_plot_grayordinates(x, layer, palette, ...),
    parcellation = .ngeo_plot_regions(x, layer, palette, ...)
  )
  invisible(x)
}

#' Plot a spatial spatial_weights diagnostic
#'
#' @param x An `ngeo_spatial_weights`.
#' @param ... Additional histogram arguments.
#'
#' @return `x`, invisibly.
#' @export
plot.ngeo_spatial_weights <- function(x, ...) {
  degree <- Matrix::rowSums(x$raw_matrix != 0)
  graphics::hist(
    degree,
    breaks = "FD",
    xlab = "Neighbor count",
    main = paste("Weights:", x$method),
    ...
  )
  invisible(x)
}

#' Plot a partition diagnostic
#'
#' @param x An `ngeo_partition`.
#' @param ... Additional barplot arguments.
#'
#' @return `x`, invisibly.
#' @export
plot.ngeo_partition <- function(x, ...) {
  counts <- table(
    factor(x$membership, levels = x$parcellation$region_id),
    useNA = "no"
  )
  graphics::barplot(
    counts,
    xlab = "Region",
    ylab = "Element count",
    main = "Partition region sizes",
    ...
  )
  invisible(x)
}
