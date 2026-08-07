#' Compute barycentric vertex area
#'
#' Triangle area is divided equally among its three vertices.
#'
#' @param x An `ngeo_surface` object.
#' @param coordinates Coordinate-set name, or `"active"`.
#'
#' @return A numeric vector aligned to surface elements.
#' @export
ngeo_vertex_area <- function(x, coordinates = "active") {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort("`x` must be an `ngeo_surface`.", "ngeo_error_argument")
  }
  ngeo_validate(x, "basic")

  if (identical(coordinates, "active")) {
    coordinates <- x$base$geometry$active_coordinates
  }
  if (!coordinates %in% names(x$base$geometry$coordinates)) {
    .ngeo_abort(
      sprintf("Unknown coordinate set `%s`.", coordinates),
      "ngeo_error_geometry"
    )
  }
  meta <- x$base$geometry$coordinate_meta
  meta <- meta[match(coordinates, meta$name), , drop = FALSE]
  if (!isTRUE(meta$metric_eligible)) {
    .ngeo_abort(
      sprintf(
        "Coordinate set `%s` is not distance_method-eligible.",
        coordinates
      ),
      "ngeo_error_metric"
    )
  }

  xyz <- x$base$geometry$coordinates[[coordinates]]
  if (ncol(xyz) == 2L) {
    xyz <- cbind(xyz, 0)
  }
  faces <- x$base$geometry$faces
  if (!nrow(faces)) {
    return(numeric(nrow(x$base$elements)))
  }

  a <- xyz[faces[, 2L], , drop = FALSE] -
    xyz[faces[, 1L], , drop = FALSE]
  b <- xyz[faces[, 3L], , drop = FALSE] -
    xyz[faces[, 1L], , drop = FALSE]
  cross <- cbind(
    a[, 2L] * b[, 3L] - a[, 3L] * b[, 2L],
    a[, 3L] * b[, 1L] - a[, 1L] * b[, 3L],
    a[, 1L] * b[, 2L] - a[, 2L] * b[, 1L]
  )
  triangle_area <- sqrt(rowSums(cross^2)) / 2

  area <- numeric(nrow(x$base$elements))
  share <- triangle_area / 3
  for (corner in seq_len(3L)) {
    grouped <- rowsum(share, faces[, corner], reorder = FALSE)
    vertex <- as.integer(rownames(grouped))
    area[vertex] <- area[vertex] + grouped[, 1L]
  }
  area
}

#' Compute voxel volume
#'
#' @param x An `ngeo_volume` object.
#' @return The absolute determinant of the affine linear component.
#' @export
ngeo_voxel_volume <- function(x) {
  if (!inherits(x, "ngeo_volume")) {
    .ngeo_abort("`x` must be an `ngeo_volume`.", "ngeo_error_argument")
  }
  ngeo_validate(x, "basic")
  abs(det(x$base$geometry$affine[1:3, 1:3, drop = FALSE]))
}
