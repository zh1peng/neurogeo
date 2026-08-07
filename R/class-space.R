#' Construct a coordinate-space description
#'
#' @param space_id A stable coordinate_space name or `"unknown"`.
#' @param kind One of `"surface"`, `"volume"`, `"hybrid"`, or `"unknown"`.
#' @param unit Coordinate unit.
#' @param structure Optional brain structure.
#' @param template Optional template identifier.
#' @param density Optional surface density.
#' @param resolution Optional voxel resolution.
#' @param source_metadata Source metadata retained by an importer.
#'
#' @return An `ngeo_coordinate_space` object.
#' @examples
#' fs_lr <- ngeo_coordinate_space(
#'   "fsLR-32k",
#'   kind = "surface",
#'   unit = "mm",
#'   structure = "CORTEX_LEFT",
#'   template = "fsLR",
#'   density = "32k"
#' )
#' fs_lr
#' ngeo_coordinate_space_hash(fs_lr)
#' @export
ngeo_coordinate_space <- function(space_id = "unknown",
                       kind = c("unknown", "surface", "volume", "hybrid"),
                       unit = "mm",
                       structure = NULL,
                       template = NULL,
                       density = NULL,
                       resolution = NULL,
                       source_metadata = list()) {
  kind <- match.arg(kind)
  .ngeo_assert_scalar_character(space_id, "space_id")
  .ngeo_assert_scalar_character(unit, "unit")

  if (!is.list(source_metadata)) {
    .ngeo_abort(
      "`source_metadata` must be a list.",
      "ngeo_error_argument"
    )
  }

  structure(
    list(
      space_id = space_id,
      kind = kind,
      unit = unit,
      structure = structure,
      template = template,
      density = density,
      resolution = resolution,
      source_metadata = source_metadata
    ),
    class = "ngeo_coordinate_space"
  )
}

#' @export
print.ngeo_coordinate_space <- function(x, ...) {
  cat(
    "<ngeo_coordinate_space> ", x$space_id,
    " [", x$kind, ", ", x$unit, "]\n",
    sep = ""
  )
  invisible(x)
}
