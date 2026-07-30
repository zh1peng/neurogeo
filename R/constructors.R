.ngeo_coordinates <- function(coordinates) {
  if (is.matrix(coordinates)) {
    coordinates <- list(active = coordinates)
  }
  if (!is.list(coordinates) || !length(coordinates)) {
    .ngeo_abort(
      "`coordinates` must be a matrix or a non-empty named list of matrices.",
      "ngeo_error_geometry"
    )
  }

  if (is.null(names(coordinates)) || any(!nzchar(names(coordinates)))) {
    names(coordinates) <- paste0("coordinates_", seq_along(coordinates))
  }
  if (anyDuplicated(names(coordinates))) {
    .ngeo_abort(
      "Coordinate-set names must be unique.",
      "ngeo_error_geometry"
    )
  }

  n <- NULL
  for (name in names(coordinates)) {
    value <- coordinates[[name]]
    if (!is.matrix(value) || !is.numeric(value) ||
        !ncol(value) %in% c(2L, 3L) ||
        anyNA(value) || any(!is.finite(value))) {
      .ngeo_abort(
        sprintf(
          "Coordinate set `%s` must be a finite numeric matrix with 2 or 3 columns.",
          name
        ),
        "ngeo_error_geometry"
      )
    }
    if (is.null(n)) {
      n <- nrow(value)
    } else if (nrow(value) != n) {
      .ngeo_abort(
        "All coordinate sets must have the same number of rows.",
        "ngeo_error_alignment"
      )
    }
  }
  coordinates
}

.ngeo_faces <- function(faces, n_vertex, index_base) {
  if (is.data.frame(faces)) {
    faces <- as.matrix(faces)
  }
  if (!is.matrix(faces) || ncol(faces) != 3L) {
    .ngeo_abort(
      "`faces` must be a matrix with exactly three columns.",
      "ngeo_error_geometry"
    )
  }
  original_dim <- dim(faces)
  faces <- .ngeo_as_integer(faces, "faces")
  dim(faces) <- original_dim

  if (identical(index_base, "zero")) {
    faces <- faces + 1L
  }
  if (length(faces) && (min(faces) < 1L || max(faces) > n_vertex)) {
    .ngeo_abort(
      sprintf("Face indices must resolve to vertices 1 through %d.", n_vertex),
      "ngeo_error_index"
    )
  }
  faces
}

#' Construct a surface dataset
#'
#' @param coordinates A coordinate matrix or named list of matrices.
#' @param faces A three-column triangle index matrix.
#' @param values Optional vertex-aligned values.
#' @param maps Optional map metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param space An `ngeo_space`.
#' @param active_coordinates Name of the active coordinate set.
#' @param coordinate_roles Roles corresponding to coordinate sets.
#' @param mask Optional logical vertex mask.
#' @param index_base Index base used by `faces` and source vertex indices.
#' @param source_index_base Optional source vertex index base when the R
#'   backend has already converted face indices.
#'
#' @return An `ngeo_surface` object.
#' @examples
#' coordinates <- matrix(
#'   c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
#'   ncol = 3, byrow = TRUE
#' )
#' faces <- matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE)
#' surface <- ngeo_surface(
#'   coordinates, faces,
#'   values = cbind(thickness = c(2.1, 2.3, 2.2, 2.4))
#' )
#' ngeo_domain_type(surface)
#' ngeo_elements(surface)
#' ngeo_values(surface)
#' ngeo_vertex_area(surface)
#' ngeo_validate(surface, "strict")
#' @export
ngeo_surface <- function(coordinates,
                         faces,
                         values = NULL,
                         maps = NULL,
                         measures = NULL,
                         labels = list(),
                         space = ngeo_space(kind = "surface"),
                         active_coordinates = NULL,
                         coordinate_roles = NULL,
                         mask = NULL,
                         index_base = c("one", "zero"),
                         source_index_base = NULL) {
  index_base <- match.arg(index_base)
  coordinates <- .ngeo_coordinates(coordinates)
  n_vertex <- nrow(coordinates[[1L]])
  faces <- .ngeo_faces(faces, n_vertex, index_base)

  if (!inherits(space, "ngeo_space")) {
    .ngeo_abort("`space` must be an `ngeo_space` object.", "ngeo_error_space")
  }
  if (!space$kind %in% c("surface", "unknown")) {
    .ngeo_abort(
      "A surface domain requires a surface or unknown space kind.",
      "ngeo_error_space"
    )
  }

  active_coordinates <- active_coordinates %||% names(coordinates)[1L]
  if (!active_coordinates %in% names(coordinates)) {
    .ngeo_abort(
      "`active_coordinates` must name an available coordinate set.",
      "ngeo_error_geometry"
    )
  }

  if (is.null(coordinate_roles)) {
    coordinate_roles <- rep.int("anatomical", length(coordinates))
  }
  if (length(coordinate_roles) != length(coordinates)) {
    .ngeo_abort(
      "`coordinate_roles` must align with coordinate sets.",
      "ngeo_error_alignment"
    )
  }
  allowed_roles <- c("anatomical", "registration", "visualization", "chart")
  if (any(!coordinate_roles %in% allowed_roles)) {
    .ngeo_abort(
      sprintf(
        "`coordinate_roles` must use: %s.",
        paste(allowed_roles, collapse = ", ")
      ),
      "ngeo_error_geometry"
    )
  }

  if (is.null(mask)) {
    mask <- rep.int(TRUE, n_vertex)
  }
  if (!is.logical(mask) || length(mask) != n_vertex || anyNA(mask)) {
    .ngeo_abort(
      sprintf("`mask` must be a non-missing logical vector of length %d.", n_vertex),
      "ngeo_error_alignment"
    )
  }

  source_base <- source_index_base %||%
    if (identical(index_base, "zero")) 0L else 1L
  source_base <- .ngeo_as_integer(source_base, "source_index_base")
  if (length(source_base) != 1L || !source_base %in% c(0L, 1L)) {
    .ngeo_abort(
      "`source_index_base` must be 0 or 1.",
      "ngeo_error_index"
    )
  }
  elements <- .ngeo_element_table(n_vertex, source_base)
  elements$included <- mask

  coordinate_meta <- data.frame(
    name = names(coordinates),
    dimension = vapply(coordinates, ncol, integer(1)),
    role = coordinate_roles,
    units = rep.int(space$units, length(coordinates)),
    metric_eligible = coordinate_roles == "anatomical",
    stringsAsFactors = FALSE
  )

  domain <- structure(
    list(
      type = "surface",
      elements = elements,
      coordinates = coordinates,
      coordinate_meta = coordinate_meta,
      active_coordinates = active_coordinates,
      faces = faces,
      face_source_index_base = source_base,
      space = space,
      mask = mask
    ),
    class = c("ngeo_surface_domain", "ngeo_domain")
  )

  provenance <- list(
    operations = list(.ngeo_operation(
      "ngeo_surface",
      list(
        internal_index_base = 1L,
        source_index_base = source_base
      )
    ))
  )

  .new_ngeo(
    domain = domain,
    values = values,
    maps = maps,
    measures = measures,
    labels = labels,
    provenance = provenance,
    class = "ngeo_surface"
  )
}

.ngeo_volume_values <- function(values, dim, mask) {
  if (is.null(values)) {
    return(NULL)
  }
  n_lattice <- prod(dim)
  n_active <- sum(mask)

  if (is.array(values) && length(dim(values)) >= 3L) {
    value_dim <- dim(values)
    if (!identical(as.integer(value_dim[1:3]), as.integer(dim))) {
      .ngeo_abort(
        "The first three `values` dimensions must equal `dim`.",
        "ngeo_error_alignment"
      )
    }
    values <- matrix(values, nrow = n_lattice)
    return(values[mask, , drop = FALSE])
  }

  if (is.data.frame(values)) {
    values <- as.matrix(values)
  } else if (is.atomic(values) && is.null(dim(values))) {
    if (length(values) == n_lattice) {
      values <- matrix(values[mask], ncol = 1L)
    } else {
      values <- matrix(values, ncol = 1L)
    }
  }

  if (!is.matrix(values)) {
    .ngeo_abort(
      "`values` must be an array, matrix, vector, or NULL.",
      "ngeo_error_values"
    )
  }
  if (nrow(values) == n_lattice) {
    values <- values[mask, , drop = FALSE]
  }
  if (nrow(values) != n_active) {
    .ngeo_abort(
      sprintf(
        "`values` has %d rows; expected %d active voxels or %d lattice voxels.",
        nrow(values), n_active, n_lattice
      ),
      "ngeo_error_alignment"
    )
  }
  values
}

#' Construct a volume dataset
#'
#' @param values Optional voxel values as an array, matrix, or vector.
#' @param dim Three lattice dimensions.
#' @param affine Active voxel-to-world 4 by 4 affine.
#' @param mask Optional logical lattice mask.
#' @param maps Optional map metadata.
#' @param measures Optional measurement semantics.
#' @param labels Optional label tables.
#' @param space An `ngeo_space`.
#' @param index_base Source IJK index base to preserve.
#'
#' @return An `ngeo_volume` object.
#' @examples
#' image <- array(seq_len(8), dim = c(2, 2, 2))
#' volume <- ngeo_volume(
#'   values = image,
#'   dim = dim(image),
#'   affine = diag(4)
#' )
#' ngeo_voxel_volume(volume)
#' ngeo_support_size(volume)
#' ngeo_validate(volume, "strict")
#' @export
ngeo_volume <- function(values = NULL,
                        dim,
                        affine,
                        mask = NULL,
                        maps = NULL,
                        measures = NULL,
                        labels = list(),
                        space = ngeo_space(kind = "volume"),
                        index_base = c("one", "zero")) {
  index_base <- match.arg(index_base)
  dim <- .ngeo_as_integer(dim, "dim")
  if (length(dim) != 3L || any(dim <= 0L)) {
    .ngeo_abort(
      "`dim` must contain three positive integers.",
      "ngeo_error_geometry"
    )
  }

  if (!is.matrix(affine) || !is.numeric(affine) ||
      !identical(base::dim(affine), c(4L, 4L)) ||
      anyNA(affine) || any(!is.finite(affine))) {
    .ngeo_abort(
      "`affine` must be a finite numeric 4 by 4 matrix.",
      "ngeo_error_geometry"
    )
  }
  if (abs(det(affine[1:3, 1:3, drop = FALSE])) <=
      .Machine$double.eps) {
    .ngeo_abort(
      "The affine linear component must be non-singular.",
      "ngeo_error_geometry"
    )
  }

  if (!inherits(space, "ngeo_space")) {
    .ngeo_abort("`space` must be an `ngeo_space` object.", "ngeo_error_space")
  }
  if (!space$kind %in% c("volume", "unknown")) {
    .ngeo_abort(
      "A volume domain requires a volume or unknown space kind.",
      "ngeo_error_space"
    )
  }

  n_lattice <- prod(dim)
  if (is.null(mask)) {
    mask <- rep.int(TRUE, n_lattice)
  } else if (is.array(mask)) {
    if (!identical(as.integer(base::dim(mask)), as.integer(dim))) {
      .ngeo_abort(
        "Array `mask` dimensions must equal `dim`.",
        "ngeo_error_alignment"
      )
    }
    mask <- as.vector(mask)
  }
  if (!is.logical(mask) || length(mask) != n_lattice || anyNA(mask)) {
    .ngeo_abort(
      sprintf(
        "`mask` must be a non-missing logical vector/array of length %d.",
        n_lattice
      ),
      "ngeo_error_alignment"
    )
  }

  values <- .ngeo_volume_values(values, dim, mask)
  voxel_index <- arrayInd(which(mask), .dim = dim)
  source_base <- if (identical(index_base, "zero")) 0L else 1L
  source_voxel_index <- voxel_index - 1L + source_base

  elements <- .ngeo_element_table(nrow(voxel_index), source_base)
  elements$included <- TRUE

  domain <- structure(
    list(
      type = "volume",
      elements = elements,
      dim = dim,
      affine = affine,
      voxel_index = voxel_index,
      source_voxel_index = source_voxel_index,
      source_index_base = source_base,
      header_transforms = list(),
      space = space,
      mask = mask
    ),
    class = c("ngeo_volume_domain", "ngeo_domain")
  )

  provenance <- list(
    operations = list(.ngeo_operation(
      "ngeo_volume",
      list(
        internal_index_base = 1L,
        source_index_base = source_base
      )
    ))
  )

  .new_ngeo(
    domain = domain,
    values = values,
    maps = maps,
    measures = measures,
    labels = labels,
    provenance = provenance,
    class = "ngeo_volume"
  )
}
