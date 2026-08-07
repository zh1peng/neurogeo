.ngeo_builder_support <- function(source, source_support = NULL) {
  candidate <- source_support %||% ngeo_support_size(source)
  if (!is.numeric(candidate) ||
      length(candidate) != nrow(source$base$elements) ||
      anyNA(candidate) || any(!is.finite(candidate)) ||
      any(candidate <= 0)) {
    .ngeo_abort(
      "A builder requires positive source support aligned to every element.",
      "ngeo_error_support"
    )
  }
  as.numeric(candidate)
}

.ngeo_builder_registration <- function(source, target, registration) {
  source_space <- source$base$coordinate_space
  target_space <- target$base$coordinate_space
  source_structure <- source_space$structure
  target_structure <- target_space$structure
  if (!is.null(source_structure) && !is.null(target_structure) &&
      !identical(source_structure, target_structure)) {
    .ngeo_abort(
      "Support mapping across different brain structures is not allowed.",
      "ngeo_error_coordinate_space"
    )
  }
  same_known_space <- identical(
    source_space$space_id,
    target_space$space_id
  ) && !identical(source_space$space_id, "unknown")
  if (is.null(registration) && !same_known_space) {
    .ngeo_abort(
      paste(
        "Different or unknown spaces require an explicit identifier for the",
        "already-known registration."
      ),
      "ngeo_error_coordinate_space"
    )
  }
  registration <- registration %||% source_space$space_id
  .ngeo_assert_scalar_character(registration, "registration")
  registration
}

.ngeo_builder_map <- function(source,
                              target,
                              operator,
                              type,
                              source_support,
                              coverage,
                              operation,
                              parameters,
                              weight_variance = NULL) {
  operator <- .ngeo_as_dgCMatrix(operator)
  source_support <- .ngeo_builder_support(source, source_support)
  history <- list(operations = list(.ngeo_operation(
    operation,
    parameters
  )))
  .ngeo_support_map_structure(
    operator = operator,
    type = type,
    source_hash = base_hash(source),
    target_hash = base_hash(target),
    source_id = source$base$elements$element_id,
    target_id = target$base$elements$element_id,
    source_support = source_support,
    target_support = as.numeric(operator %*% source_support),
    weight_variance = weight_variance,
    coverage = coverage,
    history = history
  )
}

.ngeo_surface_coordinates <- function(x, coordinates, argument) {
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      sprintf("`%s` must be an `ngeo_surface`.", argument),
      "ngeo_error_argument"
    )
  }
  coordinates <- if (identical(coordinates, "active")) {
    x$base$geometry$active_coordinates
  } else {
    coordinates
  }
  .ngeo_assert_scalar_character(coordinates, argument)
  if (!coordinates %in% names(x$base$geometry$coordinates)) {
    .ngeo_abort(
      sprintf("Unknown %s coordinate set `%s`.", argument, coordinates),
      "ngeo_error_geometry"
    )
  }
  value <- x$base$geometry$coordinates[[coordinates]]
  if (ncol(value) == 2L) {
    value <- cbind(value, 0)
  }
  list(name = coordinates, value = value)
}

.ngeo_query_nearest <- function(data, query) {
  if (!nrow(data)) {
    .ngeo_abort("No eligible target elements remain.", "ngeo_error_coverage")
  }
  if (nrow(data) == 1L) {
    difference <- sweep(query, 2L, data[1L, ], "-")
    return(list(
      index = rep.int(1L, nrow(query)),
      distance = sqrt(rowSums(difference^2)),
      engine = "single_target"
    ))
  }
  exact_pairs <- as.double(nrow(data)) * as.double(nrow(query))
  maximum <- getOption("neurogeo.max_exact_mapping_pairs", 2e6)
  if (exact_pairs > maximum) {
    .ngeo_require("dbscan", "scalable support-map construction")
    result <- dbscan::kNN(data, k = 1L, query = query, sort = TRUE)
    return(list(
      index = as.integer(result$id[, 1L]),
      distance = as.numeric(result$dist[, 1L]),
      engine = "dbscan"
    ))
  }
  block_size <- max(1L, floor(maximum / nrow(data)))
  index <- integer(nrow(query))
  distance <- numeric(nrow(query))
  starts <- seq.int(1L, nrow(query), by = block_size)
  for (start in starts) {
    rows <- seq.int(start, min(start + block_size - 1L, nrow(query)))
    squared <- matrix(0, nrow = length(rows), ncol = nrow(data))
    for (axis in seq_len(ncol(data))) {
      squared <- squared +
        outer(query[rows, axis], data[, axis], "-")^2
    }
    index[rows] <- max.col(-squared, ties.method = "first")
    distance[rows] <- sqrt(squared[cbind(
      seq_along(rows),
      index[rows]
    )])
  }
  list(index = index, distance = distance, engine = "exact_block")
}

#' Build a nearest-vertex map in a known surface registration
#'
#' The coordinate sets must already express a common registration. This
#' function does not estimate or repair a registration.
#'
#' @param source Source surface.
#' @param target Target surface.
#' @param source_coordinates Registered source coordinate-set name.
#' @param target_coordinates Registered target coordinate-set name.
#' @param registration Identifier for the known registration. It is required
#'   when the declared spaces differ or are unknown.
#' @param max_distance Optional maximum registered-coordinate distance.
#' @param source_support Optional positive source vertex support.
#'
#' @return A sparse crisp `ngeo_support_map`.
#' @export
ngeo_surface_nearest_map <- function(
    source,
    target,
    source_coordinates = "active",
    target_coordinates = "active",
    registration = NULL,
    max_distance = Inf,
    source_support = NULL) {
  ngeo_validate(source, "strict")
  ngeo_validate(target, "strict")
  registration <- .ngeo_builder_registration(
    source, target, registration
  )
  source_xyz <- .ngeo_surface_coordinates(
    source, source_coordinates, "source_coordinates"
  )
  target_xyz <- .ngeo_surface_coordinates(
    target, target_coordinates, "target_coordinates"
  )
  if (!is.numeric(max_distance) || length(max_distance) != 1L ||
      is.na(max_distance) || max_distance < 0) {
    .ngeo_abort(
      "`max_distance` must be one non-negative number.",
      "ngeo_error_argument"
    )
  }
  source_eligible <- which(source$base$geometry$mask)
  target_eligible <- which(target$base$geometry$mask)
  nearest <- .ngeo_query_nearest(
    target_xyz$value[target_eligible, , drop = FALSE],
    source_xyz$value[source_eligible, , drop = FALSE]
  )
  mapped <- nearest$distance <= max_distance
  source_column <- source_eligible[mapped]
  target_row <- target_eligible[nearest$index[mapped]]
  operator <- Matrix::sparseMatrix(
    i = target_row,
    j = source_column,
    x = 1,
    dims = c(
      nrow(target$base$elements),
      nrow(source$base$elements)
    )
  )
  complete <- length(source_column) == nrow(source$base$elements)
  .ngeo_builder_map(
    source,
    target,
    operator,
    type = "crisp",
    source_support = source_support,
    coverage = if (complete) "complete" else "partial",
    operation = "ngeo_surface_nearest_map",
    parameters = list(
      registration = registration,
      source_coordinates = source_xyz$name,
      target_coordinates = target_xyz$name,
      max_distance = max_distance,
      search_engine = nearest$engine,
      mapped = length(source_column),
      unmapped = nrow(source$base$elements) - length(source_column),
      distance_max = if (any(mapped)) max(nearest$distance[mapped]) else NA_real_
    )
  )
}

.ngeo_closest_triangle <- function(point, a, b, c, tolerance) {
  ab <- b - a
  ac <- c - a
  normal <- c(
    ab[[2L]] * ac[[3L]] - ab[[3L]] * ac[[2L]],
    ab[[3L]] * ac[[1L]] - ab[[1L]] * ac[[3L]],
    ab[[1L]] * ac[[2L]] - ab[[2L]] * ac[[1L]]
  )
  if (sum(normal^2) <= tolerance^2) {
    return(NULL)
  }
  ap <- point - a
  d1 <- sum(ab * ap)
  d2 <- sum(ac * ap)
  if (d1 <= 0 && d2 <= 0) {
    return(list(weight = c(1, 0, 0), point = a))
  }
  bp <- point - b
  d3 <- sum(ab * bp)
  d4 <- sum(ac * bp)
  if (d3 >= 0 && d4 <= d3) {
    return(list(weight = c(0, 1, 0), point = b))
  }
  vc <- d1 * d4 - d3 * d2
  if (vc <= 0 && d1 >= 0 && d3 <= 0) {
    v <- d1 / (d1 - d3)
    return(list(weight = c(1 - v, v, 0), point = a + v * ab))
  }
  cp <- point - c
  d5 <- sum(ab * cp)
  d6 <- sum(ac * cp)
  if (d6 >= 0 && d5 <= d6) {
    return(list(weight = c(0, 0, 1), point = c))
  }
  vb <- d5 * d2 - d1 * d6
  if (vb <= 0 && d2 >= 0 && d6 <= 0) {
    w <- d2 / (d2 - d6)
    return(list(weight = c(1 - w, 0, w), point = a + w * ac))
  }
  va <- d3 * d6 - d5 * d4
  if (va <= 0 && (d4 - d3) >= 0 && (d5 - d6) >= 0) {
    w <- (d4 - d3) / ((d4 - d3) + (d5 - d6))
    return(list(
      weight = c(0, 1 - w, w),
      point = b + w * (c - b)
    ))
  }
  denominator <- 1 / (va + vb + vc)
  v <- vb * denominator
  w <- vc * denominator
  list(
    weight = c(1 - v - w, v, w),
    point = a + ab * v + ac * w
  )
}

.ngeo_triangle_candidates <- function(centroid, query, candidate_faces) {
  candidate_faces <- min(as.integer(candidate_faces), nrow(centroid))
  exact_pairs <- as.double(nrow(centroid)) * as.double(nrow(query))
  maximum <- getOption("neurogeo.max_exact_mapping_pairs", 2e6)
  if (exact_pairs <= maximum || nrow(centroid) <= candidate_faces) {
    return(list(
      index = matrix(
        rep(seq_len(nrow(centroid)), each = nrow(query)),
        nrow = nrow(query)
      ),
      engine = "exact_all_faces"
    ))
  }
  .ngeo_require("dbscan", "scalable barycentric support-map construction")
  result <- dbscan::kNN(
    centroid,
    k = candidate_faces,
    query = query,
    sort = TRUE
  )
  index <- result$id
  if (is.null(dim(index))) {
    index <- matrix(index, ncol = 1L)
  }
  list(index = index, engine = "dbscan_candidate_faces")
}

#' Build a barycentric map in a known surface registration
#'
#' Each eligible source vertex is projected to the closest candidate target
#' triangle. Non-negative barycentric spatial_weights form one operator column.
#'
#' @inheritParams ngeo_surface_nearest_map
#' @param candidate_faces Number of centroid-nearest target faces evaluated
#'   for large meshes.
#' @param tolerance Geometric and sparse-weight tolerance.
#'
#' @return A sparse probabilistic `ngeo_support_map`.
#' @export
ngeo_surface_barycentric_map <- function(
    source,
    target,
    source_coordinates = "active",
    target_coordinates = "active",
    registration = NULL,
    max_distance = Inf,
    candidate_faces = 16L,
    tolerance = 1e-12,
    source_support = NULL) {
  ngeo_validate(source, "strict")
  ngeo_validate(target, "strict")
  registration <- .ngeo_builder_registration(
    source, target, registration
  )
  if (!is.numeric(candidate_faces) || length(candidate_faces) != 1L ||
      is.na(candidate_faces) || candidate_faces < 1 ||
      candidate_faces != floor(candidate_faces)) {
    .ngeo_abort(
      "`candidate_faces` must be one positive integer.",
      "ngeo_error_argument"
    )
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || tolerance <= 0 ||
      !is.numeric(max_distance) || length(max_distance) != 1L ||
      is.na(max_distance) || max_distance < 0) {
    .ngeo_abort(
      "`tolerance` must be positive and `max_distance` non-negative.",
      "ngeo_error_argument"
    )
  }
  source_xyz <- .ngeo_surface_coordinates(
    source, source_coordinates, "source_coordinates"
  )
  target_xyz <- .ngeo_surface_coordinates(
    target, target_coordinates, "target_coordinates"
  )
  faces <- target$base$geometry$faces
  face_keep <- apply(
    matrix(target$base$geometry$mask[faces], ncol = 3L),
    1L,
    all
  )
  faces <- faces[face_keep, , drop = FALSE]
  if (!nrow(faces)) {
    .ngeo_abort(
      "No target triangle remains after applying the target mask.",
      "ngeo_error_coverage"
    )
  }
  xyz <- target_xyz$value
  centroid <- (
    xyz[faces[, 1L], , drop = FALSE] +
      xyz[faces[, 2L], , drop = FALSE] +
      xyz[faces[, 3L], , drop = FALSE]
  ) / 3
  source_eligible <- which(source$base$geometry$mask)
  query <- source_xyz$value[source_eligible, , drop = FALSE]
  candidates <- .ngeo_triangle_candidates(
    centroid, query, candidate_faces
  )
  rows <- integer()
  columns <- integer()
  spatial_weights <- numeric()
  distances <- rep.int(Inf, length(source_eligible))
  for (q in seq_along(source_eligible)) {
    best <- NULL
    best_face <- NA_integer_
    best_distance <- Inf
    for (face_index in unique(candidates$index[q, ])) {
      vertex <- faces[face_index, ]
      current <- .ngeo_closest_triangle(
        query[q, ],
        xyz[vertex[[1L]], ],
        xyz[vertex[[2L]], ],
        xyz[vertex[[3L]], ],
        tolerance
      )
      if (is.null(current)) {
        next
      }
      distance <- sqrt(sum((query[q, ] - current$point)^2))
      if (distance < best_distance) {
        best <- current
        best_face <- face_index
        best_distance <- distance
      }
    }
    if (is.null(best) || best_distance > max_distance) {
      next
    }
    weight <- pmax(0, best$weight)
    weight <- weight / sum(weight)
    keep <- weight > tolerance
    rows <- c(rows, faces[best_face, keep])
    columns <- c(
      columns,
      rep.int(source_eligible[[q]], sum(keep))
    )
    spatial_weights <- c(spatial_weights, weight[keep])
    distances[[q]] <- best_distance
  }
  operator <- Matrix::sparseMatrix(
    i = rows,
    j = columns,
    x = spatial_weights,
    dims = c(
      nrow(target$base$elements),
      nrow(source$base$elements)
    )
  )
  mapped <- Matrix::colSums(operator) > tolerance
  complete <- all(mapped)
  .ngeo_builder_map(
    source,
    target,
    operator,
    type = "probabilistic",
    source_support = source_support,
    coverage = if (complete) "complete" else "partial",
    operation = "ngeo_surface_barycentric_map",
    parameters = list(
      registration = registration,
      source_coordinates = source_xyz$name,
      target_coordinates = target_xyz$name,
      max_distance = max_distance,
      candidate_faces = as.integer(candidate_faces),
      tolerance = tolerance,
      search_engine = candidates$engine,
      mapped = sum(mapped),
      unmapped = sum(!mapped),
      distance_max = if (any(is.finite(distances))) {
        max(distances[is.finite(distances)])
      } else {
        NA_real_
      }
    )
  )
}

#' Build a support map from a declared surface registration
#'
#' @inheritParams ngeo_surface_barycentric_map
#' @param method Nearest vertex or barycentric projection.
#'
#' @return A sparse `ngeo_support_map`.
#' @export
ngeo_surface_registration_map <- function(
    source,
    target,
    method = c("barycentric", "nearest"),
    source_coordinates = "active",
    target_coordinates = "active",
    registration = NULL,
    max_distance = Inf,
    candidate_faces = 16L,
    tolerance = 1e-12,
    source_support = NULL) {
  method <- match.arg(method)
  if (identical(method, "nearest")) {
    return(ngeo_surface_nearest_map(
      source,
      target,
      source_coordinates,
      target_coordinates,
      registration,
      max_distance,
      source_support
    ))
  }
  ngeo_surface_barycentric_map(
    source,
    target,
    source_coordinates,
    target_coordinates,
    registration,
    max_distance,
    candidate_faces,
    tolerance,
    source_support
  )
}

.ngeo_volume_registration <- function(source, target, registration) {
  if (!inherits(source, "ngeo_volume") ||
      !inherits(target, "ngeo_volume")) {
    .ngeo_abort(
      "Volume support mapping requires two `ngeo_volume` objects.",
      "ngeo_error_argument"
    )
  }
  .ngeo_builder_registration(source, target, registration)
}

.ngeo_voxel_world <- function(x) {
  index <- x$base$geometry$source_voxel_index
  homogeneous <- cbind(index, 1)
  world <- homogeneous %*% t(x$base$geometry$affine)
  world[, 1:3, drop = FALSE]
}

.ngeo_target_fractional_index <- function(target, world) {
  homogeneous <- cbind(world, 1)
  index <- homogeneous %*% t(solve(target$base$geometry$affine))
  index[, 1:3, drop = FALSE]
}

.ngeo_internal_voxel_index <- function(x, source_index) {
  source_index - x$base$geometry$source_index_base + 1
}

.ngeo_linear_voxel <- function(index, dim) {
  index[, 1L] +
    (index[, 2L] - 1L) * dim[[1L]] +
    (index[, 3L] - 1L) * dim[[1L]] * dim[[2L]]
}

.ngeo_active_voxel_rows <- function(x, internal_index) {
  inside <- apply(internal_index >= 1, 1L, all) &
    apply(
      sweep(internal_index, 2L, x$base$geometry$dim, "<="),
      1L,
      all
    )
  result <- rep.int(NA_integer_, nrow(internal_index))
  if (!any(inside)) {
    return(result)
  }
  target_linear <- .ngeo_linear_voxel(
    x$base$geometry$voxel_index,
    x$base$geometry$dim
  )
  query_linear <- .ngeo_linear_voxel(
    internal_index[inside, , drop = FALSE],
    x$base$geometry$dim
  )
  result[inside] <- match(query_linear, target_linear)
  result
}

#' Build a map between known affine voxel grids
#'
#' Source voxel centres are expressed in world coordinates by the source
#' affine and then located in the target affine grid.
#'
#' @param source Source volume.
#' @param target Target volume.
#' @param method Nearest-centre or trilinear allocation.
#' @param registration Known registration identifier when spaces differ.
#' @param outside Error, drop, or renormalize contributions outside the
#'   active target grid.
#' @param tolerance Numerical coverage tolerance.
#' @param source_support Optional positive source voxel support.
#'
#' @return A sparse crisp or probabilistic `ngeo_support_map`.
#' @export
ngeo_affine_grid_map <- function(
    source,
    target,
    method = c("nearest", "trilinear"),
    registration = NULL,
    outside = c("error", "drop", "normalize"),
    tolerance = 1e-10,
    source_support = NULL) {
  ngeo_validate(source, "strict")
  ngeo_validate(target, "strict")
  registration <- .ngeo_volume_registration(
    source, target, registration
  )
  method <- match.arg(method)
  outside <- match.arg(outside)
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || tolerance <= 0) {
    .ngeo_abort("`tolerance` must be positive.", "ngeo_error_argument")
  }
  world <- .ngeo_voxel_world(source)
  target_index <- .ngeo_target_fractional_index(target, world)
  n_source <- nrow(source$base$elements)
  n_target <- nrow(target$base$elements)
  if (identical(method, "nearest")) {
    internal <- .ngeo_internal_voxel_index(
      target, round(target_index)
    )
    target_row <- .ngeo_active_voxel_rows(target, internal)
    mapped <- !is.na(target_row)
    operator <- Matrix::sparseMatrix(
      i = target_row[mapped],
      j = which(mapped),
      x = 1,
      dims = c(n_target, n_source)
    )
    type <- "crisp"
  } else {
    lower <- floor(target_index)
    fraction <- target_index - lower
    target_rows <- integer()
    source_columns <- integer()
    contribution <- numeric()
    for (corner in 0:7) {
      high <- as.integer(intToBits(corner))[1:3]
      current_source <- sweep(lower, 2L, high, "+")
      current_internal <- .ngeo_internal_voxel_index(
        target, current_source
      )
      current_row <- .ngeo_active_voxel_rows(
        target, current_internal
      )
      weight <- rep.int(1, n_source)
      for (axis in seq_len(3L)) {
        weight <- weight * if (high[[axis]] == 1L) {
          fraction[, axis]
        } else {
          1 - fraction[, axis]
        }
      }
      keep <- !is.na(current_row) & weight > tolerance
      target_rows <- c(target_rows, current_row[keep])
      source_columns <- c(source_columns, which(keep))
      contribution <- c(contribution, weight[keep])
    }
    operator <- Matrix::sparseMatrix(
      i = target_rows,
      j = source_columns,
      x = contribution,
      dims = c(n_target, n_source)
    )
    type <- "probabilistic"
  }
  column_sum <- Matrix::colSums(operator)
  incomplete <- abs(column_sum - 1) > tolerance
  if (any(incomplete) && identical(outside, "error")) {
    .ngeo_abort(
      "The target grid or mask does not completely cover every source voxel.",
      "ngeo_error_coverage"
    )
  }
  if (identical(outside, "normalize") && any(column_sum > tolerance)) {
    inverse <- numeric(length(column_sum))
    inverse[column_sum > tolerance] <- 1 / column_sum[column_sum > tolerance]
    operator <- .ngeo_as_dgCMatrix(
      operator %*% Matrix::Diagonal(x = inverse)
    )
    column_sum <- Matrix::colSums(operator)
  }
  complete <- all(abs(column_sum - 1) <= tolerance)
  .ngeo_builder_map(
    source,
    target,
    operator,
    type = type,
    source_support = source_support,
    coverage = if (complete) "complete" else "partial",
    operation = "ngeo_affine_grid_map",
    parameters = list(
      method = method,
      registration = registration,
      outside = outside,
      tolerance = tolerance,
      source_affine = source$base$geometry$affine,
      target_affine = target$base$geometry$affine,
      mapped = sum(column_sum > tolerance),
      unmapped = sum(column_sum <= tolerance)
    )
  )
}

.ngeo_axis_aligned_affine <- function(affine, tolerance) {
  linear <- affine[1:3, 1:3, drop = FALSE]
  off_diagonal <- linear
  diag(off_diagonal) <- 0
  if (any(abs(off_diagonal) > tolerance)) {
    .ngeo_abort(
      paste(
        "Exact voxel overlap currently requires axis-aligned affines;",
        "rotation, shear, and axis permutation are unsupported."
      ),
      "ngeo_error_geometry"
    )
  }
  list(scale = diag(linear), translation = affine[1:3, 4L])
}

#' Build an exact overlap map for axis-aligned voxel grids
#'
#' @inheritParams ngeo_affine_grid_map
#' @param outside Error, drop, or normalize source overlap outside the active
#'   target grid.
#' @param max_contributions Maximum non-zero overlap entries.
#'
#' @return A sparse probabilistic `ngeo_support_map`.
#' @export
ngeo_voxel_overlap_map <- function(
    source,
    target,
    registration = NULL,
    outside = c("error", "drop", "normalize"),
    tolerance = 1e-10,
    max_contributions = getOption(
      "neurogeo.max_support_contributions", 1e7
    ),
    source_support = NULL) {
  ngeo_validate(source, "strict")
  ngeo_validate(target, "strict")
  registration <- .ngeo_volume_registration(
    source, target, registration
  )
  outside <- match.arg(outside)
  source_affine <- .ngeo_axis_aligned_affine(
    source$base$geometry$affine, tolerance
  )
  target_affine <- .ngeo_axis_aligned_affine(
    target$base$geometry$affine, tolerance
  )
  if (!is.numeric(max_contributions) ||
      length(max_contributions) != 1L ||
      is.na(max_contributions) || max_contributions < 1) {
    .ngeo_abort(
      "`max_contributions` must be one positive number.",
      "ngeo_error_argument"
    )
  }
  source_index <- source$base$geometry$source_voxel_index
  target_index <- target$base$geometry$source_voxel_index
  source_center <- sweep(
    sweep(source_index, 2L, source_affine$scale, "*"),
    2L,
    source_affine$translation,
    "+"
  )
  target_center <- sweep(
    sweep(target_index, 2L, target_affine$scale, "*"),
    2L,
    target_affine$translation,
    "+"
  )
  source_half <- abs(source_affine$scale) / 2
  target_half <- abs(target_affine$scale) / 2
  source_volume <- prod(2 * source_half)
  rows <- integer()
  columns <- integer()
  values <- numeric()
  for (source_row in seq_len(nrow(source_index))) {
    candidates <- rep.int(TRUE, nrow(target_index))
    overlap_axis <- vector("list", 3L)
    for (axis in seq_len(3L)) {
      overlap_axis[[axis]] <- pmax(
        0,
        pmin(
          source_center[source_row, axis] + source_half[[axis]],
          target_center[, axis] + target_half[[axis]]
        ) -
          pmax(
            source_center[source_row, axis] - source_half[[axis]],
            target_center[, axis] - target_half[[axis]]
          )
      )
      candidates <- candidates & overlap_axis[[axis]] > tolerance
    }
    target_rows <- which(candidates)
    if (!length(target_rows)) {
      next
    }
    overlap <- overlap_axis[[1L]][target_rows] *
      overlap_axis[[2L]][target_rows] *
      overlap_axis[[3L]][target_rows]
    rows <- c(rows, target_rows)
    columns <- c(columns, rep.int(source_row, length(target_rows)))
    values <- c(values, overlap / source_volume)
    if (length(values) > max_contributions) {
      .ngeo_abort(
        "Voxel overlap exceeds the configured sparse contribution limit.",
        "ngeo_error_resource"
      )
    }
  }
  operator <- Matrix::sparseMatrix(
    i = rows,
    j = columns,
    x = values,
    dims = c(nrow(target_index), nrow(source_index))
  )
  column_sum <- Matrix::colSums(operator)
  incomplete <- abs(column_sum - 1) > tolerance
  if (any(incomplete) && identical(outside, "error")) {
    .ngeo_abort(
      "The target grid does not completely overlap every source voxel.",
      "ngeo_error_coverage"
    )
  }
  if (identical(outside, "normalize") && any(column_sum > tolerance)) {
    inverse <- numeric(length(column_sum))
    inverse[column_sum > tolerance] <- 1 / column_sum[column_sum > tolerance]
    operator <- .ngeo_as_dgCMatrix(
      operator %*% Matrix::Diagonal(x = inverse)
    )
    column_sum <- Matrix::colSums(operator)
  }
  complete <- all(abs(column_sum - 1) <= tolerance)
  .ngeo_builder_map(
    source,
    target,
    operator,
    type = "probabilistic",
    source_support = source_support,
    coverage = if (complete) "complete" else "partial",
    operation = "ngeo_voxel_overlap_map",
    parameters = list(
      method = "exact_axis_aligned_overlap",
      registration = registration,
      outside = outside,
      tolerance = tolerance,
      source_affine = source$base$geometry$affine,
      target_affine = target$base$geometry$affine,
      mapped = sum(column_sum > tolerance),
      unmapped = sum(column_sum <= tolerance)
    )
  )
}

.ngeo_atlas_target <- function(source, region_id) {
  ngeo_parcellation(
    data.frame(region_id = region_id, stringsAsFactors = FALSE),
    source_base = source,
    support_size = rep.int(NA_real_, length(region_id)),
    coordinate_space = source$base$coordinate_space
  )
}

#' Build a crisp support map from aligned atlas labels
#'
#' @param source Source dataset.
#' @param labels One hard label per source element.
#' @param target Optional parcellation target. When omitted, a target template is
#'   constructed and stored in `map$target`.
#' @param exclude Labels to leave unmapped.
#' @param source_support Optional positive source support.
#'
#' @return A sparse crisp `ngeo_support_map`.
#' @examples
#' source <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 5)),
#'   measures = ngeo_measure(support_behavior = "intensive")
#' )
#' atlas <- ngeo_atlas_map(
#'   source, c("A", "A", "B", "B"), source_support = rep(1, 4)
#' )
#' ngeo_support_diagnostics(atlas)
#' regional <- aggregate_to(source, atlas$target, atlas)
#' values(regional)
#' @export
ngeo_atlas_map <- function(
    source,
    labels,
    target = NULL,
    exclude = NA,
    source_support = NULL) {
  ngeo_validate(source, "strict")
  if (length(labels) != nrow(source$base$elements) ||
      !(is.atomic(labels) || is.factor(labels))) {
    .ngeo_abort(
      "`labels` must be one atomic value per source element.",
      "ngeo_error_alignment"
    )
  }
  labels <- as.character(labels)
  excluded <- is.na(labels)
  if (!all(is.na(exclude))) {
    excluded <- excluded | labels %in% as.character(exclude)
  }
  region_id <- unique(labels[!excluded])
  if (!length(region_id)) {
    .ngeo_abort("No atlas region remains.", "ngeo_error_coverage")
  }
  support <- .ngeo_builder_support(source, source_support)
  constructed_target <- is.null(target)
  if (constructed_target) {
    target <- .ngeo_atlas_target(source, region_id)
  } else {
    ngeo_validate(target, "strict")
    if (!inherits(target, "ngeo_parcellation")) {
      .ngeo_abort(
        "`target` must be an `ngeo_parcellation` template.",
        "ngeo_error_argument"
      )
    }
    target_id <- as.character(target$base$elements$region_id)
    if (any(!region_id %in% target_id)) {
      .ngeo_abort(
        "Atlas labels contain identifiers absent from `target`.",
        "ngeo_error_alignment"
      )
    }
    region_id <- target_id
  }
  target_row <- match(labels, region_id)
  mapped <- !excluded & !is.na(target_row)
  operator <- Matrix::sparseMatrix(
    i = target_row[mapped],
    j = which(mapped),
    x = 1,
    dims = c(length(region_id), length(labels))
  )
  map <- .ngeo_builder_map(
    source,
    target,
    operator,
    type = "crisp",
    source_support = support,
    coverage = if (all(mapped)) "complete" else "partial",
    operation = "ngeo_atlas_map",
    parameters = list(
      parcellation = region_id,
      excluded = sum(!mapped),
      target_constructed = constructed_target
    )
  )
  if (constructed_target) {
    target$base$geometry$support_size <- map$target_support
    map$target_base_hash <- base_hash(target)
    map$target_element_id <- target$base$elements$element_id
    map$target <- target
    ngeo_validate_support_map(map)
  }
  map
}

#' Build a support map from aligned probabilistic atlas memberships
#'
#' @param source Source dataset.
#' @param probabilities Source-element by region non-negative membership
#'   matrix.
#' @param region_id Region identifiers, defaulting to matrix column names.
#' @param target Optional parcellation target. A target is stored in `map$target`
#'   when omitted.
#' @param coverage Complete, partial, or infer from column sums.
#' @param tolerance Numerical membership tolerance.
#' @param source_support Optional positive source support.
#'
#' @return A sparse probabilistic or overlapping `ngeo_support_map`.
#' @examples
#' source <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE)
#' )
#' probability <- matrix(
#'   c(1, 0, 0.5, 0.5, 0, 1),
#'   nrow = 3, byrow = TRUE,
#'   dimnames = list(NULL, c("A", "B"))
#' )
#' atlas <- ngeo_probabilistic_atlas_map(
#'   source, probability, source_support = rep(1, 3)
#' )
#' atlas
#' ngeo_support_diagnostics(atlas)
#' @export
ngeo_probabilistic_atlas_map <- function(
    source,
    probabilities,
    region_id = colnames(probabilities),
    target = NULL,
    coverage = c("auto", "complete", "partial"),
    tolerance = 1e-10,
    source_support = NULL) {
  ngeo_validate(source, "strict")
  coverage <- match.arg(coverage)
  if (!(is.matrix(probabilities) || inherits(probabilities, "Matrix")) ||
      nrow(probabilities) != nrow(source$base$elements)) {
    .ngeo_abort(
      "`probabilities` must be source-element by region.",
      "ngeo_error_alignment"
    )
  }
  probabilities <- .ngeo_as_dgCMatrix(probabilities)
  if (length(probabilities@x) &&
      (any(!is.finite(probabilities@x)) ||
        any(probabilities@x < 0))) {
    .ngeo_abort(
      "Atlas memberships must be finite and non-negative.",
      "ngeo_error_support_map"
    )
  }
  if (is.null(region_id)) {
    region_id <- as.character(seq_len(ncol(probabilities)))
  }
  region_id <- as.character(region_id)
  if (length(region_id) != ncol(probabilities) ||
      anyNA(region_id) || any(!nzchar(region_id)) ||
      anyDuplicated(region_id)) {
    .ngeo_abort(
      "`region_id` must uniquely identify probability columns.",
      "ngeo_error_alignment"
    )
  }
  source_sum <- Matrix::rowSums(probabilities)
  type <- if (any(source_sum > 1 + tolerance)) {
    "overlapping"
  } else {
    "probabilistic"
  }
  inferred_complete <- if (identical(type, "overlapping")) {
    all(source_sum > tolerance)
  } else {
    all(abs(source_sum - 1) <= tolerance)
  }
  coverage <- if (identical(coverage, "auto")) {
    if (inferred_complete) "complete" else "partial"
  } else {
    coverage
  }
  support <- .ngeo_builder_support(source, source_support)
  constructed_target <- is.null(target)
  if (constructed_target) {
    target <- .ngeo_atlas_target(source, region_id)
  } else {
    ngeo_validate(target, "strict")
    if (!inherits(target, "ngeo_parcellation") ||
        !identical(
          as.character(target$base$elements$region_id),
          region_id
        )) {
      .ngeo_abort(
        "`target` region order must match `region_id`.",
        "ngeo_error_alignment"
      )
    }
  }
  operator <- .ngeo_as_dgCMatrix(Matrix::t(probabilities))
  map <- .ngeo_builder_map(
    source,
    target,
    operator,
    type = type,
    source_support = support,
    coverage = coverage,
    operation = "ngeo_probabilistic_atlas_map",
    parameters = list(
      parcellation = region_id,
      tolerance = tolerance,
      target_constructed = constructed_target
    )
  )
  if (constructed_target) {
    target$base$geometry$support_size <- map$target_support
    map$target_base_hash <- base_hash(target)
    map$target_element_id <- target$base$elements$element_id
    map$target <- target
    ngeo_validate_support_map(map)
  }
  map
}

#' Build a label support map
#'
#' This is an explicit alias for `ngeo_atlas_map()` used by volume and
#' standard-format label workflows.
#'
#' @inheritParams ngeo_atlas_map
#' @return A sparse crisp `ngeo_support_map`.
#' @export
ngeo_label_overlap_map <- function(
    source,
    labels,
    target = NULL,
    exclude = NA,
    source_support = NULL) {
  ngeo_atlas_map(source, labels, target, exclude, source_support)
}
