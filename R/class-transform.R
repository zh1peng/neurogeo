#' Record a known spatial transform
#'
#' This constructor records an existing transform. It does not estimate
#' registration.
#'
#' @param source_space Source `ngeo_coordinate_space`.
#' @param target_space Target `ngeo_coordinate_space`.
#' @param type Transform type.
#' @param direction Transform direction.
#' @param method Method or software identifier.
#' @param interpolation Interpolation or resampling rule.
#' @param jacobian_available Whether a Jacobian is available.
#' @param source Optional source file or identifier.
#' @param parameters Transform parameters.
#'
#' @return An `ngeo_transform` object.
#' @templateVar example_call ngeo_transform(source_space, target_space, affine = affine_matrix)
#' @template stable-transform-core
#' @export
ngeo_transform <- function(source_space,
                           target_space,
                           type,
                           direction = "source_to_target",
                           method = "unknown",
                           interpolation = "none",
                           jacobian_available = FALSE,
                           source = NULL,
                           parameters = list()) {
  if (!inherits(source_space, "ngeo_coordinate_space") ||
      !inherits(target_space, "ngeo_coordinate_space")) {
    .ngeo_abort(
      "`source_space` and `target_space` must be `ngeo_coordinate_space` objects.",
      "ngeo_error_coordinate_space"
    )
  }
  .ngeo_assert_scalar_character(type, "type")
  .ngeo_assert_scalar_character(direction, "direction")
  .ngeo_assert_scalar_character(method, "method")
  .ngeo_assert_scalar_character(interpolation, "interpolation")
  if (!is.logical(jacobian_available) ||
      length(jacobian_available) != 1L ||
      is.na(jacobian_available)) {
    .ngeo_abort(
      "`jacobian_available` must be TRUE or FALSE.",
      "ngeo_error_argument"
    )
  }
  if (!is.list(parameters)) {
    .ngeo_abort("`parameters` must be a list.", "ngeo_error_argument")
  }
  if (identical(type, "affine")) {
    matrix <- parameters$matrix
    if (!is.matrix(matrix) || !is.numeric(matrix) ||
        !identical(dim(matrix), c(4L, 4L)) ||
        anyNA(matrix) || any(!is.finite(matrix)) ||
        !isTRUE(all.equal(
          matrix[4L, ],
          c(0, 0, 0, 1),
          tolerance = sqrt(.Machine$double.eps),
          check.attributes = FALSE
        ))) {
      .ngeo_abort(
        "An affine transform requires a finite 4 by 4 `parameters$matrix`.",
        "ngeo_error_transform"
      )
    }
    if (abs(det(matrix)) < sqrt(.Machine$double.eps)) {
      .ngeo_abort(
        "The affine transform matrix must be invertible.",
        "ngeo_error_transform"
      )
    }
  }

  base::structure(
    list(
      source_space = source_space,
      target_space = target_space,
      type = type,
      direction = direction,
      method = method,
      interpolation = interpolation,
      jacobian_available = jacobian_available,
      source = source,
      parameters = parameters
    ),
    class = "ngeo_transform"
  )
}

.ngeo_coordinate_space_signature <- function(coordinate_space) {
  coordinate_space[c(
    "space_id", "kind", "unit", "structure",
    "template", "density", "resolution"
  )]
}

.ngeo_coordinate_spaces_match <- function(left, right) {
  identical(
    .ngeo_coordinate_space_signature(left),
    .ngeo_coordinate_space_signature(right)
  )
}

#' Validate a known transform
#'
#' @param transform An `ngeo_transform`.
#'
#' @return `transform`, invisibly.
#' @templateVar example_call ngeo_validate_transform(transform)
#' @template stable-transform-core
#' @export
ngeo_validate_transform <- function(transform) {
  if (!inherits(transform, "ngeo_transform")) {
    .ngeo_abort(
      "`transform` must be an `ngeo_transform` object.",
      "ngeo_error_transform"
    )
  }
  if (!transform$direction %in% c(
    "source_to_target", "target_to_source"
  )) {
    .ngeo_abort(
      "Transform direction must be source_to_target or target_to_source.",
      "ngeo_error_transform"
    )
  }
  if (!identical(transform$type, "affine")) {
    .ngeo_abort(
      sprintf(
        "Transform type `%s` is recorded but cannot be applied by neurogeo.",
        transform$type
      ),
      "ngeo_error_transform_type"
    )
  }
  matrix <- transform$parameters$matrix
  if (!is.matrix(matrix) || !is.numeric(matrix) ||
      !identical(dim(matrix), c(4L, 4L)) ||
      anyNA(matrix) || any(!is.finite(matrix)) ||
      !isTRUE(all.equal(
        matrix[4L, ],
        c(0, 0, 0, 1),
        tolerance = sqrt(.Machine$double.eps),
        check.attributes = FALSE
      )) ||
      abs(det(matrix)) < sqrt(.Machine$double.eps)) {
    .ngeo_abort(
      "Affine transform parameters are invalid.",
      "ngeo_error_transform"
    )
  }
  invisible(transform)
}

.ngeo_affine_coordinates <- function(coordinates, matrix) {
  dimension <- ncol(coordinates)
  xyz <- if (dimension == 2L) {
    cbind(coordinates, 0)
  } else {
    coordinates
  }
  result <- cbind(xyz, 1) %*% t(matrix)
  result <- result[, 1:3, drop = FALSE] / result[, 4L]
  if (dimension == 2L) result[, 1:2, drop = FALSE] else result
}

.ngeo_apply_affine_base <- function(base, matrix, target_space) {
  switch(
    base$type,
    point = {
      base$geometry$coordinates <- .ngeo_affine_coordinates(
        base$geometry$coordinates,
        matrix
      )
    },
    surface = {
      selected <- base$geometry$coordinate_meta$name[
        base$geometry$coordinate_meta$metric_eligible
      ]
      if (!length(selected)) {
        .ngeo_abort(
          "Surface transform requires metric-eligible coordinates.",
          "ngeo_error_capability"
        )
      }
      for (name in selected) {
        base$geometry$coordinates[[name]] <- .ngeo_affine_coordinates(
          base$geometry$coordinates[[name]],
          matrix
        )
      }
    },
    volume = {
      base$geometry$affine <- matrix %*% base$geometry$affine
      if (!is.null(base$geometry$header_transforms)) {
        base$geometry$header_transforms <- NULL
      }
    },
    parcellation = {
      if (is.null(base$geometry$centroid)) {
        .ngeo_abort(
          "Region transform requires explicit centroids.",
          "ngeo_error_capability"
        )
      }
      base$geometry$centroid <- .ngeo_affine_coordinates(
        base$geometry$centroid,
        matrix
      )
    },
    grayordinate = {
      for (i in seq_along(base$geometry$components)) {
        component <- base$geometry$components[[i]]
        if (identical(component$kind, "volume")) {
          component$affine <- matrix %*% component$affine
        } else if (!is.null(component$geometry)) {
          geometry <- component$geometry
          geometry$base <- .ngeo_apply_affine_base(
            geometry$base,
            matrix,
            geometry$base$coordinate_space
          )
          component$geometry <- geometry
        }
        base$geometry$components[[i]] <- component
      }
    }
  )
  base$coordinate_space <- target_space
  base
}

#' Apply a known affine transform without resampling
#'
#' The operation changes geometry only. Element order, topology, values,
#' layers, and measurement semantics are preserved exactly.
#'
#' @param x An `ngeo` dataset.
#' @param transform A source-to-target affine `ngeo_transform`.
#'
#' @return A transformed copy of `x`.
#' @templateVar example_call ngeo_apply_transform(data, transform, authorize = TRUE)
#' @template stable-transform-core
#' @export
ngeo_apply_transform <- function(x, transform) {
  ngeo_validate(x, "strict")
  ngeo_validate_transform(transform)
  if (!identical(transform$direction, "source_to_target")) {
    .ngeo_abort(
      "Apply a target-to-source transform only after `ngeo_invert_transform()`.",
      "ngeo_error_transform_direction"
    )
  }
  if (!.ngeo_coordinate_spaces_match(
    x$base$coordinate_space,
    transform$source_space
  )) {
    .ngeo_abort(
      "Transform source coordinate_space does not match the dataset coordinate_space.",
      "ngeo_error_coordinate_space_mismatch"
    )
  }
  result <- x
  source_hash <- base_hash(x)
  result$base <- .ngeo_apply_affine_base(
    result$base,
    transform$parameters$matrix,
    transform$target_space
  )
  result$history$operations <- c(
    result$history$operations %||% list(),
    list(.ngeo_operation(
      "ngeo_apply_transform",
      list(
        type = transform$type,
        method = transform$method,
        source_space = transform$source_space$space_id,
        target_space = transform$target_space$space_id,
        source_base_hash = source_hash
      )
    ))
  )
  ngeo_validate(result, "strict")
  result
}

#' Compose known affine transforms
#'
#' @param first Source-to-intermediate transform.
#' @param second Intermediate-to-target transform.
#'
#' @return A composed source-to-target `ngeo_transform`.
#' @templateVar example_call ngeo_compose_transform(source_to_mid, mid_to_target)
#' @template stable-transform-core
#' @export
ngeo_compose_transform <- function(first, second) {
  ngeo_validate_transform(first)
  ngeo_validate_transform(second)
  if (!identical(first$direction, "source_to_target") ||
      !identical(second$direction, "source_to_target")) {
    .ngeo_abort(
      "Only source-to-target transforms can be composed.",
      "ngeo_error_transform_direction"
    )
  }
  if (!.ngeo_coordinate_spaces_match(first$target_space, second$source_space)) {
    .ngeo_abort(
      "The first target coordinate_space must match the second source coordinate_space.",
      "ngeo_error_coordinate_space_mismatch"
    )
  }
  ngeo_transform(
    source_space = first$source_space,
    target_space = second$target_space,
    type = "affine",
    method = paste(first$method, second$method, sep = " -> "),
    interpolation = "none",
    jacobian_available = first$jacobian_available &&
      second$jacobian_available,
    parameters = list(
      matrix = second$parameters$matrix %*% first$parameters$matrix,
      composed = TRUE
    )
  )
}

#' Invert a known affine transform
#'
#' @param transform A valid affine `ngeo_transform`.
#'
#' @return An inverse source-to-target `ngeo_transform`.
#' @templateVar example_call ngeo_invert_transform(transform)
#' @template stable-transform-core
#' @export
ngeo_invert_transform <- function(transform) {
  ngeo_validate_transform(transform)
  ngeo_transform(
    source_space = transform$target_space,
    target_space = transform$source_space,
    type = "affine",
    method = paste("inverse of", transform$method),
    interpolation = "none",
    jacobian_available = transform$jacobian_available,
    source = transform$source,
    parameters = list(
      matrix = solve(transform$parameters$matrix),
      inverted = TRUE
    )
  )
}
