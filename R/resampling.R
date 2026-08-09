# Explicit, authorized resampling.
.ngeo_resampling_methods <- function(source, target) {
  if (inherits(source, "ngeo_surface") &&
      inherits(target, "ngeo_surface")) {
    return(c("nearest", "barycentric"))
  }
  if (inherits(source, "ngeo_volume") &&
      inherits(target, "ngeo_volume")) {
    return(c("nearest", "linear", "overlap"))
  }
  character()
}

.ngeo_resampling_parameter_names <- function(method, base_type) {
  if (identical(base_type, "surface")) {
    common <- c(
      "source_coordinates", "target_coordinates", "max_distance"
    )
    if (identical(method, "barycentric")) {
      c(common, "candidate_faces", "tolerance")
    } else {
      common
    }
  } else if (identical(method, "overlap")) {
    c("tolerance", "max_contributions")
  } else {
    "tolerance"
  }
}

.ngeo_resampling_identity <- function(x) {
  list(
    schema = "NGCS-resampling-plan-1",
    source_base_hash = x$source_base_hash,
    target_base_hash = x$target_base_hash,
    path_hash = x$path$path_hash,
    composed_transform_hash = if (is.null(x$path$composed)) {
      NULL
    } else {
      ngeo_transform_hash(x$path$composed)
    },
    method = x$method,
    policies = x$policies,
    source_support = x$source_support,
    weight_variance = x$weight_variance,
    parameters = x$parameters,
    budget = unclass(x$budget)
  )
}

.ngeo_validate_resampling_path <- function(source, target, path) {
  if (!inherits(path, "ngeo_transform_path")) {
    .ngeo_abort(
      "`path` must be an explicitly selected `ngeo_transform_path`.",
      "ngeo_error_transform_path"
    )
  }
  if (!identical(
    ngeo_coordinate_space_hash(source$base$coordinate_space),
    ngeo_coordinate_space_hash(path$from)
  ) || !identical(
    ngeo_coordinate_space_hash(target$base$coordinate_space),
    ngeo_coordinate_space_hash(path$to)
  )) {
    .ngeo_abort(
      "Transform-path endpoints do not match the exact source and target spaces.",
      "ngeo_error_coordinate_space_mismatch"
    )
  }
  if (any(path$lossy) || !isTRUE(path$applicable) ||
      is.null(path$composed) ||
      !identical(path$composed$type, "affine")) {
    .ngeo_abort(
      paste(
        "Resampling requires a supplied, non-lossy affine-applicable path;",
        "lossy and non-affine paths are unsupported."
      ),
      "ngeo_error_resampling_path"
    )
  }
  expected_hash <- digest::digest(
    path[c(
      "graph_hash", "tokens", "edge_hashes", "reversed", "lossy"
    )],
    algo = "sha256"
  )
  if (!identical(path$path_hash, expected_hash)) {
    .ngeo_abort(
      "Transform-path identity changed after selection.",
      "ngeo_error_transform_path_mutation"
    )
  }
  recomposed <- if (!length(path$transforms)) {
    ngeo_transform(
      path$from,
      path$to,
      "affine",
      method = "identity path",
      parameters = list(matrix = diag(4))
    )
  } else {
    Reduce(ngeo_compose_transform, path$transforms)
  }
  forward <- !path$reversed
  if (!identical(
        ngeo_transform_hash(recomposed),
        ngeo_transform_hash(path$composed)
      ) ||
      (any(forward) && !identical(
        unname(vapply(
          path$transforms[forward],
          ngeo_transform_hash,
          character(1)
        )),
        unname(path$edge_hashes[forward])
      ))) {
    .ngeo_abort(
      "Transform-path contents changed after graph selection.",
      "ngeo_error_transform_path_mutation"
    )
  }
  invisible(path)
}

#' Create an explicitly authorized transform-aware resampling plan
#'
#' The plan consumes an already-supplied transform path. It never estimates
#' registration, resolves path ambiguity, or resamples until a later call
#' explicitly sets `authorize = TRUE`.
#'
#' @param source,target Source data and target-base template.
#' @param path An explicitly selected `ngeo_transform_path` from the exact
#' source coordinate_space to the exact target coordinate_space.
#' @param method Nearest, volume linear/trilinear, conservative surface
#'   source-scatter barycentric, or exact axis-aligned overlap mapping.
#' @param coverage Geometric coverage policy: reject, retain partial
#' contributions, or normalize covered contributions.
#' @param conservation For extensive/count layers, reject non-unit allocation
#' or explicitly normalize it.
#' @param missing Reject or explicitly drop wholly unmapped source support.
#' @param unknown Interpretation of unknown measurement semantics.
#' @param uncertainty No variance output, value variance only, or value and
#' mapping-weight variance.
#' @param source_support Optional positive source support sizes.
#' @param weight_variance Optional target-by-source independent mapping-weight
#' variances, required by `uncertainty = "value_and_mapping"`.
#' @param parameters Named method-specific builder parameters.
#' @param budget Resource limits for sparse mapping and materialized results.
#' @return An immutable-identity `ngeo_resampling_plan`.
#' @templateVar example_call ngeo_resampling_plan(source_data, target_base, method = "nearest")
#' @template stable-neuroimaging-method
#' @export
ngeo_resampling_plan <- function(
    source,
    target,
    path,
    method = NULL,
    coverage = c("error", "drop", "normalize"),
    conservation = c("strict", "normalize"),
    missing = c("error", "drop"),
    unknown = c("error", "intensive", "extensive"),
    uncertainty = c("none", "value", "value_and_mapping"),
    source_support = NULL,
    weight_variance = NULL,
    parameters = list(),
    budget = ngeo_resource_budget()) {
  ngeo_validate(source, "strict")
  ngeo_validate(target, "strict")
  .ngeo_validate_resampling_path(source, target, path)
  methods <- .ngeo_resampling_methods(source, target)
  if (!length(methods)) {
    .ngeo_abort(
      "Resampling requires two surfaces or two volumes.",
      "ngeo_error_resampling_method"
    )
  }
  method <- method %||% methods[[1L]]
  if (!is.character(method) || length(method) != 1L ||
      !method %in% methods) {
    .ngeo_abort(
      sprintf(
        "Method is incompatible; available methods are: %s.",
        paste(methods, collapse = ", ")
      ),
      "ngeo_error_resampling_method"
    )
  }
  coverage <- match.arg(coverage)
  conservation <- match.arg(conservation)
  missing <- match.arg(missing)
  unknown <- match.arg(unknown)
  uncertainty <- match.arg(uncertainty)
  if (!is.list(parameters) ||
      (length(parameters) && (
        is.null(names(parameters)) ||
          any(!nzchar(names(parameters))) ||
          anyDuplicated(names(parameters))
      ))) {
    .ngeo_abort(
      "`parameters` must be a uniquely named list.",
      "ngeo_error_argument"
    )
  }
  base_type <- if (inherits(source, "ngeo_surface")) {
    "surface"
  } else {
    "volume"
  }
  allowed <- .ngeo_resampling_parameter_names(method, base_type)
  unknown_parameter <- setdiff(names(parameters), allowed)
  if (length(unknown_parameter)) {
    .ngeo_abort(
      sprintf(
        "Unsupported `%s` parameter(s): %s.",
        method, paste(unknown_parameter, collapse = ", ")
      ),
      "ngeo_error_resampling_method"
    )
  }
  if (!inherits(budget, "ngeo_resource_budget")) {
    .ngeo_abort(
      "`budget` must be an `ngeo_resource_budget`.",
      "ngeo_error_argument"
    )
  }
  if (!is.null(source_support)) {
    source_support <- .ngeo_support_vector(
      source, source_support, "source_support"
    )
  }
  if (!is.null(weight_variance)) {
    if (!(is.matrix(weight_variance) ||
          inherits(weight_variance, "Matrix")) ||
        !identical(
          dim(weight_variance),
          c(
            nrow(target$base$elements),
            nrow(source$base$elements)
          )
        )) {
      .ngeo_abort(
        "`weight_variance` must be target-by-source.",
        "ngeo_error_uncertainty"
      )
    }
    weight_variance <- .ngeo_as_dgCMatrix(weight_variance)
    if (length(weight_variance@x) &&
        (any(!is.finite(weight_variance@x)) ||
          any(weight_variance@x < 0))) {
      .ngeo_abort(
        "Mapping-weight variances must be finite and non-negative.",
        "ngeo_error_uncertainty"
      )
    }
  }
  if (identical(uncertainty, "value_and_mapping") &&
      is.null(weight_variance)) {
    .ngeo_abort(
      "Value-and-mapping uncertainty requires `weight_variance`.",
      "ngeo_error_uncertainty"
    )
  }
  if (!identical(uncertainty, "value_and_mapping") &&
      !is.null(weight_variance)) {
    .ngeo_abort(
      "Mapping-weight variance requires `uncertainty = \"value_and_mapping\"`.",
      "ngeo_error_uncertainty"
    )
  }
  result <- structure(
    list(
      source = source,
      target = target,
      source_base_hash = base_hash(source),
      target_base_hash = base_hash(target),
      path = path,
      method = method,
      policies = list(
        coverage = coverage,
        conservation = conservation,
        missing = missing,
        unknown = unknown,
        uncertainty = uncertainty
      ),
      source_support = source_support,
      weight_variance = weight_variance,
      parameters = parameters,
      budget = budget,
      plan_hash = NULL,
      specification = "NGCS 3.2"
    ),
    class = "ngeo_resampling_plan"
  )
  result$plan_hash <- digest::digest(
    .ngeo_resampling_identity(result), algo = "sha256"
  )
  ngeo_validate_resampling_plan(result)
  result
}

#' Validate an NGCS 3.2 resampling plan
#'
#' @param x An `ngeo_resampling_plan`.
#' @return `x`, invisibly.
#' @templateVar example_call ngeo_validate_resampling_plan(resampling_plan)
#' @template stable-neuroimaging-method
#' @export
ngeo_validate_resampling_plan <- function(x) {
  if (!inherits(x, "ngeo_resampling_plan")) {
    .ngeo_abort(
      "`x` must be an `ngeo_resampling_plan`.",
      "ngeo_error_resampling_plan"
    )
  }
  ngeo_validate(x$source, "strict")
  ngeo_validate(x$target, "strict")
  .ngeo_validate_resampling_path(x$source, x$target, x$path)
  methods <- .ngeo_resampling_methods(x$source, x$target)
  valid_policies <- is.list(x$policies) &&
    identical(
      names(x$policies),
      c(
        "coverage", "conservation", "missing", "unknown",
        "uncertainty"
      )
    ) &&
    x$policies$coverage %in% c("error", "drop", "normalize") &&
    x$policies$conservation %in% c("strict", "normalize") &&
    x$policies$missing %in% c("error", "drop") &&
    x$policies$unknown %in% c("error", "intensive", "extensive") &&
    x$policies$uncertainty %in%
      c("none", "value", "value_and_mapping")
  if (!x$method %in% methods || !valid_policies ||
      !inherits(x$budget, "ngeo_resource_budget") ||
      !identical(base_hash(x$source), x$source_base_hash) ||
      !identical(base_hash(x$target), x$target_base_hash)) {
    .ngeo_abort(
      "Resampling plan bases, method, policies, or budget changed.",
      "ngeo_error_resampling_plan_mutation"
    )
  }
  expected <- digest::digest(
    .ngeo_resampling_identity(x), algo = "sha256"
  )
  if (!identical(expected, x$plan_hash)) {
    .ngeo_abort(
      "Resampling plan identity changed after construction.",
      "ngeo_error_resampling_plan_mutation"
    )
  }
  invisible(x)
}

.ngeo_resampling_budget_map <- function(plan) {
  n_source <- nrow(plan$source$base$elements)
  multiplier <- switch(
    plan$method,
    nearest = 1,
    linear = 8,
    barycentric = 3,
    overlap = plan$parameters$max_contributions %||%
      min(64 * n_source, 1e7)
  )
  nonzero <- if (identical(plan$method, "overlap")) {
    multiplier
  } else {
    multiplier * n_source
  }
  .ngeo_budget_assert(
    plan$budget, "materialized_elements", nonzero
  )
  .ngeo_budget_assert(
    plan$budget, "memory_bytes", 24 * nonzero
  )
  .ngeo_budget_assert(plan$budget, "blocks", 1)
  nonzero
}

.ngeo_resampling_build <- function(plan, authorize) {
  ngeo_validate_resampling_plan(plan)
  if (!isTRUE(authorize)) {
    .ngeo_abort(
      "Resampling-map construction requires explicit `authorize = TRUE`.",
      "ngeo_error_authorization"
    )
  }
  maximum_nonzero <- .ngeo_resampling_budget_map(plan)
  transformed <- ngeo_apply_transform_path(
    plan$source, plan$path, authorize = TRUE
  )
  registration <- plan$path$path_hash
  arguments <- plan$parameters
  if (inherits(transformed, "ngeo_surface")) {
    common <- list(
      source = transformed,
      target = plan$target,
      registration = registration,
      source_support = plan$source_support
    )
    map <- if (identical(plan$method, "nearest")) {
      do.call(
        ngeo_surface_nearest_map,
        c(common, arguments)
      )
    } else {
      do.call(
        ngeo_surface_barycentric_map,
        c(common, arguments)
      )
    }
  } else {
    outside <- plan$policies$coverage
    common <- list(
      source = transformed,
      target = plan$target,
      registration = registration,
      outside = outside,
      source_support = plan$source_support
    )
    map <- if (identical(plan$method, "overlap")) {
      arguments$max_contributions <-
        arguments$max_contributions %||% maximum_nonzero
      do.call(
        ngeo_voxel_overlap_map,
        c(common, arguments)
      )
    } else {
      do.call(
        ngeo_affine_grid_map,
        c(
          common,
          list(method = if (
            identical(plan$method, "linear")
          ) "trilinear" else "nearest"),
          arguments
        )
      )
    }
  }
  column_sum <- Matrix::colSums(map$operator)
  tolerance <- plan$parameters$tolerance %||% 1e-10
  incomplete <- abs(column_sum - 1) > tolerance
  if (any(incomplete) &&
      identical(plan$policies$coverage, "error")) {
    .ngeo_abort(
      "The resampling support map does not completely cover the source.",
      "ngeo_error_coverage"
    )
  }
  if (identical(plan$policies$coverage, "normalize") &&
      any(column_sum > tolerance)) {
    inverse <- numeric(length(column_sum))
    inverse[column_sum > tolerance] <-
      1 / column_sum[column_sum > tolerance]
    map$operator <- .ngeo_as_dgCMatrix(
      map$operator %*% Matrix::Diagonal(x = inverse)
    )
    map$target_support <- if (is.null(map$source_support)) {
      NULL
    } else {
      as.numeric(map$operator %*% map$source_support)
    }
    column_sum <- Matrix::colSums(map$operator)
    map$coverage <- if (
      all(abs(column_sum - 1) <= tolerance)
    ) "complete" else "partial"
  }
  if (!is.null(plan$weight_variance)) {
    map$weight_variance <- plan$weight_variance
  }
  map$history$resampling <- list(
    schema = "NGCS-resampling-support-1",
    plan_hash = plan$plan_hash,
    path = ngeo_transform_path_history(plan$path),
    method = plan$method,
    policies = plan$policies,
    transformed_source_base_hash = base_hash(transformed),
    original_source_base_hash = plan$source_base_hash,
    target_base_hash = plan$target_base_hash,
    registration_estimated = FALSE,
    implicit_resampling = FALSE
  )
  ngeo_validate_support_map(map)
  list(source = transformed, map = map)
}

#' Build an authorized transform-aware sparse support map
#'
#' @param plan A validated `ngeo_resampling_plan`.
#' @param authorize Must be explicitly `TRUE`.
#' @return An `ngeo_support_map` carrying joint path/plan history.
#' @templateVar example_call ngeo_build_resampling_map(resampling_plan, authorize = TRUE)
#' @template stable-neuroimaging-method
#' @export
ngeo_build_resampling_map <- function(plan, authorize = FALSE) {
  .ngeo_resampling_build(plan, authorize)$map
}

#' Diagnose a transform-aware resampling map
#'
#' @param plan A validated `ngeo_resampling_plan`.
#' @param support_map A map created from the plan.
#' @param tolerance Numeric coverage tolerance.
#' @return An `ngeo_resampling_diagnostics`.
#' @templateVar example_call ngeo_resampling_diagnostics(resampling_plan)
#' @template stable-neuroimaging-method
#' @export
ngeo_resampling_diagnostics <- function(
    plan, support_map, tolerance = 1e-10) {
  ngeo_validate_resampling_plan(plan)
  ngeo_validate_support_map(support_map, tolerance)
  history <- support_map$history$resampling
  if (!is.list(history) ||
      !identical(history$plan_hash, plan$plan_hash) ||
      !identical(
        support_map$target_base_hash,
        plan$target_base_hash
      )) {
    .ngeo_abort(
      "Support map was not built from this resampling plan.",
      "ngeo_error_resampling_plan"
    )
  }
  column_sum <- Matrix::colSums(support_map$operator)
  row_sum <- Matrix::rowSums(support_map$operator)
  mapped <- column_sum > tolerance
  source_total <- if (is.null(support_map$source_support)) {
    NA_real_
  } else {
    sum(support_map$source_support)
  }
  target_total <- if (is.null(support_map$target_support)) {
    NA_real_
  } else {
    sum(support_map$target_support)
  }
  issues <- list()
  if (any(!mapped)) {
    issues[[length(issues) + 1L]] <- data.frame(
      severity = "warning",
      code = "PARTIAL_SOURCE_COVERAGE",
      count = sum(!mapped),
      stringsAsFactors = FALSE
    )
  }
  nonconservative <- mapped &
    abs(column_sum - 1) > tolerance
  if (any(nonconservative)) {
    issues[[length(issues) + 1L]] <- data.frame(
      severity = "warning",
      code = "NONCONSERVATIVE_SOURCE_ALLOCATION",
      count = sum(nonconservative),
      stringsAsFactors = FALSE
    )
  }
  if (any(row_sum <= tolerance)) {
    issues[[length(issues) + 1L]] <- data.frame(
      severity = "warning",
      code = "EMPTY_TARGET_SUPPORT",
      count = sum(row_sum <= tolerance),
      stringsAsFactors = FALSE
    )
  }
  issues <- if (length(issues)) {
    do.call(rbind, issues)
  } else {
    data.frame(
      severity = character(),
      code = character(),
      count = integer(),
      stringsAsFactors = FALSE
    )
  }
  result <- list(
    plan_hash = plan$plan_hash,
    path_hash = plan$path$path_hash,
    support_map_hash = ngeo_support_map_hash(support_map),
    joint_hash = digest::digest(
      list(
        plan = plan$plan_hash,
        path = plan$path$path_hash,
        support_map = ngeo_support_map_hash(support_map)
      ),
      algo = "sha256"
    ),
    method = plan$method,
    policies = plan$policies,
    source_elements = ncol(support_map$operator),
    target_elements = nrow(support_map$operator),
    nonzero = length(support_map$operator@x),
    sparse_bytes = as.numeric(utils::object.size(support_map$operator)),
    mapped_source = sum(mapped),
    unmapped_source = sum(!mapped),
    empty_target = sum(row_sum <= tolerance),
    column_sum_min = min(column_sum),
    column_sum_max = max(column_sum),
    conservative = all(abs(column_sum - 1) <= tolerance),
    source_support_total = source_total,
    target_support_total = target_total,
    support_conservation_ratio = if (
      is.finite(source_total) && source_total > 0
    ) {
      target_total / source_total
    } else {
      NA_real_
    },
    mapping_variance_declared =
      !is.null(support_map$weight_variance),
    registration_estimated = FALSE,
    issues = issues
  )
  class(result) <- "ngeo_resampling_diagnostics"
  result
}

.ngeo_validate_resampling_result <- function(x) {
  if (!inherits(x, "ngeo_resampling_result") ||
      !inherits(x$data, "ngeo") ||
      !inherits(x$support_map, "ngeo_support_map") ||
      !inherits(x$diagnostics, "ngeo_resampling_diagnostics") ||
      !is.list(x$history) ||
      !identical(
        x$history$joint_hash,
        x$diagnostics$joint_hash
      )) {
    .ngeo_abort(
      "Resampling result identity or contents are invalid.",
      "ngeo_error_resampling_result"
    )
  }
  ngeo_validate(x$data, "strict")
  ngeo_validate_support_map(x$support_map)
  invisible(x)
}

#' Execute one explicitly authorized resampling plan
#'
#' @param plan A validated `ngeo_resampling_plan`.
#' @param layers Optional source layer selection.
#' @param value_variance Optional source-by-selected-map independent variance.
#' @param authorize Must be explicitly `TRUE`.
#' @param output_path Optional one-artifact output path.
#' @param writer Optional function receiving the resampled dataset and the
#' temporary output path. It must create exactly that one file.
#' @param overwrite Whether an atomic output may replace an existing file.
#' @return An `ngeo_resampling_result` with data, map, optional variance,
#' diagnostics, history, and optional atomic output metadata.
#' @templateVar example_call ngeo_resample(source_data, resampling_plan, authorize = TRUE)
#' @template stable-neuroimaging-method
#' @export
ngeo_resample <- function(
    plan,
    layers = NULL,
    value_variance = NULL,
    authorize = FALSE,
    output_path = NULL,
    writer = NULL,
    overwrite = FALSE) {
  built <- .ngeo_resampling_build(plan, authorize)
  layer_index <- .ngeo_layer_selection(built$source, layers)
  materialized <- (
    nrow(built$source$base$elements) +
      nrow(plan$target$base$elements)
  ) * length(layer_index)
  .ngeo_budget_assert(
    plan$budget, "materialized_elements", materialized
  )
  .ngeo_budget_assert(
    plan$budget, "memory_bytes",
    8 * materialized +
      as.numeric(utils::object.size(built$map$operator))
  )
  data <- aggregate_to(
    built$source,
    plan$target,
    built$map,
    layers = layers,
    allocation = if (
      identical(plan$policies$conservation, "normalize")
    ) "normalize" else "error",
    unmapped = plan$policies$missing,
    unknown = plan$policies$unknown
  )
  uncertainty <- plan$policies$uncertainty
  if (identical(uncertainty, "none") &&
      !is.null(value_variance)) {
    .ngeo_abort(
      "The plan does not authorize uncertainty propagation.",
      "ngeo_error_uncertainty"
    )
  }
  if (!identical(uncertainty, "none") &&
      is.null(value_variance)) {
    .ngeo_abort(
      "The plan requires aligned `value_variance`.",
      "ngeo_error_uncertainty"
    )
  }
  variance <- if (identical(uncertainty, "none")) {
    NULL
  } else {
    ngeo_support_variance(
      built$source,
      plan$target,
      built$map,
      value_variance = value_variance,
      layers = layers,
      allocation = if (
        identical(plan$policies$conservation, "normalize")
      ) "normalize" else "error",
      unmapped = plan$policies$missing,
      unknown = plan$policies$unknown
    )
  }
  diagnostics <- ngeo_resampling_diagnostics(plan, built$map)
  history <- list(
    schema = "NGCS-resampling-result-1",
    specification = "NGCS 3.2",
    plan_hash = plan$plan_hash,
    path = ngeo_transform_path_history(plan$path),
    support_map_hash = diagnostics$support_map_hash,
    joint_hash = diagnostics$joint_hash,
    policies = plan$policies,
    registration_estimated = FALSE,
    implicit_resampling = FALSE
  )
  data$history$resampling <- history
  output <- NULL
  if (xor(is.null(output_path), is.null(writer))) {
    .ngeo_abort(
      "`output_path` and `writer` must be supplied together.",
      "ngeo_error_argument"
    )
  }
  if (!is.null(output_path)) {
    if (!is.function(writer)) {
      .ngeo_abort("`writer` must be a function.",
                  "ngeo_error_argument")
    }
    output <- .ngeo_atomic_write(
      output_path,
      function(temporary) writer(data, temporary),
      overwrite = overwrite
    )
  }
  result <- structure(
    list(
      data = data,
      support_map = built$map,
      variance = variance,
      diagnostics = diagnostics,
      history = history,
      output = output
    ),
    class = "ngeo_resampling_result"
  )
  .ngeo_validate_resampling_result(result)
  result
}

#' @export
print.ngeo_resampling_plan <- function(x, ...) {
  cat(
    "<ngeo_resampling_plan>\n  method: ", x$method,
    "\n  path: ", x$path$path_hash,
    "\n  plan: ", x$plan_hash, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_resampling_diagnostics <- function(x, ...) {
  cat(
    "<ngeo_resampling_diagnostics>\n  method: ", x$method,
    "\n  mapped source: ", x$mapped_source, "/", x$source_elements,
    "\n  nonzero: ", x$nonzero,
    "\n  conservative: ", x$conservative, "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.ngeo_resampling_result <- function(x, ...) {
  cat(
    "<ngeo_resampling_result>\n  base: ",
    x$data$base$type,
    "\n  layers: ", nrow(x$data$layers),
    "\n  variance: ", !is.null(x$variance),
    "\n  joint hash: ", x$history$joint_hash, "\n",
    sep = ""
  )
  invisible(x)
}
