.ngeo_schema_definitions <- function() {
  definitions <- list(
    list("ngcs/ngeo-surface", "ngeo_surface", c(
      "one_domain", "aligned_values", "unique_element_id",
      "triangular_faces", "explicit_space"
    )),
    list("ngcs/ngeo-volume", "ngeo_volume", c(
      "one_domain", "aligned_values", "unique_element_id",
      "invertible_affine", "bounded_voxel_index"
    )),
    list("ngcs/ngeo-points", "ngeo_points", c(
      "one_domain", "aligned_values", "unique_element_id",
      "finite_coordinates"
    )),
    list("ngcs/ngeo-grayordinates", "ngeo_grayordinates", c(
      "one_domain", "aligned_values", "unique_element_id",
      "ordered_component_partition"
    )),
    list("ngcs/ngeo-regions", "ngeo_regions", c(
      "one_domain", "aligned_values", "unique_region_id",
      "aligned_support"
    )),
    list("ngcs/space", "ngeo_space", c(
      "stable_signature", "explicit_units", "explicit_kind"
    )),
    list("ngcs/transform", "ngeo_transform", c(
      "exact_source_space", "exact_target_space", "declared_method"
    )),
    list("ngcs/weights", "ngeo_weights", c(
      "square_sparse_matrix", "domain_bound", "declared_style"
    )),
    list("ngcs/partition", "ngeo_partition", c(
      "domain_bound", "aligned_membership", "known_regions"
    )),
    list("ngcs/support-map", "ngeo_support_map", c(
      "target_by_source", "nonnegative_sparse_operator",
      "ordered_domain_identity"
    )),
    list("ngcs/block-support-map", "ngeo_block_support_map", c(
      "complete_ordered_blocks", "logical_hash", "target_by_source"
    )),
    list("ngcs/support-covariance", "ngeo_support_covariance", c(
      "domain_bound", "ordered_element_id", "nonnegative_variance"
    )),
    list("ngcs/support-ensemble", "ngeo_support_ensemble", c(
      "common_domains", "normalized_weights", "typed_uncertainty"
    )),
    list("ngcs/delayed-values", "ngeo_delayed_values", c(
      "one_values_block", "positive_dimensions", "aligned_reader"
    )),
    list("ngcs/file-values", "ngeo_file_values", c(
      "one_values_block", "verified_source_identity",
      "bounded_binary_selection"
    )),
    list("ngcs/resampling-plan", "ngeo_resampling_plan", c(
      "exact_domain_identity", "authorized_path_only",
      "explicit_resampling_policies", "bounded_execution"
    )),
    list("ngcs/resampling-result", "ngeo_resampling_result", c(
      "one_target_domain", "joint_path_map_identity",
      "explicit_uncertainty_policy"
    )),
    list("ngcs/time-axis", "ngeo_time_axis", c(
      "strictly_increasing_time", "explicit_temporal_unit",
      "explicit_support", "stable_axis_identity"
    )),
    list("ngcs/temporal-weights", "ngeo_temporal_weights", c(
      "square_sparse_matrix", "axis_bound",
      "declared_temporal_relation"
    )),
    list(
      "ngcs/spatiotemporal-weights",
      "ngeo_spatiotemporal_weights",
      c(
        "separable_component_operators", "domain_and_axis_bound",
        "matrix_free_normative_execution"
      )
    ),
    list("ngcs/solver-control", "ngeo_solver_control", c(
      "immutable_solver_policy", "declared_convergence",
      "declared_approximation", "resource_budget"
    )),
    list("ngcs/iterative-solution", "ngeo_iterative_solution", c(
      "finite_aligned_solution", "explicit_convergence",
      "residual_history", "control_bound"
    )),
    list("ngcs/logdet-estimate", "ngeo_logdet_estimate", c(
      "declared_method", "convergent_parameter_bound",
      "monte_carlo_error", "truncation_diagnostic"
    )),
    list(
      "ngcs/iterative-spatial-regression",
      "ngeo_iterative_spatial_regression",
      c(
        "domain_and_weights_bound", "explicit_optimization_convergence",
        "declared_logdet_method", "solver_diagnostics"
      )
    ),
    list("ngcs/iterative-car", "ngeo_iterative_car", c(
      "domain_and_weights_bound", "declared_car_precision",
      "iterative_convergence", "sparse_operator"
    )),
    list("ngcs/provenance-dag", "ngeo_provenance_dag", c(
      "unique_nodes", "existing_parents", "acyclic_graph",
      "immutable_identity"
    )),
    list("ngcs/replay-manifest", "ngeo_replay_manifest", c(
      "whitelisted_operations", "ordered_dependencies",
      "logical_input_and_output_hashes", "explicit_environment"
    )),
    list("ngcs/artifact-manifest", "ngeo_artifact_manifest", c(
      "root_relative_paths", "complete_artifacts",
      "content_sha256", "immutable_identity"
    )),
    list("ngcs/batch-manifest", "ngeo_batch_manifest", c(
      "derivative_only_scope", "complete_batch",
      "verified_artifact_manifest", "atomic_publication"
    )),
    list("ngcs/space-registry", "ngeo_space_registry", c(
      "exact_space_hashes", "immutable_alias_targets", "registry_hash"
    )),
    list("ngcs/transform-graph", "ngeo_transform_graph", c(
      "supplied_edges_only", "exact_space_endpoints", "graph_hash"
    )),
    list("ngcs/execution-plan", "ngeo_execution_plan", c(
      "deterministic_tasks", "complete_identity", "resource_budget"
    )),
    list("ngcs/resource-budget", "ngeo_resource_budget", c(
      "positive_limits", "explicit_materialization_limit"
    ))
  )
  data.frame(
    schema_id = vapply(definitions, `[[`, character(1), 1L),
    class = vapply(definitions, `[[`, character(1), 2L),
    version = {
      schema_id <- vapply(definitions, `[[`, character(1), 1L)
      introduced <- rep.int("3.0", length(schema_id))
      names(introduced) <- schema_id
      by_version <- list(
        "3.1" = "ngcs/file-values",
        "3.2" = c("ngcs/resampling-plan", "ngcs/resampling-result"),
        "3.3" = c(
          "ngcs/time-axis", "ngcs/temporal-weights",
          "ngcs/spatiotemporal-weights"
        ),
        "3.4" = c(
          "ngcs/solver-control", "ngcs/iterative-solution",
          "ngcs/logdet-estimate", "ngcs/iterative-spatial-regression",
          "ngcs/iterative-car"
        ),
        "3.5" = c(
          "ngcs/provenance-dag", "ngcs/replay-manifest",
          "ngcs/artifact-manifest", "ngcs/batch-manifest"
        )
      )
      for (current in names(by_version)) {
        introduced[by_version[[current]]] <- current
      }
      unname(introduced)
    },
    status = "stable",
    invariants = I(lapply(definitions, `[[`, 3L)),
    stringsAsFactors = FALSE
  )
}

#' Return the current NGCS 3.x schema registry
#'
#' @return An `ngeo_schema_registry_30` containing versioned schema
#' descriptors and API lifecycle metadata.
#' @export
ngeo_schema_registry <- function() {
  structure(
    list(
      specification = "NGCS 3.5",
      version = "3.5",
      schemas = .ngeo_schema_definitions(),
      api = ngeo_api_lifecycle()
    ),
    class = "ngeo_schema_registry_30"
  )
}

#' Resolve the registered schema for an object or class
#'
#' @param x An NGCS object or one class name.
#' @return One schema descriptor row.
#' @export
ngeo_schema <- function(x) {
  classes <- if (is.character(x) && length(x) == 1L) x else class(x)
  definitions <- .ngeo_schema_definitions()
  index <- match(classes, definitions$class, nomatch = 0L)
  index <- index[index > 0L]
  if (!length(index)) {
    .ngeo_abort(
      "No NGCS 3.x schema is registered for this object class.",
      "ngeo_error_schema"
    )
  }
  definitions[index[[1L]], , drop = FALSE]
}

.ngeo_schema_validate_one <- function(x, schema_id) {
  if (grepl("^ngcs/ngeo-", schema_id)) {
    return(ngeo_validate(x, "strict"))
  }
  switch(
    schema_id,
    "ngcs/space" = {
      ngeo_space_hash(x)
      invisible(x)
    },
    "ngcs/transform" = ngeo_validate_transform(x),
    "ngcs/weights" = {
      if (!inherits(x, "ngeo_weights") ||
          !inherits(x$matrix, "Matrix") ||
          nrow(x$matrix) != ncol(x$matrix) ||
          !is.character(x$domain_hash) ||
          length(x$domain_hash) != 1L) {
        .ngeo_abort("Invalid `ngeo_weights` object.",
                    "ngeo_error_weights")
      }
      invisible(x)
    },
    "ngcs/partition" = {
      .ngeo_validate_partition(x)
      invisible(x)
    },
    "ngcs/support-map" = ngeo_validate_support_map(x),
    "ngcs/block-support-map" = ngeo_validate_block_support_map(x),
    "ngcs/support-covariance" = ngeo_validate_support_covariance(x),
    "ngcs/support-ensemble" = ngeo_validate_support_ensemble(x),
    "ngcs/delayed-values" = {
      if (!inherits(x, "ngeo_delayed_values") ||
          length(x$dim) != 2L || any(x$dim < 1L) ||
          !(is.function(x$reader) ||
            (is.character(x$reader) && length(x$reader) == 1L &&
             file.exists(x$reader)))) {
        .ngeo_abort("Invalid delayed values block.",
                    "ngeo_error_values")
      }
      invisible(x)
    },
    "ngcs/file-values" = ngeo_validate_file_values(x),
    "ngcs/resampling-plan" = ngeo_validate_resampling_plan(x),
    "ngcs/resampling-result" = .ngeo_validate_resampling_result(x),
    "ngcs/time-axis" = ngeo_validate_time_axis(x),
    "ngcs/temporal-weights" = ngeo_validate_temporal_weights(x),
    "ngcs/spatiotemporal-weights" =
      ngeo_validate_spatiotemporal_weights(x),
    "ngcs/solver-control" = ngeo_validate_solver_control(x),
    "ngcs/iterative-solution" = {
      if (!inherits(x, "ngeo_iterative_solution") ||
          !is.numeric(x$solution) || any(!is.finite(x$solution)) ||
          !is.logical(x$converged) || length(x$converged) != 1L ||
          is.na(x$converged) ||
          !is.numeric(x$iterations) || length(x$iterations) != 1L ||
          is.na(x$iterations) || x$iterations < 0 ||
          !is.numeric(x$residual_history) ||
          !length(x$residual_history) ||
          anyNA(x$residual_history) ||
          any(!is.finite(x$residual_history)) ||
          any(x$residual_history < 0) ||
          !is.character(x$control_hash) ||
          length(x$control_hash) != 1L) {
        .ngeo_abort(
          "Invalid iterative solver result.",
          "ngeo_error_solver"
        )
      }
      invisible(x)
    },
    "ngcs/logdet-estimate" = {
      if (!inherits(x, "ngeo_logdet_estimate") ||
          !is.numeric(x$estimate) || length(x$estimate) != 1L ||
          !is.finite(x$estimate) ||
          !is.numeric(x$standard_error) ||
          length(x$standard_error) != 1L ||
          is.na(x$standard_error) || x$standard_error < 0 ||
          !is.numeric(x$truncation_bound) ||
          length(x$truncation_bound) != 1L ||
          is.na(x$truncation_bound) || x$truncation_bound < 0 ||
          !x$method %in%
            c("exact_small", "hutchinson_power_series")) {
        .ngeo_abort(
          "Invalid log-determinant estimate.",
          "ngeo_error_logdet"
        )
      }
      invisible(x)
    },
    "ngcs/iterative-spatial-regression" = {
      if (!inherits(x, "ngeo_iterative_spatial_regression") ||
          !x$model %in% c("sar", "sem") ||
          !is.data.frame(x$coefficients) ||
          any(!is.finite(x$coefficients$estimate)) ||
          !is.numeric(x$fitted) || any(!is.finite(x$fitted)) ||
          !is.numeric(x$residuals) ||
          any(!is.finite(x$residuals)) ||
          !is.list(x$log_determinant) ||
          !is.list(x$optimization) ||
          !is.logical(x$optimization$converged) ||
          length(x$optimization$converged) != 1L ||
          !is.character(x$domain_hash) ||
          !is.character(x$control_hash)) {
        .ngeo_abort(
          "Invalid iterative spatial regression result.",
          "ngeo_error_model"
        )
      }
      invisible(x)
    },
    "ngcs/iterative-car" = {
      if (!inherits(x, "ngeo_iterative_car") ||
          !x$type %in% c("proper", "intrinsic") ||
          !is.numeric(x$fitted) || any(!is.finite(x$fitted)) ||
          !is.numeric(x$residuals) ||
          any(!is.finite(x$residuals)) ||
          !is.numeric(x$precision) || length(x$precision) != 1L ||
          !is.finite(x$precision) || x$precision <= 0 ||
          !inherits(x$solve, "ngeo_iterative_solution") ||
          !is.character(x$domain_hash) ||
          !is.character(x$control_hash)) {
        .ngeo_abort(
          "Invalid iterative CAR result.",
          "ngeo_error_model"
        )
      }
      invisible(x)
    },
    "ngcs/provenance-dag" = ngeo_validate_provenance_dag(x),
    "ngcs/replay-manifest" =
      ngeo_validate_replay_manifest(x, mode = "error"),
    "ngcs/artifact-manifest" =
      ngeo_validate_artifact_manifest(x, mode = "error"),
    "ngcs/batch-manifest" =
      ngeo_validate_artifact_batch(x, mode = "error"),
    "ngcs/space-registry" = ngeo_validate_space_registry(x),
    "ngcs/transform-graph" = ngeo_validate_transform_graph(x),
    "ngcs/execution-plan" = {
      if (!inherits(x, "ngeo_execution_plan") ||
          !is.character(x$operation) || length(x$operation) != 1L ||
          !is.list(x$tasks) || !length(x$tasks) ||
          !is.function(x$executor) ||
          !is.character(x$executor_id) ||
          length(x$executor_id) != 1L ||
          !inherits(x$budget, "ngeo_resource_budget") ||
          !identical(
            digest::digest(
              list(
                operation = x$operation,
                tasks = x$tasks,
                executor_id = x$executor_id,
                identity = x$identity
              ),
              algo = "sha256"
            ),
            x$plan_hash
          )) {
        .ngeo_abort("Invalid deterministic execution plan.",
                    "ngeo_error_execution_plan")
      }
      invisible(x)
    },
    "ngcs/resource-budget" = {
      limits <- unlist(x, use.names = FALSE)
      if (!inherits(x, "ngeo_resource_budget") ||
          length(limits) != 4L || anyNA(limits) ||
          any(limits <= 0) ||
          any(!is.finite(limits) & limits != Inf)) {
        .ngeo_abort("Invalid resource budget.",
                    "ngeo_error_resource")
      }
      invisible(x)
    },
    .ngeo_abort("Schema validator is not implemented.",
                "ngeo_error_schema")
  )
}

.ngeo_issue_frame <- function(severity = character(),
                              code = character(),
                              condition_class = character(),
                              message = character()) {
  data.frame(
    severity = severity,
    code = code,
    condition_class = condition_class,
    message = message,
    stringsAsFactors = FALSE
  )
}

.ngeo_condition_code <- function(condition) {
  primary <- class(condition)[
    grepl("^ngeo_(error|warning)", class(condition))
  ][1L]
  if (is.na(primary) || !nzchar(primary)) primary <- if (
    inherits(condition, "warning")
  ) {
    "warning"
  } else {
    "error"
  }
  toupper(gsub("[^A-Za-z0-9]+", "_", primary))
}

#' Validate any registered NGCS 3.x object
#'
#' @param x A registered NGCS object.
#' @param mode Return a structured report or raise a classed condition when
#' invalid.
#' @return An `ngeo_validation_report`.
#' @export
ngeo_validate_schema <- function(x, mode = c("report", "error")) {
  mode <- match.arg(mode)
  schema <- ngeo_schema(x)
  warnings <- list()
  failure <- tryCatch(
    withCallingHandlers(
      {
        .ngeo_schema_validate_one(x, schema$schema_id[[1L]])
        NULL
      },
      warning = function(condition) {
        warnings[[length(warnings) + 1L]] <<- condition
        invokeRestart("muffleWarning")
      }
    ),
    error = identity
  )
  issues <- .ngeo_issue_frame()
  if (length(warnings)) {
    issues <- do.call(rbind, lapply(warnings, function(condition) {
      .ngeo_issue_frame(
        "warning",
        .ngeo_condition_code(condition),
        class(condition)[[1L]],
        conditionMessage(condition)
      )
    }))
  }
  if (inherits(failure, "error")) {
    issues <- rbind(
      issues,
      .ngeo_issue_frame(
        "error",
        .ngeo_condition_code(failure),
        class(failure)[[1L]],
        conditionMessage(failure)
      )
    )
  }
  report <- structure(
    list(
      valid = !any(issues$severity == "error"),
      schema_id = schema$schema_id[[1L]],
      schema_version = schema$version[[1L]],
      object_class = class(x)[[1L]],
      checked_invariants = schema$invariants[[1L]],
      issues = issues
    ),
    class = "ngeo_validation_report"
  )
  if (!report$valid && identical(mode, "error")) {
    condition <- structure(
      list(
        message = paste0(
          "NGCS schema validation failed: ",
          report$issues$message[
            report$issues$severity == "error"
          ][[1L]]
        ),
        call = NULL,
        report = report
      ),
      class = c(
        "ngeo_error_schema_validation",
        "ngeo_error_schema",
        "ngeo_error",
        "error",
        "condition"
      )
    )
    stop(condition)
  }
  report
}

.ngeo_canonical_json_value <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      .ngeo_canonical_json_value(as.list(x[i, , drop = FALSE]))
    })
    return(rows)
  }
  if (is.list(x)) {
    value <- lapply(x, .ngeo_canonical_json_value)
    names_value <- names(value)
    if (!is.null(names_value) && length(value) &&
        all(!is.na(names_value)) && all(nzchar(names_value))) {
      value <- value[order(enc2utf8(names_value), method = "radix")]
    }
    return(value)
  }
  x
}

.ngeo_manifest_json <- function(x) {
  jsonlite::toJSON(
    .ngeo_canonical_json_value(x),
    auto_unbox = TRUE,
    null = "null",
    na = "string",
    digits = NA,
    dataframe = "rows",
    matrix = "rowmajor",
    pretty = FALSE
  )
}

.ngeo_manifest_sha256 <- function(x) {
  value <- x
  value$canonical_sha256 <- NULL
  digest::digest(
    .ngeo_manifest_json(value),
    algo = "sha256",
    serialize = FALSE
  )
}

.ngeo_order_hash <- function(x) {
  digest::digest(
    .ngeo_manifest_json(as.character(x)),
    algo = "sha256",
    serialize = FALSE
  )
}

.ngeo_storage_name <- function(values) {
  if (is.null(values)) return("none")
  if (inherits(values, "ngeo_file_values")) return("file-backed")
  if (inherits(values, "ngeo_delayed_values")) return("delayed")
  if (inherits(values, "Matrix")) return(class(values)[[1L]])
  typeof(values)
}

.ngeo_portable_sparse_signature <- function(x) {
  entries <- Matrix::summary(x)
  list(
    dimensions = dim(x),
    i = entries$i,
    j = entries$j,
    x = entries$x
  )
}

.ngeo_portable_domain_signature <- function(domain) {
  common <- list(
    type = domain$type,
    elements = domain$elements,
    space = .ngeo_space_signature(domain$space)
  )
  specific <- switch(
    domain$type,
    surface = list(
      coordinates = domain$coordinates,
      coordinate_meta = domain$coordinate_meta,
      faces = domain$faces,
      active_coordinate = domain$active_coordinate
    ),
    volume = list(
      dimensions = domain$dim,
      affine = domain$affine,
      voxel_index = domain$voxel_index
    ),
    points = list(
      coordinates = domain$coordinates,
      uncertainty = domain$uncertainty
    ),
    grayordinates = list(
      components = lapply(domain$components, function(component) {
        geometry <- component$geometry
        component$geometry <- NULL
        if (inherits(geometry, "ngeo")) {
          component$geometry_sha256 <- digest::digest(
            .ngeo_manifest_json(
              .ngeo_portable_domain_signature(geometry$domain)
            ),
            algo = "sha256",
            serialize = FALSE
          )
        }
        component
      })
    ),
    regions = list(
      support_size = domain$support_size,
      centroid = domain$centroid,
      adjacency = if (inherits(domain$adjacency, "Matrix")) {
        .ngeo_portable_sparse_signature(domain$adjacency)
      } else {
        domain$adjacency
      }
    ),
    .ngeo_abort("Portable domain signature is not implemented.",
                "ngeo_error_schema")
  )
  c(common, specific)
}

.ngeo_portable_domain_hash <- function(domain) {
  digest::digest(
    .ngeo_manifest_json(.ngeo_portable_domain_signature(domain)),
    algo = "sha256",
    serialize = FALSE
  )
}

.ngeo_object_manifest_metadata <- function(x, schema_id) {
  if (inherits(x, "ngeo")) {
    return(list(
      domain_type = x$domain$type,
      domain_sha256 = .ngeo_portable_domain_hash(x$domain),
      ordered_element_id_sha256 = .ngeo_order_hash(
        x$domain$elements$element_id
      ),
      element_count = nrow(x$domain$elements),
      space = list(
        space_id = x$domain$space$space_id,
        sha256 = ngeo_space_hash(x$domain$space)
      ),
      values = list(
        storage = .ngeo_storage_name(x$values),
        dimensions = if (is.null(x$values)) c(0L, 0L) else dim(x$values),
        file_identity = if (inherits(x$values, "ngeo_file_values")) {
          ngeo_file_values_identity(x$values)
        } else {
          NULL
        }
      ),
      map_count = nrow(x$maps),
      ordered_map_id_sha256 = .ngeo_order_hash(x$maps$map_id),
      maps = x$maps,
      measures = x$measures
    ))
  }
  if (inherits(x, "ngeo_space")) {
    return(list(
      space_id = x$space_id,
      kind = x$kind,
      units = x$units,
      structure = x$structure,
      template = x$template,
      density = x$density,
      resolution = x$resolution,
      sha256 = ngeo_space_hash(x)
    ))
  }
  if (inherits(x, "ngeo_transform")) {
    return(list(
      type = x$type,
      method = x$method,
      source_space_sha256 = ngeo_space_hash(x$from),
      target_space_sha256 = ngeo_space_hash(x$to),
      transform_sha256 = ngeo_transform_hash(x)
    ))
  }
  if (inherits(x, "ngeo_support_map")) {
    return(list(
      direction = x$direction,
      type = x$type,
      coverage = x$coverage,
      dimensions = dim(x$operator),
      source_domain_hash = x$source_domain_hash,
      target_domain_hash = x$target_domain_hash,
      logical_hash = ngeo_support_map_hash(x)
    ))
  }
  if (inherits(x, "ngeo_block_support_map")) {
    return(list(
      direction = x$orientation,
      dimensions = x$dim,
      row_blocks = length(x$row_groups),
      source_blocks = length(x$column_groups),
      logical_hash = x$logical_hash,
      block_hash = x$block_hash
    ))
  }
  if (inherits(x, "ngeo_resampling_plan")) {
    return(list(
      plan_hash = x$plan_hash,
      source_domain_hash = x$source_domain_hash,
      target_domain_hash = x$target_domain_hash,
      path_hash = x$path$path_hash,
      method = x$method,
      policies = x$policies
    ))
  }
  if (inherits(x, "ngeo_resampling_result")) {
    return(list(
      plan_hash = x$provenance$plan_hash,
      path_hash = x$provenance$path$path_hash,
      support_map_hash = x$provenance$support_map_hash,
      joint_hash = x$provenance$joint_hash,
      target_domain_hash = ngeo_domain_hash(x$data),
      dimensions = dim(x$data$values),
      variance_dimensions = dim(x$variance) %||% NULL
    ))
  }
  if (inherits(x, "ngeo_time_axis")) {
    return(list(
      axis_hash = x$axis_hash,
      n_time = length(x$time),
      unit = x$unit,
      support = x$support,
      regular = x$regular
    ))
  }
  if (inherits(x, "ngeo_temporal_weights")) {
    return(list(
      weights_hash = x$weights_hash,
      axis_hash = x$axis_hash,
      dimensions = dim(x$matrix),
      method = x$method,
      normalization = x$normalization,
      directed = x$directed
    ))
  }
  if (inherits(x, "ngeo_spatiotemporal_weights")) {
    return(list(
      weights_hash = x$weights_hash,
      domain_hash = x$domain_hash,
      axis_hash = x$axis_hash,
      n_space = x$n_space,
      n_time = x$n_time,
      combination = x$combination,
      matrix_materialized = x$matrix_materialized
    ))
  }
  if (inherits(x, "ngeo_solver_control")) {
    return(c(
      .ngeo_solver_control_payload(x),
      list(control_hash = x$control_hash)
    ))
  }
  if (inherits(x, "ngeo_iterative_solution")) {
    return(list(
      method = x$method,
      converged = x$converged,
      iterations = x$iterations,
      relative_residual = x$relative_residual,
      control_hash = x$control_hash,
      solution_sha256 = digest::digest(
        x$solution, algo = "sha256"
      )
    ))
  }
  if (inherits(x, "ngeo_logdet_estimate")) {
    return(list(
      method = x$method,
      estimate = x$estimate,
      standard_error = x$standard_error,
      truncation_bound = x$truncation_bound,
      parameter = x$parameter,
      dimension = x$dimension,
      control_hash = x$control_hash
    ))
  }
  if (inherits(x, "ngeo_iterative_spatial_regression")) {
    return(list(
      model = x$model,
      domain_hash = x$domain_hash,
      control_hash = x$control_hash,
      spatial_parameter = x$spatial_parameter,
      coefficients = x$coefficients,
      log_determinant = x$log_determinant,
      optimization = x$optimization,
      result_sha256 = digest::digest(
        list(x$fitted, x$residuals), algo = "sha256"
      )
    ))
  }
  if (inherits(x, "ngeo_iterative_car")) {
    return(list(
      type = x$type,
      domain_hash = x$domain_hash,
      control_hash = x$control_hash,
      precision = x$precision,
      rho = x$rho,
      solve_converged = x$solve$converged,
      result_sha256 = digest::digest(
        list(x$fitted, x$residuals), algo = "sha256"
      )
    ))
  }
  if (inherits(x, "ngeo_delayed_values")) {
    return(list(
      dimensions = x$dim,
      ordered_map_name_sha256 = .ngeo_order_hash(x$dimnames[[2L]]),
      source = as.character(x$source)
    ))
  }
  list(
    names = names(x),
    dimensions = dim(x) %||% NULL,
    length = length(x)
  )
}

#' Create a portable NGCS 3.x object metadata manifest
#'
#' @param x A valid registered NGCS object.
#' @return A JSON-compatible `ngeo_object_manifest`.
#' @export
ngeo_object_manifest <- function(x) {
  report <- ngeo_validate_schema(x, "error")
  manifest <- list(
    schema = "NGCS-object-manifest-1",
    specification = paste("NGCS", report$schema_version),
    object_schema = report$schema_id,
    object_schema_version = report$schema_version,
    object_class = report$object_class,
    metadata = .ngeo_object_manifest_metadata(x, report$schema_id)
  )
  manifest$canonical_sha256 <- .ngeo_manifest_sha256(manifest)
  structure(manifest, class = c("ngeo_object_manifest", "list"))
}

#' Validate a portable NGCS object manifest
#'
#' @param manifest A manifest list.
#' @param x Optional object that the manifest must describe.
#' @param mode Return a report or raise a classed error.
#' @return An `ngeo_manifest_validation_report`.
#' @export
ngeo_validate_manifest <- function(
    manifest,
    x = NULL,
    mode = c("report", "error")) {
  mode <- match.arg(mode)
  required <- c(
    "schema", "specification", "object_schema",
    "object_schema_version", "object_class", "metadata",
    "canonical_sha256"
  )
  issues <- .ngeo_issue_frame()
  add_error <- function(code, message) {
    issues <<- rbind(
      issues,
      .ngeo_issue_frame("error", code, "ngeo_error_manifest", message)
    )
  }
  if (!is.list(manifest) || any(!required %in% names(manifest))) {
    add_error("MANIFEST_STRUCTURE", "Manifest fields are incomplete.")
  } else {
    definitions <- .ngeo_schema_definitions()
    schema_row <- match(
      manifest$object_schema, definitions$schema_id,
      nomatch = 0L
    )
    expected_version <- if (schema_row) {
      definitions$version[[schema_row]]
    } else {
      NA_character_
    }
    if (!identical(manifest$schema, "NGCS-object-manifest-1") ||
        !identical(
          manifest$specification,
          paste("NGCS", manifest$object_schema_version)
        ) ||
        !identical(
          manifest$object_schema_version,
          expected_version
        )) {
      add_error("MANIFEST_SCHEMA", "Manifest schema is unsupported.")
    }
    if (!identical(
      .ngeo_manifest_sha256(manifest),
      manifest$canonical_sha256
    )) {
      add_error("MANIFEST_HASH", "Manifest canonical SHA-256 differs.")
    }
    if (!is.null(x)) {
      expected <- ngeo_object_manifest(x)
      if (!identical(
        manifest$canonical_sha256,
        expected$canonical_sha256
      )) {
        add_error(
          "MANIFEST_OBJECT_MISMATCH",
          "Manifest does not describe the supplied object."
        )
      }
    }
  }
  report <- structure(
    list(
      valid = !nrow(issues),
      schema = if (is.list(manifest)) manifest$schema %||% NA_character_
        else NA_character_,
      issues = issues
    ),
    class = "ngeo_manifest_validation_report"
  )
  if (!report$valid && identical(mode, "error")) {
    condition <- structure(
      list(
        message = paste(
          "NGCS manifest validation failed:",
          report$issues$message[[1L]]
        ),
        call = NULL,
        report = report
      ),
      class = c(
        "ngeo_error_manifest",
        "ngeo_error_schema",
        "ngeo_error",
        "error",
        "condition"
      )
    )
    stop(condition)
  }
  report
}

#' Write and read portable NGCS object manifests
#'
#' @param manifest A valid manifest.
#' @param path JSON path.
#' @param overwrite Whether to replace an existing file atomically.
#' @return `write_ngeo_manifest()` returns an atomic output;
#' `read_ngeo_manifest()` returns a verified manifest.
#' @name ngeo_manifest_io
NULL

#' @rdname ngeo_manifest_io
#' @export
write_ngeo_manifest <- function(manifest, path, overwrite = FALSE) {
  .ngeo_require("jsonlite", "NGCS manifest writing")
  ngeo_validate_manifest(manifest, mode = "error")
  ngeo_atomic_write(
    path,
    function(temporary) {
      jsonlite::write_json(
        unclass(manifest),
        temporary,
        pretty = TRUE,
        auto_unbox = TRUE,
        null = "null",
        na = "string",
        digits = NA
      )
    },
    overwrite = overwrite
  )
}

#' @rdname ngeo_manifest_io
#' @export
read_ngeo_manifest <- function(path) {
  .ngeo_require("jsonlite", "NGCS manifest reading")
  .ngeo_assert_scalar_character(path, "path")
  if (!file.exists(path)) {
    .ngeo_abort("NGCS manifest does not exist.", "ngeo_error_io")
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      .ngeo_abort(
        paste("Could not parse NGCS manifest:", conditionMessage(error)),
        "ngeo_error_io"
      )
    }
  )
  ngeo_validate_manifest(manifest, mode = "error")
  structure(manifest, class = c("ngeo_object_manifest", "list"))
}

#' Report the public API lifecycle for the 4.0 transition
#'
#' @return A data frame containing every exported API and lifecycle state.
#' @export
ngeo_api_lifecycle <- function() {
  exports <- sort(getNamespaceExports("neurogeo"))
  introduced_30 <- c(
    "ngeo_schema_registry", "ngeo_schema", "ngeo_validate_schema",
    "ngeo_object_manifest", "ngeo_validate_manifest",
    "write_ngeo_manifest", "read_ngeo_manifest",
    "ngeo_api_lifecycle", "ngeo_migrate_schema"
  )
  introduced_31 <- c(
    "ngeo_file_values", "ngeo_validate_file_values",
    "ngeo_file_values_identity", "read_ngeo_nifti_filebacked",
    "read_ngeo_cifti_filebacked", "read_ngeo_mgh_filebacked",
    "read_ngeo_filebacked", "write_ngeo_filebacked"
  )
  introduced_32 <- c(
    "ngeo_resampling_plan", "ngeo_validate_resampling_plan",
    "ngeo_build_resampling_map", "ngeo_resampling_diagnostics",
    "ngeo_resample"
  )
  introduced_33 <- c(
    "ngeo_time_axis", "ngeo_validate_time_axis",
    "ngeo_time_axis_hash", "ngeo_set_time_axis",
    "ngeo_get_time_axis", "ngeo_time_slice",
    "ngeo_temporal_weights", "ngeo_validate_temporal_weights",
    "ngeo_temporal_neighbors", "ngeo_spatiotemporal_weights",
    "ngeo_validate_spatiotemporal_weights",
    "ngeo_materialize_spatiotemporal_weights",
    "ngeo_spatiotemporal_lag", "ngeo_temporal_moran",
    "ngeo_spatiotemporal_moran", "ngeo_temporal_variogram",
    "ngeo_spatiotemporal_variogram", "ngeo_longitudinal_change",
    "ngeo_temporal_trend", "ngeo_temporal_contrast"
  )
  introduced_34 <- c(
    "ngeo_solver_control", "ngeo_validate_solver_control",
    "ngeo_iterative_solve", "ngeo_logdet_approx",
    "ngeo_spatial_regression_iterative", "ngeo_car_iterative",
    "ngeo_gwr_batched", "ngeo_kriging_batched"
  )
  introduced_35 <- c(
    "ngeo_provenance_dag", "ngeo_validate_provenance_dag",
    "ngeo_environment_snapshot", "ngeo_logical_hash",
    "ngeo_replay_step", "ngeo_record_replay",
    "ngeo_validate_replay_manifest", "ngeo_replay",
    "write_ngeo_replay_manifest", "read_ngeo_replay_manifest",
    "ngeo_artifact_manifest", "ngeo_validate_artifact_manifest",
    "write_ngeo_artifact_manifest", "read_ngeo_artifact_manifest",
    "ngeo_validate_artifact_batch", "ngeo_write_artifact_batch",
    "ngeo_read_artifact_batch"
  )
  introduction <- rep.int("<=2.9.1", length(exports))
  names(introduction) <- exports
  by_version <- list(
    "3.0" = introduced_30,
    "3.1" = introduced_31,
    "3.2" = introduced_32,
    "3.3" = introduced_33,
    "3.4" = introduced_34,
    "3.5" = introduced_35
  )
  for (current in names(by_version)) {
    introduction[intersect(exports, by_version[[current]])] <- current
  }
  deprecated <- c(
    ngeo_metric = NA_character_,
    ngeo_delayed_values = "read_ngeo_filebacked",
    ngeo_block_support_map = "ngeo_support_map",
    ngeo_validate_block_support_map = "ngeo_validate",
    ngeo_materialize_support_map = "ngeo_support_map",
    ngeo_change_support_block = "ngeo_change_support",
    ngeo_block_diagnostics = "ngeo_support_diagnostics",
    ngeo_block_variance = "ngeo_support_variance",
    ngeo_compose_block_support_map = "ngeo_compose_support_map",
    ngeo_execution_plan = "ngeo_record_replay",
    ngeo_execute = "ngeo_replay",
    ngeo_cache = NA_character_,
    ngeo_cache_compute = NA_character_,
    ngeo_atomic_write = NA_character_,
    ngeo_gwr_batched = "ngeo_gwr",
    ngeo_kriging_batched = "ngeo_kriging",
    ngeo_schema_registry = NA_character_,
    ngeo_schema = "ngeo_validate",
    ngeo_validate_schema = "ngeo_validate",
    ngeo_migrate_schema = NA_character_,
    ngeo_api_inventory = "ngeo_api_lifecycle",
    ngeo_compatibility_matrix = NA_character_,
    ngeo_conformance_manifest = NA_character_
  )
  lifecycle <- rep.int("stable", length(exports))
  names(lifecycle) <- exports
  lifecycle[intersect(exports, names(deprecated))] <- "deprecated"
  replacement <- rep.int(NA_character_, length(exports))
  names(replacement) <- exports
  replacement[intersect(exports, names(deprecated))] <-
    deprecated[intersect(exports, names(deprecated))]
  planned_action <- ifelse(
    lifecycle == "deprecated",
    "remove_in_4.0",
    "retain"
  )
  data.frame(
    api = exports,
    introduced = unname(introduction),
    lifecycle = unname(lifecycle),
    replacement = unname(replacement),
    planned_action = unname(planned_action),
    stringsAsFactors = FALSE
  )
}

#' Mark a validated object as migrated to an NGCS schema version
#'
#' @param x A registered NGCS object.
#' @param target_version NGCS 3.0, 3.1, 3.2, 3.3, 3.4, or 3.5.
#' @return The validated object with an auditable schema-migration attribute.
#' @export
ngeo_migrate_schema <- function(x, target_version = "3.0") {
  target_version <- match.arg(
    target_version, c("3.0", "3.1", "3.2", "3.3", "3.4", "3.5")
  )
  schema <- ngeo_schema(x)
  if (utils::compareVersion(
    target_version, schema$version[[1L]]
  ) < 0L) {
    .ngeo_abort(
      paste0(
        "Object schema ", schema$version[[1L]],
        " cannot migrate backward to ", target_version, "."
      ),
                "ngeo_error_schema")
  }
  report <- ngeo_validate_schema(x, "error")
  attr(x, "ngeo_schema_migration") <- list(
    source_embedded_version = if (inherits(x, "ngeo")) {
      x$provenance$spec_version
    } else {
      x$spec_version %||% "pre-3.0"
    },
    target_version = target_version,
    schema_id = schema$schema_id[[1L]],
    valid = report$valid
  )
  x
}

#' @export
print.ngeo_schema_registry_30 <- function(x, ...) {
  cat("<ngeo_schema_registry_30>\n  specification: ",
      x$specification, "\n  schemas: ", nrow(x$schemas),
      "\n  public APIs: ", nrow(x$api), "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_validation_report <- function(x, ...) {
  cat("<ngeo_validation_report>\n  schema: ", x$schema_id,
      "\n  valid: ", x$valid,
      "\n  issues: ", nrow(x$issues), "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_object_manifest <- function(x, ...) {
  cat("<ngeo_object_manifest>\n  schema: ", x$object_schema,
      "\n  SHA-256: ", x$canonical_sha256, "\n", sep = "")
  invisible(x)
}
