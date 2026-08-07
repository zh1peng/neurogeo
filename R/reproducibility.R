# Reproducible replay and artifact publication.
.ngeo_named_list <- function(x, name) {
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x))) {
    .ngeo_abort(
      sprintf("`%s` must be a uniquely named list.", name),
      "ngeo_error_replay"
    )
  }
  x
}

.ngeo_replay_operations <- function() {
  c(
    "ngeo_subset",
    "ngeo_time_slice",
    "ngeo_longitudinal_change",
    "ngeo_temporal_trend",
    "ngeo_temporal_contrast"
  )
}

.ngeo_hash_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[0-9a-f]{64}$", x)
}

.ngeo_dag_sha256 <- function(x) {
  value <- x
  value$dag_sha256 <- NULL
  digest::digest(
    .ngeo_manifest_json(value),
    algo = "sha256",
    serialize = FALSE
  )
}

#' Construct an auditable history directed acyclic graph
#'
#' @param nodes A list of nodes with `id`, `type`, and `logical_hash`.
#' @param edges A list of directed edges with `from`, `to`, and `role`.
#' @return An immutable `ngeo_history_dag`.
#' @export
ngeo_history_dag <- function(nodes, edges = list()) {
  if (!is.list(nodes) || !length(nodes) || !is.list(edges)) {
    .ngeo_abort(
      "Provenance DAG nodes and edges must be non-empty/list values.",
      "ngeo_error_history_dag"
    )
  }
  nodes <- lapply(nodes, function(node) {
    if (!is.list(node) ||
        any(!c("id", "type", "logical_hash") %in% names(node))) {
      .ngeo_abort(
        "Every history node requires id, type, and logical_hash.",
        "ngeo_error_history_dag"
      )
    }
    list(
      id = as.character(node$id),
      type = as.character(node$type),
      logical_hash = as.character(node$logical_hash)
    )
  })
  edges <- lapply(edges, function(edge) {
    if (!is.list(edge) ||
        any(!c("from", "to", "role") %in% names(edge))) {
      .ngeo_abort(
        "Every history edge requires from, to, and role.",
        "ngeo_error_history_dag"
      )
    }
    list(
      from = as.character(edge$from),
      to = as.character(edge$to),
      role = as.character(edge$role)
    )
  })
  nodes <- nodes[order(vapply(nodes, `[[`, character(1), "id"))]
  if (length(edges)) {
    key <- vapply(
      edges,
      function(edge) paste(edge$from, edge$to, edge$role, sep = "\r"),
      character(1)
    )
    edges <- edges[order(key)]
  }
  dag <- list(
    schema = "NGCS-history-dag-1",
    specification = "NGCS 3.5",
    nodes = nodes,
    edges = edges,
    dag_sha256 = NULL
  )
  dag$dag_sha256 <- .ngeo_dag_sha256(dag)
  class(dag) <- c("ngeo_history_dag", "list")
  ngeo_validate_history_dag(dag)
  dag
}

#' Validate a history DAG and its immutable identity
#'
#' @param x A history DAG.
#' @return `x`, invisibly.
#' @export
ngeo_validate_history_dag <- function(x) {
  invalid <- !is.list(x) ||
    any(!c(
      "schema", "specification", "nodes", "edges", "dag_sha256"
    ) %in% names(x)) ||
    !identical(x$schema, "NGCS-history-dag-1") ||
    !identical(x$specification, "NGCS 3.5") ||
    !is.list(x$nodes) || !length(x$nodes) || !is.list(x$edges)
  if (invalid) {
    .ngeo_abort(
      "Provenance DAG structure or schema is invalid.",
      "ngeo_error_history_dag"
    )
  }
  ids <- vapply(x$nodes, function(node) {
    if (!is.list(node) ||
        any(!c("id", "type", "logical_hash") %in% names(node)) ||
        !is.character(node$id) || length(node$id) != 1L ||
        is.na(node$id) || !nzchar(node$id) ||
        !is.character(node$type) || length(node$type) != 1L ||
        is.na(node$type) || !nzchar(node$type) ||
        !.ngeo_hash_string(node$logical_hash)) {
      .ngeo_abort(
        "Provenance node fields are invalid.",
        "ngeo_error_history_dag"
      )
    }
    node$id
  }, character(1))
  if (anyDuplicated(ids)) {
    .ngeo_abort(
      "Provenance node identifiers must be unique.",
      "ngeo_error_history_dag"
    )
  }
  indegree <- stats::setNames(integer(length(ids)), ids)
  children <- stats::setNames(vector("list", length(ids)), ids)
  edge_key <- character()
  for (edge in x$edges) {
    if (!is.list(edge) ||
        any(!c("from", "to", "role") %in% names(edge)) ||
        !is.character(edge$from) || length(edge$from) != 1L ||
        !is.character(edge$to) || length(edge$to) != 1L ||
        !is.character(edge$role) || length(edge$role) != 1L ||
        is.na(edge$role) || !nzchar(edge$role) ||
        !edge$from %in% ids || !edge$to %in% ids) {
      .ngeo_abort(
        "Provenance edge has a missing parent or invalid field.",
        "ngeo_error_history_parent"
      )
    }
    edge_key <- c(edge_key, paste(edge$from, edge$to, edge$role))
    indegree[[edge$to]] <- indegree[[edge$to]] + 1L
    children[[edge$from]] <- c(children[[edge$from]], edge$to)
  }
  if (anyDuplicated(edge_key)) {
    .ngeo_abort(
      "Provenance edges must be unique.",
      "ngeo_error_history_dag"
    )
  }
  queue <- names(indegree)[indegree == 0L]
  visited <- 0L
  while (length(queue)) {
    id <- queue[[1L]]
    queue <- queue[-1L]
    visited <- visited + 1L
    for (child in children[[id]]) {
      indegree[[child]] <- indegree[[child]] - 1L
      if (indegree[[child]] == 0L) queue <- c(queue, child)
    }
  }
  if (visited != length(ids)) {
    .ngeo_abort(
      "Provenance graph contains a directed cycle.",
      "ngeo_error_history_cycle"
    )
  }
  if (!.ngeo_hash_string(x$dag_sha256) ||
      !identical(x$dag_sha256, .ngeo_dag_sha256(x))) {
    .ngeo_abort(
      "Provenance DAG immutable identity differs.",
      "ngeo_error_history_hash"
    )
  }
  invisible(x)
}

#' Capture the deterministic software environment used for replay
#'
#' @return An `ngeo_environment_snapshot`.
#' @export
ngeo_environment_snapshot <- function() {
  dependencies <- c("digest", "jsonlite", "Matrix")
  versions <- lapply(dependencies, function(package) {
    if (requireNamespace(package, quietly = TRUE)) {
      as.character(utils::packageVersion(package))
    } else {
      NA_character_
    }
  })
  names(versions) <- dependencies
  snapshot <- list(
    schema = "NGCS-environment-snapshot-1",
    specification = "NGCS 3.5",
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    neurogeo_version = .ngeo_package_version(),
    dependencies = versions,
    environment_sha256 = NULL
  )
  snapshot$environment_sha256 <- .ngeo_environment_sha256(snapshot)
  class(snapshot) <- c("ngeo_environment_snapshot", "list")
  snapshot
}

.ngeo_environment_sha256 <- function(x) {
  value <- x
  value$environment_sha256 <- NULL
  digest::digest(
    .ngeo_manifest_json(value),
    algo = "sha256",
    serialize = FALSE
  )
}

.ngeo_logical_payload <- function(x, budget) {
  if (inherits(x, "ngeo")) {
    ngeo_validate(x, "strict")
    manifest <- ngeo_object_manifest(x)
    values <- if (inherits(x$values, "ngeo_file_values")) {
      list(file_identity = ngeo_file_values_identity(x$values))
    } else if (is.null(x$values)) {
      NULL
    } else {
      count <- prod(dim(x$values))
      .ngeo_budget_assert(budget, "materialized_elements", count)
      .ngeo_budget_assert(budget, "memory_bytes", 8 * count)
      value <- as.matrix(x$values)
      list(
        dimensions = dim(value),
        dimnames = dimnames(value),
        data = as.numeric(value)
      )
    }
    return(list(
      schema = "NGCS-logical-object-1",
      manifest = unclass(manifest),
      values = values,
      labels = x$base$labels
    ))
  }
  if (is.atomic(x) || is.matrix(x) || is.data.frame(x) || is.list(x)) {
    .ngeo_budget_assert(budget, "materialized_elements", length(x))
    return(list(
      schema = "NGCS-logical-value-1",
      class = class(x),
      dimensions = dim(x),
      names = names(x),
      value = unclass(x)
    ))
  }
  .ngeo_abort(
    "Logical hashing supports NGCS objects and JSON-compatible values.",
    "ngeo_error_logical_hash"
  )
}

#' Compute a scientific logical hash independent of history timestamps
#'
#' @param x An NGCS object or JSON-compatible value.
#' @param budget Resource limits for reading in-memory values.
#' @return A lowercase SHA-256 string.
#' @examples
#' point <- ngeo_point(
#'   matrix(c(0, 0, 1, 0, 1, 1), ncol = 2, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 3))
#' )
#' ngeo_logical_hash(point)
#' @export
ngeo_logical_hash <- function(
    x,
    budget = ngeo_resource_budget()) {
  payload <- .ngeo_logical_payload(x, budget)
  digest::digest(
    .ngeo_manifest_json(payload),
    algo = "sha256",
    serialize = FALSE
  )
}

#' Declare one replayable neurogeo operation
#'
#' @param id Unique output artifact identifier.
#' @param operation A supported deterministic neurogeo operation.
#' @param inputs Named character mapping from formal arguments to artifacts.
#' @param arguments JSON-compatible non-artifact arguments.
#' @return An `ngeo_replay_step`.
#' @export
ngeo_replay_step <- function(
    id,
    operation,
    inputs,
    arguments = list()) {
  .ngeo_assert_scalar_character(id, "id")
  .ngeo_assert_scalar_character(operation, "operation")
  supported <- .ngeo_replay_operations()
  if (!operation %in% supported ||
      !is.character(inputs) || !length(inputs) ||
      is.null(names(inputs)) || any(!nzchar(names(inputs))) ||
      anyDuplicated(names(inputs)) || anyNA(inputs) ||
      any(!nzchar(inputs)) || !is.list(arguments) ||
      (!is.null(names(arguments)) &&
        (any(!nzchar(names(arguments))) || anyDuplicated(names(arguments)))) ||
      any(names(arguments) %in% names(inputs))) {
    .ngeo_abort(
      "Replay step operation, inputs, or arguments are invalid.",
      "ngeo_error_replay_step"
    )
  }
  structure(
    list(
      id = id,
      operation = operation,
      inputs = as.list(inputs),
      arguments = arguments
    ),
    class = c("ngeo_replay_step", "list")
  )
}

.ngeo_replay_execute <- function(step, artifacts) {
  input_ids <- unlist(step$inputs, use.names = TRUE)
  missing <- setdiff(input_ids, names(artifacts))
  if (length(missing)) {
    .ngeo_abort(
      paste("Replay step has missing input artifacts:",
            paste(missing, collapse = ", ")),
      "ngeo_error_replay_parent"
    )
  }
  function_value <- getExportedValue("neurogeo", step$operation)
  input_arguments <- artifacts[input_ids]
  names(input_arguments) <- names(input_ids)
  restore_vector <- function(value) {
    if (is.list(value) && length(value) &&
        all(vapply(value, function(item) {
          is.atomic(item) && length(item) == 1L
        }, logical(1)))) {
      return(unlist(value, use.names = FALSE))
    }
    value
  }
  arguments <- lapply(step$arguments, restore_vector)
  do.call(function_value, c(input_arguments, arguments))
}

.ngeo_replay_dag <- function(input_hashes, steps) {
  nodes <- lapply(names(input_hashes), function(id) {
    list(id = id, type = "input", logical_hash = input_hashes[[id]])
  })
  edges <- list()
  for (step in steps) {
    nodes[[length(nodes) + 1L]] <- list(
      id = step$id,
      type = paste0("operation:", step$operation),
      logical_hash = step$expected_logical_hash
    )
    for (role in names(step$inputs)) {
      edges[[length(edges) + 1L]] <- list(
        from = step$inputs[[role]], to = step$id, role = role
      )
    }
  }
  ngeo_history_dag(nodes, edges)
}

#' Record a deterministic replay manifest by executing declared steps
#'
#' @param inputs Uniquely named input artifacts.
#' @param steps Ordered `ngeo_replay_step` objects.
#' @param outputs Optional artifact identifiers to expose as outputs.
#' @param budget Resource limits for logical hashing.
#' @return A validated `ngeo_replay_manifest`.
#' @export
ngeo_record_replay <- function(
    inputs,
    steps,
    outputs = NULL,
    budget = ngeo_resource_budget()) {
  inputs <- .ngeo_named_list(inputs, "inputs")
  if (!is.list(steps) || !length(steps) ||
      any(!vapply(steps, inherits, logical(1), "ngeo_replay_step"))) {
    .ngeo_abort(
      "`steps` must contain replay step declarations.",
      "ngeo_error_replay"
    )
  }
  step_ids <- vapply(steps, `[[`, character(1), "id")
  if (anyDuplicated(c(names(inputs), step_ids))) {
    .ngeo_abort(
      "Input and step artifact identifiers must be unique.",
      "ngeo_error_replay"
    )
  }
  artifacts <- inputs
  input_hashes <- lapply(inputs, ngeo_logical_hash, budget = budget)
  recorded <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    value <- .ngeo_replay_execute(step, artifacts)
    artifacts[[step$id]] <- value
    recorded[[i]] <- c(
      unclass(step),
      list(expected_logical_hash = ngeo_logical_hash(value, budget))
    )
  }
  outputs <- outputs %||% step_ids[[length(step_ids)]]
  if (!is.character(outputs) || !length(outputs) || anyNA(outputs) ||
      any(!outputs %in% names(artifacts)) || anyDuplicated(outputs)) {
    .ngeo_abort(
      "`outputs` must select unique recorded artifacts.",
      "ngeo_error_replay"
    )
  }
  output_hashes <- lapply(
    artifacts[outputs], ngeo_logical_hash, budget = budget
  )
  manifest <- list(
    schema = "NGCS-replay-manifest-1",
    specification = "NGCS 3.5",
    environment = unclass(ngeo_environment_snapshot()),
    input_hashes = input_hashes,
    steps = recorded,
    output_hashes = output_hashes,
    dag = unclass(.ngeo_replay_dag(input_hashes, recorded)),
    canonical_sha256 = NULL
  )
  manifest$canonical_sha256 <- .ngeo_manifest_sha256(manifest)
  class(manifest) <- c("ngeo_replay_manifest", "list")
  ngeo_validate_replay_manifest(manifest)
  manifest
}

.ngeo_replay_error <- function(message, class, report = NULL) {
  condition <- structure(
    list(message = message, call = NULL, report = report),
    class = c(class, "ngeo_error_replay", "ngeo_error",
              "error", "condition")
  )
  stop(condition)
}

#' Validate a replay manifest without executing it
#'
#' @param manifest A replay manifest list.
#' @param mode Return a report or raise a classed error.
#' @return An `ngeo_replay_validation_report`.
#' @export
ngeo_validate_replay_manifest <- function(
    manifest,
    mode = c("report", "error")) {
  mode <- match.arg(mode)
  issues <- .ngeo_issue_frame()
  add_error <- function(code, message) {
    issues <<- rbind(
      issues,
      .ngeo_issue_frame("error", code, "ngeo_error_replay", message)
    )
  }
  required <- c(
    "schema", "specification", "environment", "input_hashes",
    "steps", "output_hashes", "dag", "canonical_sha256"
  )
  if (!is.list(manifest) || any(!required %in% names(manifest))) {
    add_error("REPLAY_STRUCTURE", "Replay manifest fields are incomplete.")
  } else {
    if (!identical(manifest$schema, "NGCS-replay-manifest-1") ||
        !identical(manifest$specification, "NGCS 3.5")) {
      add_error("REPLAY_SCHEMA", "Replay manifest schema is unsupported.")
    }
    if (!.ngeo_hash_string(manifest$canonical_sha256) ||
        !identical(
          manifest$canonical_sha256,
          .ngeo_manifest_sha256(manifest)
        )) {
      add_error("REPLAY_HASH", "Replay manifest SHA-256 differs.")
    }
    input_hashes <- unlist(manifest$input_hashes, use.names = TRUE)
    output_hashes <- unlist(manifest$output_hashes, use.names = TRUE)
    if (!length(input_hashes) || is.null(names(input_hashes)) ||
        any(!vapply(input_hashes, .ngeo_hash_string, logical(1))) ||
        !length(output_hashes) || is.null(names(output_hashes)) ||
        any(!vapply(output_hashes, .ngeo_hash_string, logical(1)))) {
      add_error("REPLAY_ARTIFACT_HASH", "Artifact hashes are invalid.")
    }
    step_ids <- character()
    available <- names(input_hashes)
    for (step in manifest$steps) {
      valid_step <- is.list(step) &&
        all(c(
          "id", "operation", "inputs", "arguments",
          "expected_logical_hash"
        ) %in% names(step)) &&
        is.character(step$id) && length(step$id) == 1L &&
        !is.na(step$id) && nzchar(step$id) &&
        is.character(step$operation) && length(step$operation) == 1L &&
        step$operation %in% .ngeo_replay_operations() &&
        is.list(step$inputs) && length(step$inputs) > 0L &&
        !is.null(names(step$inputs)) &&
        all(nzchar(names(step$inputs))) &&
        !anyDuplicated(names(step$inputs)) &&
        is.list(step$arguments) &&
        .ngeo_hash_string(step$expected_logical_hash)
      if (!valid_step) {
        add_error("REPLAY_STEP", "A replay step is invalid.")
        next
      }
      parent <- unlist(step$inputs, use.names = TRUE)
      if (!is.character(parent) || anyNA(parent) ||
          any(!nzchar(parent)) || any(!parent %in% available)) {
        add_error(
          "REPLAY_PARENT",
          "A replay step references a missing or forward parent."
        )
      }
      if (step$id %in% c(available, step_ids)) {
        add_error("REPLAY_ID", "Replay artifact identifiers are duplicated.")
      }
      step_ids <- c(step_ids, step$id)
      available <- c(available, step$id)
    }
    if (any(!names(output_hashes) %in% available)) {
      add_error("REPLAY_OUTPUT", "A replay output is not produced.")
    }
    dag <- structure(
      manifest$dag,
      class = c("ngeo_history_dag", "list")
    )
    dag_failure <- tryCatch(
      {
        ngeo_validate_history_dag(dag)
        NULL
      },
      error = identity
    )
    if (inherits(dag_failure, "error")) {
      add_error("REPLAY_DAG", conditionMessage(dag_failure))
    }
  }
  report <- structure(
    list(valid = !nrow(issues), issues = issues),
    class = "ngeo_replay_validation_report"
  )
  if (!report$valid && mode == "error") {
    .ngeo_replay_error(
      paste("Replay manifest validation failed:",
            report$issues$message[[1L]]),
      "ngeo_error_replay_manifest",
      report
    )
  }
  report
}

.ngeo_environment_matches <- function(recorded, current, policy) {
  if (!.ngeo_hash_string(recorded$environment_sha256) ||
      !identical(
        recorded$environment_sha256,
        .ngeo_environment_sha256(recorded)
      )) {
    return(FALSE)
  }
  if (policy == "exact") {
    return(identical(
      recorded$environment_sha256,
      current$environment_sha256
    ))
  }
  recorded_major <- strsplit(recorded$r_version, ".", fixed = TRUE)[[1L]][1:2]
  current_major <- strsplit(current$r_version, ".", fixed = TRUE)[[1L]][1:2]
  identical(recorded$specification, current$specification) &&
    identical(recorded_major, current_major) &&
    identical(recorded$platform, current$platform)
}

#' Replay and verify a recorded neurogeo workflow
#'
#' @param manifest A validated replay manifest.
#' @param inputs Named input artifacts.
#' @param environment_policy Require an exact or compatible environment.
#' @param budget Resource limits for logical hashing.
#' @return An `ngeo_replay_result` containing verified outputs.
#' @export
ngeo_replay <- function(
    manifest,
    inputs,
    environment_policy = c("exact", "compatible"),
    budget = ngeo_resource_budget()) {
  ngeo_validate_replay_manifest(manifest, "error")
  inputs <- .ngeo_named_list(inputs, "inputs")
  environment_policy <- match.arg(environment_policy)
  current <- ngeo_environment_snapshot()
  if (!.ngeo_environment_matches(
    manifest$environment, current, environment_policy
  )) {
    .ngeo_replay_error(
      "Replay environment does not satisfy the recorded policy.",
      "ngeo_error_replay_environment"
    )
  }
  expected_inputs <- unlist(manifest$input_hashes, use.names = TRUE)
  if (!setequal(names(inputs), names(expected_inputs))) {
    .ngeo_replay_error(
      "Replay input identifiers differ from the manifest.",
      "ngeo_error_replay_input"
    )
  }
  actual_inputs <- vapply(
    inputs[names(expected_inputs)],
    ngeo_logical_hash,
    character(1),
    budget = budget
  )
  if (!identical(unname(actual_inputs), unname(expected_inputs))) {
    .ngeo_replay_error(
      "Replay input logical hash differs from the recorded value.",
      "ngeo_error_replay_mutation"
    )
  }
  artifacts <- inputs
  for (recorded in manifest$steps) {
    step <- structure(
      recorded[c("id", "operation", "inputs", "arguments")],
      class = c("ngeo_replay_step", "list")
    )
    value <- .ngeo_replay_execute(step, artifacts)
    actual_hash <- ngeo_logical_hash(value, budget)
    if (!identical(actual_hash, recorded$expected_logical_hash)) {
      .ngeo_replay_error(
        paste("Replay output differs at step", recorded$id),
        "ngeo_error_replay_output"
      )
    }
    artifacts[[recorded$id]] <- value
  }
  output_ids <- names(manifest$output_hashes)
  output <- artifacts[output_ids]
  actual_output <- vapply(
    output, ngeo_logical_hash, character(1), budget = budget
  )
  expected_output <- unlist(manifest$output_hashes, use.names = TRUE)
  if (!identical(unname(actual_output), unname(expected_output))) {
    .ngeo_replay_error(
      "Replay final output hashes differ.",
      "ngeo_error_replay_output"
    )
  }
  structure(
    list(
      outputs = output,
      output_hashes = as.list(actual_output),
      manifest_sha256 = manifest$canonical_sha256,
      verified = TRUE,
      environment_policy = environment_policy
    ),
    class = "ngeo_replay_result"
  )
}

#' Write or read an NGCS replay manifest
#'
#' @param manifest A valid replay manifest.
#' @param path JSON path.
#' @param overwrite Whether to replace an existing manifest atomically.
#' @return The atomic output or a validated replay manifest.
#' @name ngeo_replay_manifest_io
NULL

#' @rdname ngeo_replay_manifest_io
#' @export
write_ngeo_replay_manifest <- function(
    manifest,
    path,
    overwrite = FALSE) {
  .ngeo_require("jsonlite", "replay manifest writing")
  ngeo_validate_replay_manifest(manifest, "error")
  .ngeo_atomic_write(
    path,
    function(temporary) {
      jsonlite::write_json(
        unclass(manifest), temporary, pretty = TRUE,
        auto_unbox = TRUE, null = "null", na = "string", digits = NA
      )
    },
    overwrite
  )
}

#' @rdname ngeo_replay_manifest_io
#' @export
read_ngeo_replay_manifest <- function(path) {
  .ngeo_require("jsonlite", "replay manifest reading")
  .ngeo_assert_scalar_character(path, "path")
  manifest <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      .ngeo_abort(
        paste("Could not parse replay manifest:", conditionMessage(error)),
        "ngeo_error_io"
      )
    }
  )
  ngeo_validate_replay_manifest(manifest, "error")
  structure(manifest, class = c("ngeo_replay_manifest", "list"))
}

.ngeo_artifact_entries <- function(paths, root, roles) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  absolute <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  root_prefix <- paste0(sub("/+$", "", root), "/")
  within <- startsWith(paste0(absolute, ifelse(
    file.info(absolute)$isdir, "/", ""
  )), root_prefix)
  if (any(!within) || any(file.info(absolute)$isdir)) {
    .ngeo_abort(
      "Every artifact must be a file below the declared root.",
      "ngeo_error_artifact_scope"
    )
  }
  relative <- substring(absolute, nchar(root_prefix) + 1L)
  entries <- lapply(seq_along(absolute), function(i) {
    list(
      path = relative[[i]],
      size = unname(file.info(absolute[[i]])$size),
      sha256 = digest::digest(
        absolute[[i]], algo = "sha256", file = TRUE, serialize = FALSE
      ),
      role = roles[[i]],
      complete = TRUE
    )
  })
  entries[order(relative)]
}

#' Inventory portable artifacts with content integrity
#'
#' @param paths Existing artifact files.
#' @param root Root below which paths are represented.
#' @param roles Optional artifact roles aligned with `paths`.
#' @return An immutable `ngeo_artifact_manifest`.
#' @export
ngeo_artifact_manifest <- function(
    paths,
    root = ".",
    roles = NULL) {
  if (!is.character(paths) || !length(paths) || anyNA(paths) ||
      any(!file.exists(paths)) || anyDuplicated(paths)) {
    .ngeo_abort(
      "`paths` must identify unique existing artifact files.",
      "ngeo_error_artifact"
    )
  }
  roles <- roles %||% rep.int("artifact", length(paths))
  if (!is.character(roles) || length(roles) != length(paths) ||
      anyNA(roles) || any(!nzchar(roles))) {
    .ngeo_abort(
      "Artifact roles must align with paths.",
      "ngeo_error_artifact"
    )
  }
  manifest <- list(
    schema = "NGCS-artifact-manifest-1",
    specification = "NGCS 3.5",
    entries = .ngeo_artifact_entries(paths, root, roles),
    complete = TRUE,
    canonical_sha256 = NULL
  )
  manifest$canonical_sha256 <- .ngeo_manifest_sha256(manifest)
  class(manifest) <- c("ngeo_artifact_manifest", "list")
  ngeo_validate_artifact_manifest(manifest)
  manifest
}

#' Validate artifact metadata and optionally verify every file
#'
#' @param manifest An artifact manifest.
#' @param root Optional filesystem root for content verification.
#' @param mode Return a report or raise a classed error.
#' @return An `ngeo_artifact_validation_report`.
#' @export
ngeo_validate_artifact_manifest <- function(
    manifest,
    root = NULL,
    mode = c("report", "error")) {
  mode <- match.arg(mode)
  issues <- .ngeo_issue_frame()
  add_error <- function(code, message) {
    issues <<- rbind(
      issues,
      .ngeo_issue_frame("error", code, "ngeo_error_artifact", message)
    )
  }
  required <- c(
    "schema", "specification", "entries", "complete", "canonical_sha256"
  )
  if (!is.list(manifest) || any(!required %in% names(manifest)) ||
      !identical(manifest$schema, "NGCS-artifact-manifest-1") ||
      !identical(manifest$specification, "NGCS 3.5") ||
      !is.list(manifest$entries) || !length(manifest$entries)) {
    add_error("ARTIFACT_STRUCTURE", "Artifact manifest structure is invalid.")
  } else {
    path <- vapply(manifest$entries, function(entry) {
      valid <- is.list(entry) &&
        all(c("path", "size", "sha256", "role", "complete") %in%
              names(entry)) &&
        is.character(entry$path) && length(entry$path) == 1L &&
        !is.na(entry$path) && nzchar(entry$path) &&
        !grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", entry$path) &&
        !grepl("^([A-Za-z]:|[/\\\\])", entry$path) &&
        is.numeric(entry$size) && length(entry$size) == 1L &&
        !is.na(entry$size) && entry$size >= 0 &&
        .ngeo_hash_string(entry$sha256) &&
        is.character(entry$role) && length(entry$role) == 1L &&
        !is.na(entry$role) && nzchar(entry$role) &&
        identical(entry$complete, TRUE)
      if (!valid) {
        add_error("ARTIFACT_ENTRY", "An artifact entry is invalid.")
        return("")
      }
      entry$path
    }, character(1))
    if (anyDuplicated(path[path != ""])) {
      add_error("ARTIFACT_PATH", "Artifact paths must be unique.")
    }
    if (!identical(manifest$complete, TRUE)) {
      add_error("ARTIFACT_INCOMPLETE", "Artifact batch is incomplete.")
    }
    if (!.ngeo_hash_string(manifest$canonical_sha256) ||
        !identical(
          manifest$canonical_sha256,
          .ngeo_manifest_sha256(manifest)
        )) {
      add_error("ARTIFACT_HASH", "Artifact manifest SHA-256 differs.")
    }
    if (!is.null(root) && !any(path == "")) {
      .ngeo_assert_scalar_character(root, "root")
      for (i in seq_along(path)) {
        artifact <- file.path(root, path[[i]])
        if (!file.exists(artifact)) {
          add_error("ARTIFACT_MISSING", paste("Missing artifact:", path[[i]]))
        } else {
          info <- file.info(artifact)
          hash <- digest::digest(
            artifact, algo = "sha256", file = TRUE, serialize = FALSE
          )
          if (as.double(info$size) !=
                as.double(manifest$entries[[i]]$size) ||
              !identical(hash, manifest$entries[[i]]$sha256)) {
            add_error(
              "ARTIFACT_CORRUPT",
              paste("Artifact content differs:", path[[i]])
            )
          }
        }
      }
    }
  }
  report <- structure(
    list(valid = !nrow(issues), issues = issues),
    class = "ngeo_artifact_validation_report"
  )
  if (!report$valid && mode == "error") {
    .ngeo_abort(
      paste("Artifact validation failed:", report$issues$message[[1L]]),
      "ngeo_error_artifact"
    )
  }
  report
}

#' Write or read a portable artifact manifest
#'
#' @param manifest A valid artifact manifest.
#' @param path JSON path.
#' @param root Optional root used to verify referenced artifacts.
#' @param overwrite Whether to replace an existing manifest atomically.
#' @return The atomic output or a validated artifact manifest.
#' @name ngeo_artifact_manifest_io
NULL

#' @rdname ngeo_artifact_manifest_io
#' @export
write_ngeo_artifact_manifest <- function(
    manifest,
    path,
    root = NULL,
    overwrite = FALSE) {
  .ngeo_require("jsonlite", "artifact manifest writing")
  ngeo_validate_artifact_manifest(manifest, root, "error")
  .ngeo_atomic_write(
    path,
    function(temporary) {
      jsonlite::write_json(
        unclass(manifest), temporary, pretty = TRUE,
        auto_unbox = TRUE, null = "null", na = "string", digits = NA
      )
    },
    overwrite
  )
}

#' @rdname ngeo_artifact_manifest_io
#' @export
read_ngeo_artifact_manifest <- function(path, root = NULL) {
  .ngeo_require("jsonlite", "artifact manifest reading")
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  ngeo_validate_artifact_manifest(manifest, root, "error")
  structure(manifest, class = c("ngeo_artifact_manifest", "list"))
}

.ngeo_batch_manifest <- function(artifacts, entities) {
  batch <- list(
    schema = "NGCS-batch-manifest-1",
    specification = "NGCS 3.5",
    scope = "derivative_only",
    entities = entities,
    artifacts = unclass(artifacts),
    complete = TRUE,
    canonical_sha256 = NULL
  )
  batch$canonical_sha256 <- .ngeo_manifest_sha256(batch)
  class(batch) <- c("ngeo_batch_manifest", "list")
  batch
}

#' Validate an atomic derivative batch
#'
#' @param manifest An NGCS batch manifest.
#' @param directory Optional directory used to verify all artifacts.
#' @param mode Return a report or raise a classed error.
#' @return An `ngeo_batch_validation_report`.
#' @export
ngeo_validate_artifact_batch <- function(
    manifest,
    directory = NULL,
    mode = c("report", "error")) {
  mode <- match.arg(mode)
  valid_structure <- is.list(manifest) &&
    all(c(
      "schema", "specification", "scope", "entities",
      "artifacts", "complete", "canonical_sha256"
    ) %in% names(manifest)) &&
    identical(manifest$schema, "NGCS-batch-manifest-1") &&
    identical(manifest$specification, "NGCS 3.5") &&
    identical(manifest$scope, "derivative_only") &&
    is.list(manifest$entities) &&
    identical(manifest$complete, TRUE) &&
    .ngeo_hash_string(manifest$canonical_sha256) &&
    identical(
      manifest$canonical_sha256,
      .ngeo_manifest_sha256(manifest)
    )
  artifact_report <- if (valid_structure) {
    ngeo_validate_artifact_manifest(
      manifest$artifacts, directory, "report"
    )
  } else {
    NULL
  }
  issues <- if (!valid_structure) {
    .ngeo_issue_frame(
      "error", "BATCH_STRUCTURE", "ngeo_error_artifact_batch",
      "Batch manifest structure, scope, completeness, or hash is invalid."
    )
  } else {
    artifact_report$issues
  }
  if (valid_structure && !is.null(directory)) {
    artifact_manifest_path <- file.path(directory, "artifacts.json")
    if (!file.exists(artifact_manifest_path)) {
      issues <- rbind(
        issues,
        .ngeo_issue_frame(
          "error", "BATCH_ARTIFACT_MANIFEST",
          "ngeo_error_artifact_batch",
          "Batch artifact manifest is missing."
        )
      )
    } else {
      .ngeo_require("jsonlite", "artifact batch validation")
      artifact_manifest <- tryCatch(
        jsonlite::fromJSON(
          artifact_manifest_path,
          simplifyVector = FALSE
        ),
        error = function(...) NULL
      )
      artifact_manifest_report <- if (is.null(artifact_manifest)) {
        NULL
      } else {
        ngeo_validate_artifact_manifest(
          artifact_manifest, directory, "report"
        )
      }
      if (is.null(artifact_manifest_report) ||
          !artifact_manifest_report$valid ||
          !identical(
            artifact_manifest$canonical_sha256,
            manifest$artifacts$canonical_sha256
          )) {
        issues <- rbind(
          issues,
          .ngeo_issue_frame(
            "error", "BATCH_ARTIFACT_MANIFEST",
            "ngeo_error_artifact_batch",
            "Batch artifact manifest differs from the embedded manifest."
          )
        )
      }
    }
  }
  report <- structure(
    list(valid = !nrow(issues), issues = issues),
    class = "ngeo_batch_validation_report"
  )
  if (!report$valid && mode == "error") {
    .ngeo_abort(
      paste("Artifact batch validation failed:",
            report$issues$message[[1L]]),
      "ngeo_error_artifact_batch"
    )
  }
  report
}

#' Atomically publish a complete derivative artifact batch
#'
#' @param directory New batch directory.
#' @param files Relative artifact paths.
#' @param writers Writer functions aligned with `files`.
#' @param roles Optional artifact roles.
#' @param manifest_name Batch manifest basename.
#' @param entities BIDS-related derivative entities.
#' @param overwrite Whether to atomically replace an existing batch directory.
#' @return A verified `ngeo_batch_manifest`.
#' @export
ngeo_write_artifact_batch <- function(
    directory,
    files,
    writers,
    roles = NULL,
    manifest_name = "manifest.json",
    entities = list(),
    overwrite = FALSE) {
  .ngeo_require("jsonlite", "artifact batch writing")
  .ngeo_assert_scalar_character(directory, "directory")
  .ngeo_assert_scalar_character(manifest_name, "manifest_name")
  invalid_path <- function(path) {
    !nzchar(path) ||
      grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", path) ||
      grepl("^([A-Za-z]:|[/\\\\])", path)
  }
  if (!is.character(files) || !length(files) || anyNA(files) ||
      any(vapply(files, invalid_path, logical(1))) ||
      anyDuplicated(files) || manifest_name %in% files ||
      identical(manifest_name, "artifacts.json") ||
      invalid_path(manifest_name) ||
      !is.list(writers) || length(writers) != length(files) ||
      any(!vapply(writers, is.function, logical(1))) ||
      !is.list(entities)) {
    .ngeo_abort(
      "Batch files, writers, manifest name, or entities are invalid.",
      "ngeo_error_artifact_batch"
    )
  }
  roles <- roles %||% rep.int("artifact", length(files))
  if (!is.character(roles) || length(roles) != length(files) ||
      anyNA(roles) || any(!nzchar(roles))) {
    .ngeo_abort(
      "Batch roles must align with files.",
      "ngeo_error_artifact_batch"
    )
  }
  directory <- normalizePath(
    directory, winslash = "/", mustWork = FALSE
  )
  if (dir.exists(directory) && !isTRUE(overwrite)) {
    .ngeo_abort(
      "Artifact batch directory exists; use `overwrite = TRUE`.",
      "ngeo_error_overwrite"
    )
  }
  parent <- dirname(directory)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(paste0(".", basename(directory), "-"), tmpdir = parent)
  dir.create(stage)
  committed <- FALSE
  on.exit(if (!committed) unlink(stage, recursive = TRUE), add = TRUE)
  for (i in seq_along(files)) {
    target <- file.path(stage, files[[i]])
    .ngeo_atomic_write(target, writers[[i]])
  }
  artifacts <- ngeo_artifact_manifest(
    file.path(stage, files), root = stage, roles = roles
  )
  batch <- .ngeo_batch_manifest(artifacts, entities)
  write_ngeo_artifact_manifest(
    artifacts,
    file.path(stage, "artifacts.json"),
    root = stage
  )
  ngeo_validate_artifact_batch(batch, stage, "error")
  .ngeo_atomic_write(
    file.path(stage, manifest_name),
    function(temporary) {
      jsonlite::write_json(
        unclass(batch), temporary, pretty = TRUE,
        auto_unbox = TRUE, null = "null", na = "string", digits = NA
      )
    }
  )
  backup <- NULL
  if (dir.exists(directory)) {
    backup <- tempfile(
      paste0(".", basename(directory), "-backup-"), tmpdir = parent
    )
    if (!file.rename(directory, backup)) {
      .ngeo_abort(
        "Could not stage replacement of the existing artifact batch.",
        "ngeo_error_io"
      )
    }
  }
  if (!file.rename(stage, directory)) {
    if (!is.null(backup)) file.rename(backup, directory)
    .ngeo_abort(
      "Could not atomically publish the artifact batch.",
      "ngeo_error_io"
    )
  }
  committed <- TRUE
  if (!is.null(backup)) unlink(backup, recursive = TRUE)
  ngeo_validate_artifact_batch(batch, directory, "error")
  batch
}

#' Read and verify a derivative artifact batch before use
#'
#' @param directory Batch directory.
#' @param manifest_name Batch manifest basename.
#' @return A verified `ngeo_batch_manifest`.
#' @export
ngeo_read_artifact_batch <- function(
    directory,
    manifest_name = "manifest.json") {
  .ngeo_require("jsonlite", "artifact batch reading")
  path <- file.path(directory, manifest_name)
  if (!file.exists(path)) {
    .ngeo_abort(
      "Artifact batch manifest is missing.",
      "ngeo_error_artifact_batch"
    )
  }
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  ngeo_validate_artifact_batch(manifest, directory, "error")
  structure(manifest, class = c("ngeo_batch_manifest", "list"))
}

#' @export
print.ngeo_history_dag <- function(x, ...) {
  cat("<ngeo_history_dag>\n  nodes: ", length(x$nodes),
      "\n  edges: ", length(x$edges),
      "\n  SHA-256: ", x$dag_sha256, "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_replay_manifest <- function(x, ...) {
  cat("<ngeo_replay_manifest>\n  steps: ", length(x$steps),
      "\n  outputs: ", length(x$output_hashes),
      "\n  SHA-256: ", x$canonical_sha256, "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_replay_result <- function(x, ...) {
  cat("<ngeo_replay_result>\n  outputs: ", length(x$outputs),
      "\n  verified: ", x$verified, "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_artifact_manifest <- function(x, ...) {
  cat("<ngeo_artifact_manifest>\n  artifacts: ", length(x$entries),
      "\n  complete: ", x$complete,
      "\n  SHA-256: ", x$canonical_sha256, "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_batch_manifest <- function(x, ...) {
  cat("<ngeo_batch_manifest>\n  scope: ", x$scope,
      "\n  artifacts: ", length(x$artifacts$entries),
      "\n  complete: ", x$complete, "\n", sep = "")
  invisible(x)
}
