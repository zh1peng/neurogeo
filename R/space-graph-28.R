#' Compute the stable identity of a coordinate space
#'
#' @param x An `ngeo_space`.
#' @return A SHA-256 identity.
#' @export
ngeo_space_hash <- function(x) {
  if (!inherits(x, "ngeo_space")) {
    .ngeo_abort("`x` must be an `ngeo_space`.", "ngeo_error_space")
  }
  digest::digest(.ngeo_space_signature(x), algo = "sha256")
}

#' Audit two coordinate-space descriptions
#'
#' @param left,right `ngeo_space` objects.
#' @return An `ngeo_space_audit` data frame.
#' @export
ngeo_space_audit <- function(left, right) {
  if (!inherits(left, "ngeo_space") || !inherits(right, "ngeo_space")) {
    .ngeo_abort("Space audit requires two `ngeo_space` objects.",
                "ngeo_error_space")
  }
  fields <- c(
    "space_id", "kind", "units", "dimension", "structure",
    "template", "density", "resolution"
  )
  dimension <- function(space) {
    declared <- space$source_metadata$dimension
    if (!is.null(declared)) return(declared)
    if (space$kind %in% c("surface", "volume", "hybrid")) 3L else NULL
  }
  left_value <- c(
    left[c("space_id", "kind", "units")],
    list(dimension = dimension(left)),
    left[c("structure", "template", "density", "resolution")]
  )
  right_value <- c(
    right[c("space_id", "kind", "units")],
    list(dimension = dimension(right)),
    right[c("structure", "template", "density", "resolution")]
  )
  display <- function(value) {
    if (is.null(value)) "<unspecified>" else
      paste(as.character(value), collapse = " x ")
  }
  equivalent <- vapply(fields, function(field) {
    identical(left_value[[field]], right_value[[field]])
  }, logical(1))
  severity <- ifelse(
    fields %in% c("kind", "units", "dimension", "structure") & !equivalent,
    "incompatible",
    ifelse(!equivalent, "different", "match")
  )
  result <- data.frame(
    field = fields,
    left = vapply(left_value[fields], display, character(1)),
    right = vapply(right_value[fields], display, character(1)),
    match = equivalent,
    severity = severity,
    stringsAsFactors = FALSE
  )
  attr(result, "equivalent") <- all(equivalent)
  attr(result, "compatible") <- !any(severity == "incompatible")
  attr(result, "left_hash") <- ngeo_space_hash(left)
  attr(result, "right_hash") <- ngeo_space_hash(right)
  class(result) <- c("ngeo_space_audit", "data.frame")
  result
}

.ngeo_registry_hash <- function(x) {
  digest::digest(
    list(
      spaces = vapply(x$spaces, ngeo_space_hash, character(1)),
      aliases = x$aliases
    ),
    algo = "sha256"
  )
}

#' Construct a stable coordinate-space registry
#'
#' @param spaces A list of `ngeo_space` objects.
#' @param aliases Optional named character aliases targeting a unique
#'   `space_id` or exact space hash.
#' @return An `ngeo_space_registry`.
#' @export
ngeo_space_registry <- function(spaces = list(), aliases = character()) {
  if (inherits(spaces, "ngeo_space")) spaces <- list(spaces)
  if (!is.list(spaces) ||
      !all(vapply(spaces, inherits, logical(1), what = "ngeo_space"))) {
    .ngeo_abort("`spaces` must contain only `ngeo_space` objects.",
                "ngeo_error_space")
  }
  result <- structure(
    list(spaces = list(), aliases = character(), registry_hash = NULL),
    class = "ngeo_space_registry"
  )
  for (space in spaces) {
    hash <- ngeo_space_hash(space)
    if (is.null(result$spaces[[hash]])) result$spaces[[hash]] <- space
  }
  if (length(aliases)) {
    if (!is.character(aliases) || is.null(names(aliases)) ||
        any(!nzchar(names(aliases))) || anyDuplicated(names(aliases))) {
      .ngeo_abort("Aliases must be uniquely named character targets.",
                  "ngeo_error_alias")
    }
    for (alias in names(aliases)) {
      target <- aliases[[alias]]
      hash <- if (target %in% names(result$spaces)) {
        target
      } else {
        matches <- names(result$spaces)[vapply(
          result$spaces,
          function(space) identical(space$space_id, target),
          logical(1)
        )]
        if (length(matches) != 1L) {
          .ngeo_abort("Alias target is absent or ambiguous.",
                      "ngeo_error_alias")
        }
        matches[[1L]]
      }
      result$aliases[[alias]] <- hash
    }
  }
  result$registry_hash <- .ngeo_registry_hash(result)
  ngeo_validate_space_registry(result)
  result
}

#' Validate a coordinate-space registry
#'
#' @param x An `ngeo_space_registry`.
#' @return `x`, invisibly.
#' @export
ngeo_validate_space_registry <- function(x) {
  valid_spaces <- inherits(x, "ngeo_space_registry") &&
    is.list(x$spaces) &&
    all(vapply(x$spaces, inherits, logical(1), what = "ngeo_space")) &&
    identical(
      names(x$spaces) %||% character(),
      unname(vapply(x$spaces, ngeo_space_hash, character(1)))
    )
  valid_aliases <- is.character(x$aliases) &&
    (length(x$aliases) == 0L || (
      !is.null(names(x$aliases)) &&
        !anyNA(x$aliases) &&
        !anyDuplicated(names(x$aliases)) &&
        all(x$aliases %in% names(x$spaces))
    ))
  if (!valid_spaces || !valid_aliases ||
      !identical(x$registry_hash, .ngeo_registry_hash(x))) {
    .ngeo_abort("Space registry identity or contents are invalid.",
                "ngeo_error_space_registry")
  }
  invisible(x)
}

#' Add one space to a registry
#'
#' @param registry An `ngeo_space_registry`.
#' @param space An `ngeo_space`.
#' @param aliases Optional aliases for this exact space.
#' @return A new `ngeo_space_registry`.
#' @export
ngeo_register_space <- function(registry, space, aliases = character()) {
  ngeo_validate_space_registry(registry)
  if (!inherits(space, "ngeo_space") || !is.character(aliases) ||
      anyNA(aliases) || any(!nzchar(aliases)) || anyDuplicated(aliases)) {
    .ngeo_abort("Registered space or aliases are invalid.",
                "ngeo_error_alias")
  }
  result <- registry
  hash <- ngeo_space_hash(space)
  if (is.null(result$spaces[[hash]])) result$spaces[[hash]] <- space
  conflict <- aliases[aliases %in% names(result$aliases) &
                        result$aliases[aliases] != hash]
  if (length(conflict)) {
    .ngeo_abort("A registry alias already targets another space.",
                "ngeo_error_alias")
  }
  result$aliases[aliases] <- hash
  result$registry_hash <- .ngeo_registry_hash(result)
  ngeo_validate_space_registry(result)
  result
}

#' Resolve a space, exact hash, unique space ID, or alias
#'
#' @param registry An `ngeo_space_registry`.
#' @param space An `ngeo_space` or one character identifier.
#' @return The registered `ngeo_space`.
#' @export
ngeo_resolve_space <- function(registry, space) {
  ngeo_validate_space_registry(registry)
  if (inherits(space, "ngeo_space")) {
    hash <- ngeo_space_hash(space)
    if (!hash %in% names(registry$spaces)) {
      .ngeo_abort("The exact space is not registered.",
                  "ngeo_error_space_registry")
    }
    return(registry$spaces[[hash]])
  }
  .ngeo_assert_scalar_character(space, "space")
  if (space %in% names(registry$aliases)) {
    return(registry$spaces[[registry$aliases[[space]]]])
  }
  if (space %in% names(registry$spaces)) return(registry$spaces[[space]])
  matches <- registry$spaces[vapply(
    registry$spaces,
    function(candidate) identical(candidate$space_id, space),
    logical(1)
  )]
  if (length(matches) != 1L) {
    .ngeo_abort(
      if (length(matches)) {
        "Space ID is ambiguous; use an exact hash or alias."
      } else {
        "Space identifier is not registered."
      },
      "ngeo_error_space_ambiguity"
    )
  }
  matches[[1L]]
}

#' Compute the stable identity of a supplied transform
#'
#' @param x An `ngeo_transform`.
#' @return A SHA-256 identity.
#' @export
ngeo_transform_hash <- function(x) {
  if (!inherits(x, "ngeo_transform")) {
    .ngeo_abort("`x` must be an `ngeo_transform`.",
                "ngeo_error_transform")
  }
  digest::digest(
    list(
      source = ngeo_space_hash(x$source_space),
      target = ngeo_space_hash(x$target_space),
      type = x$type,
      direction = x$direction,
      method = x$method,
      interpolation = x$interpolation,
      jacobian_available = x$jacobian_available,
      source_identity = x$source,
      parameters = x$parameters
    ),
    algo = "sha256"
  )
}

.ngeo_graph_hash <- function(x) {
  digest::digest(
    list(
      registry = x$registry$registry_hash,
      edges = x$edges,
      transforms = vapply(x$transforms, ngeo_transform_hash, character(1))
    ),
    algo = "sha256"
  )
}

.ngeo_transform_endpoints <- function(transform) {
  if (!transform$direction %in% c("source_to_target", "target_to_source")) {
    .ngeo_abort("Transform direction is invalid.", "ngeo_error_transform")
  }
  if (transform$direction == "source_to_target") {
    c(
      from = ngeo_space_hash(transform$source_space),
      to = ngeo_space_hash(transform$target_space)
    )
  } else {
    c(
      from = ngeo_space_hash(transform$target_space),
      to = ngeo_space_hash(transform$source_space)
    )
  }
}

#' Construct an explicit directed transform graph
#'
#' @param registry An `ngeo_space_registry`.
#' @param transforms Supplied known transforms.
#' @param edge_ids Optional stable edge IDs.
#' @param invertible Optional inversion eligibility flags.
#' @param lossy Optional lossy-edge flags.
#' @return An `ngeo_transform_graph`.
#' @export
ngeo_transform_graph <- function(
    registry,
    transforms = list(),
    edge_ids = NULL,
    invertible = NULL,
    lossy = NULL) {
  ngeo_validate_space_registry(registry)
  if (inherits(transforms, "ngeo_transform")) transforms <- list(transforms)
  if (!is.list(transforms) ||
      !all(vapply(
        transforms, inherits, logical(1), what = "ngeo_transform"
      ))) {
    .ngeo_abort("Graph transforms are invalid.", "ngeo_error_transform_graph")
  }
  result <- structure(
    list(
      registry = registry,
      edges = data.frame(
        edge_id = character(),
        from = character(),
        to = character(),
        transform_hash = character(),
        invertible = logical(),
        lossy = logical(),
        stringsAsFactors = FALSE
      ),
      transforms = list(),
      graph_hash = NULL
    ),
    class = "ngeo_transform_graph"
  )
  if (length(transforms)) {
    edge_ids <- edge_ids %||% paste0("edge_", seq_along(transforms))
    invertible <- invertible %||% vapply(
      transforms, function(x) identical(x$type, "affine"), logical(1)
    )
    lossy <- lossy %||% vapply(
      transforms,
      function(x) !identical(x$interpolation, "none"),
      logical(1)
    )
    if (length(edge_ids) != length(transforms) ||
        length(invertible) != length(transforms) ||
        length(lossy) != length(transforms)) {
      .ngeo_abort("Graph edge metadata does not align.",
                  "ngeo_error_alignment")
    }
    for (i in seq_along(transforms)) {
      result <- ngeo_add_transform(
        result, transforms[[i]], edge_ids[[i]],
        invertible[[i]], lossy[[i]]
      )
    }
  } else {
    result$graph_hash <- .ngeo_graph_hash(result)
  }
  ngeo_validate_transform_graph(result)
  result
}

#' Add a supplied transform edge
#'
#' @param graph An `ngeo_transform_graph`.
#' @param transform A supplied known transform.
#' @param edge_id Stable unique edge ID.
#' @param invertible Whether graph search may explicitly invert this edge.
#' @param lossy Whether the edge loses support or resolution information.
#' @return A new `ngeo_transform_graph`.
#' @export
ngeo_add_transform <- function(
    graph,
    transform,
    edge_id,
    invertible = identical(transform$type, "affine"),
    lossy = !identical(transform$interpolation, "none")) {
  if (!is.null(graph$graph_hash)) ngeo_validate_transform_graph(graph)
  .ngeo_assert_scalar_character(edge_id, "edge_id")
  if (!inherits(transform, "ngeo_transform") ||
      !is.logical(invertible) || length(invertible) != 1L ||
      is.na(invertible) ||
      !is.logical(lossy) || length(lossy) != 1L || is.na(lossy)) {
    .ngeo_abort("Transform edge metadata are invalid.",
                "ngeo_error_transform_graph")
  }
  if (edge_id %in% graph$edges$edge_id) {
    .ngeo_abort("Transform edge ID is duplicated.",
                "ngeo_error_transform_graph")
  }
  if (isTRUE(invertible) && !identical(transform$type, "affine")) {
    .ngeo_abort("Only supported affine edges are invertible.",
                "ngeo_error_transform_type")
  }
  if (identical(transform$type, "affine")) ngeo_validate_transform(transform)
  endpoints <- .ngeo_transform_endpoints(transform)
  if (!all(endpoints %in% names(graph$registry$spaces))) {
    .ngeo_abort("Transform endpoints are not exact registered spaces.",
                "ngeo_error_space_registry")
  }
  result <- graph
  result$edges <- rbind(
    result$edges,
    data.frame(
      edge_id = edge_id,
      from = endpoints[["from"]],
      to = endpoints[["to"]],
      transform_hash = ngeo_transform_hash(transform),
      invertible = invertible,
      lossy = lossy,
      stringsAsFactors = FALSE
    )
  )
  result$transforms[[edge_id]] <- transform
  result$graph_hash <- .ngeo_graph_hash(result)
  ngeo_validate_transform_graph(result)
  result
}

#' Validate a transform graph and all edge identities
#'
#' @param x An `ngeo_transform_graph`.
#' @return `x`, invisibly.
#' @export
ngeo_validate_transform_graph <- function(x) {
  valid <- inherits(x, "ngeo_transform_graph") &&
    inherits(x$registry, "ngeo_space_registry") &&
    is.data.frame(x$edges) && is.list(x$transforms) &&
    identical(names(x$transforms) %||% character(), x$edges$edge_id) &&
    !anyDuplicated(x$edges$edge_id) &&
    all(x$edges$from %in% names(x$registry$spaces)) &&
    all(x$edges$to %in% names(x$registry$spaces)) &&
    all(vapply(
      x$transforms, inherits, logical(1), what = "ngeo_transform"
    ))
  if (!valid) {
    .ngeo_abort("Transform graph structure is invalid.",
                "ngeo_error_transform_graph")
  }
  ngeo_validate_space_registry(x$registry)
  current_hash <- vapply(x$transforms, ngeo_transform_hash, character(1))
  endpoint <- lapply(x$transforms, .ngeo_transform_endpoints)
  endpoint_from <- vapply(endpoint, `[[`, character(1), "from")
  endpoint_to <- vapply(endpoint, `[[`, character(1), "to")
  if (!identical(unname(current_hash), x$edges$transform_hash) ||
      !identical(unname(endpoint_from), x$edges$from) ||
      !identical(unname(endpoint_to), x$edges$to) ||
      !identical(x$graph_hash, .ngeo_graph_hash(x))) {
    .ngeo_abort("Transform graph or edge identity was mutated.",
                "ngeo_error_transform_graph_mutation")
  }
  invisible(x)
}

.ngeo_graph_arcs <- function(graph, allow_inverse) {
  forward <- data.frame(
    from = graph$edges$from,
    to = graph$edges$to,
    token = graph$edges$edge_id,
    edge_id = graph$edges$edge_id,
    reversed = rep.int(FALSE, nrow(graph$edges)),
    stringsAsFactors = FALSE
  )
  if (!isTRUE(allow_inverse)) return(forward)
  eligible <- graph$edges$invertible & !graph$edges$lossy
  if (!any(eligible)) return(forward)
  inverse <- data.frame(
    from = graph$edges$to[eligible],
    to = graph$edges$from[eligible],
    token = paste0(graph$edges$edge_id[eligible], "^-1"),
    edge_id = graph$edges$edge_id[eligible],
    reversed = rep.int(TRUE, sum(eligible)),
    stringsAsFactors = FALSE
  )
  rbind(forward, inverse)
}

.ngeo_graph_candidates <- function(graph, from, to, allow_inverse) {
  if (identical(from, to)) return(list(character()))
  arcs <- .ngeo_graph_arcs(graph, allow_inverse)
  queue <- list(list(node = from, path = character()))
  best <- list()
  best[[from]] <- 0L
  found <- list()
  found_distance <- Inf
  while (length(queue)) {
    state <- queue[[1L]]
    queue <- queue[-1L]
    distance <- length(state$path)
    if (distance >= found_distance) next
    outgoing <- arcs[arcs$from == state$node, , drop = FALSE]
    if (!nrow(outgoing)) next
    outgoing <- outgoing[order(outgoing$token), , drop = FALSE]
    for (i in seq_len(nrow(outgoing))) {
      next_distance <- distance + 1L
      next_path <- c(state$path, outgoing$token[[i]])
      next_node <- outgoing$to[[i]]
      if (identical(next_node, to)) {
        if (next_distance < found_distance) {
          found <- list(next_path)
          found_distance <- next_distance
        } else if (next_distance == found_distance) {
          found[[length(found) + 1L]] <- next_path
        }
      } else if (next_distance < found_distance) {
        previous <- best[[next_node]]
        if (is.null(previous) || next_distance <= previous) {
          best[[next_node]] <- next_distance
          queue[[length(queue) + 1L]] <- list(
            node = next_node,
            path = next_path
          )
        }
      }
    }
  }
  found
}

.ngeo_oriented_transform <- function(graph, token) {
  reversed <- grepl("[\\^]-1$", token)
  edge_id <- sub("[\\^]-1$", "", token)
  transform <- graph$transforms[[edge_id]]
  normalized <- if (transform$direction == "source_to_target") {
    transform
  } else {
    ngeo_transform(
      transform$target_space,
      transform$source_space,
      transform$type,
      method = transform$method,
      interpolation = transform$interpolation,
      jacobian_available = transform$jacobian_available,
      source = transform$source,
      parameters = transform$parameters
    )
  }
  if (reversed) ngeo_invert_transform(normalized) else normalized
}

#' Find one explicit deterministic transform path
#'
#' @param graph An `ngeo_transform_graph`.
#' @param from,to Registered spaces, hashes, IDs, or aliases.
#' @param allow_inverse Whether eligible non-lossy affine edges may be inverted.
#' @param selection Optional exact edge-token sequence resolving ambiguity.
#' @return An `ngeo_transform_path`.
#' @export
ngeo_transform_path <- function(
    graph,
    from,
    to,
    allow_inverse = FALSE,
    selection = NULL) {
  ngeo_validate_transform_graph(graph)
  from_space <- ngeo_resolve_space(graph$registry, from)
  to_space <- ngeo_resolve_space(graph$registry, to)
  from_hash <- ngeo_space_hash(from_space)
  to_hash <- ngeo_space_hash(to_space)
  candidates <- .ngeo_graph_candidates(
    graph, from_hash, to_hash, allow_inverse
  )
  if (!length(candidates)) {
    .ngeo_abort("No directed transform path exists.",
                "ngeo_error_transform_path")
  }
  if (!is.null(selection)) {
    if (!is.character(selection)) {
      .ngeo_abort("Path selection must be an edge-token sequence.",
                  "ngeo_error_argument")
    }
    match <- which(vapply(
      candidates, identical, logical(1), y = selection
    ))
    if (length(match) != 1L) {
      .ngeo_abort("Selected path is not one shortest candidate.",
                  "ngeo_error_transform_path")
    }
    chosen <- candidates[[match]]
  } else {
    if (length(candidates) != 1L) {
      .ngeo_abort(
        "Multiple shortest transform paths exist; select one explicitly.",
        "ngeo_error_transform_ambiguity"
      )
    }
    chosen <- candidates[[1L]]
  }
  transforms <- lapply(
    chosen,
    function(token) .ngeo_oriented_transform(graph, token)
  )
  applicable <- all(vapply(
    transforms, function(x) identical(x$type, "affine"), logical(1)
  ))
  composed <- if (!length(transforms)) {
    ngeo_transform(
      from_space, to_space, "affine",
      method = "identity path",
      parameters = list(matrix = diag(4))
    )
  } else if (applicable) {
    Reduce(ngeo_compose_transform, transforms)
  } else {
    NULL
  }
  edge_id <- sub("[\\^]-1$", "", chosen)
  edge_rows <- match(edge_id, graph$edges$edge_id)
  audits <- lapply(transforms, function(transform) {
    ngeo_space_audit(transform$source_space, transform$target_space)
  })
  result <- list(
    from = from_space,
    to = to_space,
    tokens = chosen,
    edge_ids = edge_id,
    edge_hashes = graph$edges$transform_hash[edge_rows],
    reversed = grepl("[\\^]-1$", chosen),
    lossy = graph$edges$lossy[edge_rows],
    transforms = transforms,
    composed = composed,
    applicable = applicable && !any(graph$edges$lossy[edge_rows]),
    audits = audits,
    graph_hash = graph$graph_hash
  )
  result$path_hash <- digest::digest(
    result[c(
      "graph_hash", "tokens", "edge_hashes", "reversed", "lossy"
    )],
    algo = "sha256"
  )
  class(result) <- "ngeo_transform_path"
  result
}

#' Diagnose cycles, ambiguity, and edge space mismatches
#'
#' @param graph An `ngeo_transform_graph`.
#' @return An `ngeo_transform_graph_diagnostics`.
#' @export
ngeo_transform_graph_diagnostics <- function(graph) {
  ngeo_validate_transform_graph(graph)
  hashes <- names(graph$registry$spaces)
  adjacency <- matrix(FALSE, length(hashes), length(hashes),
                      dimnames = list(hashes, hashes))
  if (nrow(graph$edges)) {
    adjacency[cbind(
      match(graph$edges$from, hashes),
      match(graph$edges$to, hashes)
    )] <- TRUE
  }
  reach <- adjacency
  for (k in seq_along(hashes)) {
    reach <- reach | outer(reach[, k], reach[k, ], "&")
  }
  ambiguous <- list()
  for (i in seq_along(hashes)) {
    for (j in seq_along(hashes)) {
      if (i == j) next
      candidate <- .ngeo_graph_candidates(
        graph, hashes[[i]], hashes[[j]], FALSE
      )
      if (length(candidate) > 1L) {
        ambiguous[[length(ambiguous) + 1L]] <- c(
          from = hashes[[i]], to = hashes[[j]],
          paths = length(candidate)
        )
      }
    }
  }
  mismatch <- lapply(seq_len(nrow(graph$edges)), function(i) {
    audit <- ngeo_space_audit(
      graph$registry$spaces[[graph$edges$from[[i]]]],
      graph$registry$spaces[[graph$edges$to[[i]]]]
    )
    data.frame(
      edge_id = graph$edges$edge_id[[i]],
      incompatible_fields = paste(
        audit$field[audit$severity == "incompatible"],
        collapse = ","
      ),
      different_fields = paste(
        audit$field[audit$severity == "different"],
        collapse = ","
      ),
      stringsAsFactors = FALSE
    )
  })
  result <- list(
    spaces = length(hashes),
    edges = nrow(graph$edges),
    cycle_space_hashes = hashes[diag(reach)],
    ambiguous_pairs = if (length(ambiguous)) {
      as.data.frame(do.call(rbind, ambiguous), stringsAsFactors = FALSE)
    } else {
      data.frame(from = character(), to = character(), paths = integer())
    },
    edge_audit = if (length(mismatch)) do.call(rbind, mismatch) else
      data.frame(
        edge_id = character(),
        incompatible_fields = character(),
        different_fields = character()
      ),
    graph_hash = graph$graph_hash
  )
  class(result) <- "ngeo_transform_graph_diagnostics"
  result
}

#' Export auditable transform-path provenance
#'
#' @param path An `ngeo_transform_path`.
#' @return A serializable provenance list.
#' @export
ngeo_transform_path_provenance <- function(path) {
  if (!inherits(path, "ngeo_transform_path")) {
    .ngeo_abort("`path` must be an `ngeo_transform_path`.",
                "ngeo_error_transform_path")
  }
  list(
    schema = "NGCS-transform-path-1",
    from_space_hash = ngeo_space_hash(path$from),
    to_space_hash = ngeo_space_hash(path$to),
    graph_hash = path$graph_hash,
    path_hash = path$path_hash,
    tokens = path$tokens,
    edge_ids = path$edge_ids,
    edge_hashes = path$edge_hashes,
    reversed = path$reversed,
    lossy = path$lossy,
    applicable = path$applicable
  )
}

#' Apply one explicitly authorized affine transform path
#'
#' @param x An `ngeo` dataset in the path source space.
#' @param path An `ngeo_transform_path`.
#' @param authorize Must be explicitly `TRUE`.
#' @return A geometry-transformed `ngeo` copy.
#' @export
ngeo_apply_transform_path <- function(x, path, authorize = FALSE) {
  if (!inherits(path, "ngeo_transform_path") || !isTRUE(authorize)) {
    .ngeo_abort(
      "Transform-path application requires explicit `authorize = TRUE`.",
      "ngeo_error_authorization"
    )
  }
  if (!isTRUE(path$applicable) || is.null(path$composed)) {
    .ngeo_abort(
      "The selected path is lossy or not affine-applicable.",
      "ngeo_error_transform_type"
    )
  }
  result <- ngeo_apply_transform(x, path$composed)
  result$provenance$operations <- c(
    result$provenance$operations,
    list(.ngeo_operation(
      "ngeo_apply_transform_path",
      ngeo_transform_path_provenance(path)
    ))
  )
  ngeo_validate(result, "strict")
  result
}

#' @export
print.ngeo_space_registry <- function(x, ...) {
  cat("<ngeo_space_registry>\n  spaces: ", length(x$spaces),
      "\n  aliases: ", length(x$aliases), "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_transform_graph <- function(x, ...) {
  cat("<ngeo_transform_graph>\n  spaces: ", length(x$registry$spaces),
      "\n  edges: ", nrow(x$edges), "\n", sep = "")
  invisible(x)
}

#' @export
print.ngeo_transform_path <- function(x, ...) {
  cat("<ngeo_transform_path>\n  edges: ", length(x$tokens),
      "\n  applicable: ", x$applicable, "\n", sep = "")
  invisible(x)
}
