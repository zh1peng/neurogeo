.ngeo_relation_base <- function(base) {
  spatial <- if (inherits(base, "ngeo")) base$base else
    if (inherits(base, "ngeo_layer_view")) base$base else base
  if (!inherits(spatial, "ngeo_base")) {
    .ngeo_abort(
      "`base` must be an `ngeo`, `ngeo_layer_view`, or `ngeo_base` object.",
      "ngeo_error_argument",
      field = "base"
    )
  }
  list(
    hash = base_hash(spatial),
    signature = base_signature(spatial),
    type = spatial$type,
    element_id = as.character(spatial$elements$element_id)
  )
}

.ngeo_relation_endpoint <- function(value, binding, name) {
  n <- length(binding$element_id)
  if (is.factor(value)) value <- as.character(value)
  if (is.character(value)) {
    index <- match(value, binding$element_id)
    if (anyNA(index)) {
      .ngeo_abort(
        sprintf("`data$%s` contains unknown base element IDs.", name),
        "ngeo_error_relation",
        field = paste0("data$", name)
      )
    }
  } else {
    index <- .ngeo_as_integer(value, paste0("data$", name))
    if (any(index < 1L | index > n)) {
      .ngeo_abort(
        sprintf("`data$%s` positions must be between 1 and %d.", name, n),
        "ngeo_error_relation",
        field = paste0("data$", name)
      )
    }
  }
  binding$element_id[index]
}

.ngeo_relation_matrix_values <- function(data) {
  if (inherits(data, "Matrix")) {
    summary <- Matrix::summary(data)
    if ("x" %in% names(summary)) summary$x else rep.int(1, nrow(summary))
  } else {
    as.vector(data)
  }
}

.ngeo_relation_data <- function(data, binding, directed, weighted) {
  n <- length(binding$element_id)
  if (is.data.frame(data)) {
    if (any(!c("from", "to") %in% names(data))) {
      .ngeo_abort(
        "Relation edge lists must contain `from` and `to` columns.",
        "ngeo_error_relation",
        field = "data"
      )
    }
    if (weighted && !"value" %in% names(data)) {
      .ngeo_abort(
        "Weighted relation edge lists must contain a numeric `value` column.",
        "ngeo_error_relation",
        field = "data$value"
      )
    }
    data$from <- .ngeo_relation_endpoint(data$from, binding, "from")
    data$to <- .ngeo_relation_endpoint(data$to, binding, "to")
    if ("value" %in% names(data)) {
      if (!is.numeric(data$value) || any(is.infinite(data$value))) {
        .ngeo_abort(
          "`data$value` must contain numeric finite or missing values.",
          "ngeo_error_relation",
          field = "data$value"
        )
      }
      if (!weighted && any(!is.na(data$value) & !data$value %in% c(0, 1))) {
        .ngeo_abort(
          "Unweighted relation values must be zero, one, or missing.",
          "ngeo_error_relation",
          field = "data$value"
        )
      }
    }
    from <- match(data$from, binding$element_id)
    to <- match(data$to, binding$element_id)
    if (!directed) {
      reverse <- from > to
      if (any(reverse)) {
        temporary <- data$from[reverse]
        data$from[reverse] <- data$to[reverse]
        data$to[reverse] <- temporary
        temporary <- from[reverse]
        from[reverse] <- to[reverse]
        to[reverse] <- temporary
      }
    }
    key <- if (directed) {
      paste(from, to, sep = "\r")
    } else {
      paste(pmin(from, to), pmax(from, to), sep = "\r")
    }
    if (anyDuplicated(key)) {
      .ngeo_abort(
        "Relation edge lists must contain at most one value per pair.",
        "ngeo_error_relation",
        field = "data"
      )
    }
    rownames(data) <- NULL
    return(data)
  }

  valid_matrix <- is.matrix(data) || inherits(data, "Matrix")
  valid_storage <- is.numeric(data) || is.integer(data) || is.logical(data) ||
    (inherits(data, "Matrix") && (
      methods::is(data, "dMatrix") || methods::is(data, "iMatrix") ||
      methods::is(data, "lMatrix") || methods::is(data, "nMatrix")
    ))
  if (!valid_matrix || !valid_storage || !identical(dim(data), c(n, n))) {
    .ngeo_abort(
      sprintf(
        "Relation `data` must be an edge list or a %d-by-%d numeric/logical matrix.",
        n, n
      ),
      "ngeo_error_relation",
      field = "data"
    )
  }
  observed <- .ngeo_relation_matrix_values(data)
  if (any(is.infinite(observed))) {
    .ngeo_abort(
      "Relation matrix values must be finite or missing.",
      "ngeo_error_relation",
      field = "data"
    )
  }
  if (!weighted && any(!is.na(observed) & !observed %in% c(0, 1))) {
    .ngeo_abort(
      "Unweighted relation matrix values must be zero, one, or missing.",
      "ngeo_error_relation",
      field = "data"
    )
  }
  symmetric <- if (inherits(data, "Matrix")) {
    Matrix::isSymmetric(data)
  } else {
    isSymmetric(data)
  }
  if (!directed && !isTRUE(symmetric)) {
    .ngeo_abort(
      "Undirected relation matrices must be symmetric.",
      "ngeo_error_relation",
      field = "data"
    )
  }
  data
}

.ngeo_relation_measure <- function(measure) {
  if (is.null(measure)) {
    measure <- ngeo_measure(
      measure_id = "relation_value",
      name = "Relation value"
    )
  }
  if (!is.data.frame(measure) || nrow(measure) != 1L) {
    .ngeo_abort(
      "`measure` must be one measurement metadata row.",
      "ngeo_error_measure",
      field = "measure"
    )
  }
  if (!"measure_id" %in% names(measure)) {
    measure$measure_id <- "relation_value"
  }
  measure <- .measures(
    measure,
    data.frame(measure_id = measure$measure_id, stringsAsFactors = FALSE)
  )
  .ngeo_validate_measures(measure)
  measure
}

#' Construct an empirical pairwise relation on a spatial base
#'
#' A relation is an optional first-class object and is never inserted into the
#' frozen five-field `ngeo` dataset. It stores empirical pairwise information,
#' such as structural or functional connectivity, morphological similarity,
#' gene coexpression, or effective connectivity. Distances, adjacency, and
#' spatial weights remain analysis objects and are rejected as relation types.
#'
#' @section When to use and when not to use:
#' Use a Relation for additional empirical pairwise measurements. Do not use it
#' for distance, adjacency, spatial weights, support mappings, or dynamical
#' operators.
#' @section Units and assumptions:
#' Matrix rows and columns or edge endpoints must follow the ordered Base.
#' Pairwise value units and support semantics come from the single `measure`
#' row; Base coordinate units are not pairwise value units.
#' @section Validation:
#' Construction checks Base identity, dimensions or endpoint IDs, finite-or-
#' missing values, directedness/symmetry, binary unweighted values, unique
#' pairs, measurement vocabulary, and provenance structure.
#'
#' @param base An `ngeo`, `ngeo_layer_view`, or `ngeo_base` object.
#' @param data A square numeric/logical matrix aligned to the ordered base, or
#'   an edge-list data frame with `from` and `to`; weighted edge lists also
#'   require `value`. Endpoints may be element positions or element IDs.
#' @param type A non-empty empirical relation type.
#' @param directed Whether pair direction is meaningful.
#' @param weighted Whether pair values carry weights rather than binary links.
#' @param measure One measurement metadata row describing pairwise values.
#' @param provenance A list recording the empirical source and processing.
#' @return An `ngeo_relation` with `base`, `data`, `type`, `directed`,
#'   `weighted`, `measure`, and `provenance` fields. `base` is a compact binding
#'   containing the implementation hash, portable signature, base type, and
#'   ordered element IDs; it is not a copy of the full spatial base.
#' @examples
#' x <- ngeo_point(cbind(x = 0:2, y = 0))
#' edges <- data.frame(from = c(1, 2), to = c(2, 3), value = c(.2, .5))
#' relation <- ngeo_relation(
#'   x, edges, type = "functional_connectivity", directed = FALSE
#' )
#' ngeo_validate_relation(relation, x)
#' @seealso [ngeo_validate_relation()], [base_hash()], [base_signature()]
#' @references neurogeo API 6.3, `inst/spec/API-6.3.md`.
#' @export
ngeo_relation <- function(base,
                          data,
                          type,
                          directed = FALSE,
                          weighted = TRUE,
                          measure = NULL,
                          provenance = list()) {
  .ngeo_assert_scalar_character(type, "type")
  normalized_type <- tolower(gsub("[^a-z0-9]+", "_", type))
  normalized_type <- gsub("^_+|_+$", "", normalized_type)
  if (grepl(
      "^(distance(s)?|adjacency|spatial_weights?)($|_)",
      normalized_type
    )) {
    .ngeo_abort(
      "Distance, adjacency, and spatial weights are analysis objects, not empirical relations.",
      "ngeo_error_relation",
      field = "type"
    )
  }
  if (!is.logical(directed) || length(directed) != 1L || is.na(directed) ||
      !is.logical(weighted) || length(weighted) != 1L || is.na(weighted)) {
    .ngeo_abort(
      "`directed` and `weighted` must each be one non-missing logical value.",
      "ngeo_error_relation"
    )
  }
  if (!is.list(provenance)) {
    .ngeo_abort(
      "`provenance` must be a list.",
      "ngeo_error_relation",
      field = "provenance"
    )
  }
  binding <- .ngeo_relation_base(base)
  result <- structure(
    list(
      base = binding,
      data = .ngeo_relation_data(data, binding, directed, weighted),
      type = type,
      directed = directed,
      weighted = weighted,
      measure = .ngeo_relation_measure(measure),
      provenance = provenance
    ),
    class = c("ngeo_relation", "list")
  )
  ngeo_validate_relation(result)
  result
}

#' Validate an empirical spatial relation
#'
#' @section When to use and when not to use:
#' Use this at package or serialization boundaries and pass `base` when exact
#' spatial alignment must be proved. Do not treat successful validation as
#' evidence for the scientific validity of the empirical relation.
#' @section Units and assumptions:
#' Validation preserves pairwise measurement units and assumes edge endpoints
#' use canonical Base element IDs. It performs no coordinate or value-unit
#' conversion.
#' @section Validation:
#' The object contract, empirical-data representation, measurement semantics,
#' and optional Base hash, portable signature, type, and ordered element IDs
#' must all validate.
#'
#' @param x An `ngeo_relation`.
#' @param base Optional spatial base whose implementation hash, portable
#'   signature, and ordered element IDs must match the relation binding.
#' @return `x`, invisibly.
#' @examples
#' x <- ngeo_point(cbind(x = 0:1, y = 0))
#' relation <- ngeo_relation(
#'   x, data.frame(from = 1, to = 2),
#'   type = "structural_connectivity", weighted = FALSE
#' )
#' ngeo_validate_relation(relation, x)
#' @seealso [ngeo_relation()], [base_hash()], [base_signature()]
#' @references neurogeo API 6.3, `inst/spec/API-6.3.md`.
#' @export
ngeo_validate_relation <- function(x, base = NULL) {
  required <- c(
    "base", "data", "type", "directed", "weighted", "measure", "provenance"
  )
  base_required <- c("hash", "signature", "type", "element_id")
  scalar_character <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  valid_flags <- is.list(x) && is.logical(x$directed) &&
    length(x$directed) == 1L && !is.na(x$directed) &&
    is.logical(x$weighted) && length(x$weighted) == 1L &&
    !is.na(x$weighted)
  if (!inherits(x, "ngeo_relation") || !is.list(x) ||
      any(!required %in% names(x)) || !is.list(x$base) ||
      any(!base_required %in% names(x$base)) ||
      !scalar_character(x$base$hash) ||
      !scalar_character(x$base$signature) ||
      !grepl("^[0-9a-f]{64}$", x$base$signature) ||
      !scalar_character(x$base$type) ||
      !is.character(x$base$element_id) || anyNA(x$base$element_id) ||
      any(!nzchar(x$base$element_id)) || anyDuplicated(x$base$element_id) ||
      !scalar_character(x$type) || !valid_flags ||
      !is.data.frame(x$measure) || nrow(x$measure) != 1L ||
      !is.list(x$provenance)) {
    .ngeo_abort("Invalid `ngeo_relation` object.", "ngeo_error_relation")
  }
  normalized_type <- tolower(gsub("[^a-z0-9]+", "_", x$type))
  normalized_type <- gsub("^_+|_+$", "", normalized_type)
  if (grepl(
      "^(distance(s)?|adjacency|spatial_weights?)($|_)",
      normalized_type
    )) {
    .ngeo_abort(
      "Distance, adjacency, and spatial weights are analysis objects, not empirical relations.",
      "ngeo_error_relation",
      field = "type"
    )
  }
  validated_data <- .ngeo_relation_data(
    x$data, x$base, x$directed, x$weighted
  )
  if (is.data.frame(x$data) && !identical(validated_data, x$data)) {
    .ngeo_abort(
      "Relation edge-list endpoints must use canonical base element IDs.",
      "ngeo_error_relation",
      field = "data"
    )
  }
  .ngeo_validate_measures(x$measure)
  if (!is.null(base)) {
    binding <- .ngeo_relation_base(base)
    if (!identical(x$base, binding)) {
      .ngeo_abort(
        "The relation does not match the supplied ordered spatial base.",
        "ngeo_error_alignment",
        field = "base"
      )
    }
  }
  invisible(x)
}

#' @export
print.ngeo_relation <- function(x, ...) {
  representation <- if (is.data.frame(x$data)) "edge list" else "matrix"
  pairs <- if (is.data.frame(x$data)) nrow(x$data) else {
    if (inherits(x$data, "Matrix")) Matrix::nnzero(x$data) else
      sum(!is.na(x$data) & x$data != 0)
  }
  cat(
    "<ngeo_relation>\n",
    "  type: ", x$type, "\n",
    "  base: ", x$base$type, " (", length(x$base$element_id), " elements)\n",
    "  data: ", representation, " (", pairs, " stored pairs)\n",
    "  directed: ", x$directed, "\n",
    "  weighted: ", x$weighted, "\n",
    sep = ""
  )
  invisible(x)
}
