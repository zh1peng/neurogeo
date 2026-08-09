.ngeo_label_values <- function(x, labels) {
  if (!is.character(labels) || length(labels) != 1L) {
    return(list(values = labels, table = NULL, name = "partition"))
  }
  if (labels %in% names(x$base$labels)) {
    label <- x$base$labels[[labels]]
    values <- label$values %||% NULL
    if (is.null(values) && !is.null(label$layer_id)) {
      layer_index <- match(label$layer_id, x$layers$layer_id)
      values <- x$values[, layer_index]
    }
    return(list(
      values = values,
      table = label$table %||% NULL,
      name = labels
    ))
  }
  if (labels %in% x$layers$name || labels %in% x$layers$layer_id) {
    layer_index <- .ngeo_layer_selection(x, labels)
    return(list(
      values = x$values[, layer_index],
      table = NULL,
      name = x$layers$name[[layer_index]]
    ))
  }
  .ngeo_abort(
    sprintf("Unknown label or map `%s`.", labels),
    "ngeo_error_labels"
  )
}

.ngeo_region_names <- function(region_ids, table) {
  names <- rep.int(NA_character_, length(region_ids))
  if (is.null(table) || !is.data.frame(table)) {
    return(names)
  }
  candidates <- list(
    c("Key", "Label"),
    c("Key", "label"),
    c("Index", "label"),
    c("code", "struct_name"),
    c("struct_index", "struct_name")
  )
  for (candidate in candidates) {
    if (all(candidate %in% names(table))) {
      names <- as.character(table[[candidate[[2L]]]][
        match(region_ids, as.character(table[[candidate[[1L]]]]))
      ])
      break
    }
  }
  names
}

#' Construct a crisp partition
#'
#' @param x Base `ngeo` dataset.
#' @param labels Label vector, label-table name, or categorical map name.
#' @param background Explicit background values to exclude. `NULL` excludes
#'   nothing.
#' @param unlabeled_policy Policy for missing labels.
#'
#' @return An `ngeo_partition` object.
#' @examples
#' surface <- ngeo_surface(
#'   matrix(
#'     c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
#'     ncol = 3, byrow = TRUE
#'   ),
#'   matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE),
#'   values = cbind(signal = c(1, 2, 4, 3)),
#'   measures = ngeo_measure(support_behavior = "intensive")
#' )
#' partition <- ngeo_partition(surface, c("A", "A", "B", "B"))
#' ngeo_boundary(surface, partition)
#' ngeo_region_adjacency(surface, partition)
#' ngeo_aggregate(surface, partition)
#' @template stable-neuroimaging-method
#' @export
ngeo_partition <- function(x,
                           labels,
                           background = NULL,
                           unlabeled_policy = c("exclude", "error")) {
  ngeo_validate(x, "basic")
  unlabeled_policy <- match.arg(unlabeled_policy)
  label <- .ngeo_label_values(x, labels)
  membership <- label$values
  if (is.factor(membership)) {
    membership <- as.character(membership)
  }
  if (!is.atomic(membership) ||
      length(membership) != nrow(x$base$elements)) {
    .ngeo_abort(
      "`labels` must provide one crisp membership value per base element.",
      "ngeo_error_alignment"
    )
  }
  if (!is.null(background)) {
    membership[membership %in% background] <- NA
  }
  if (identical(unlabeled_policy, "error") && anyNA(membership)) {
    .ngeo_abort(
      "Partition contains unlabeled/background elements.",
      "ngeo_error_partition"
    )
  }

  membership <- as.character(membership)
  region_ids <- unique(membership[!is.na(membership)])
  if (!length(region_ids)) {
    .ngeo_abort(
      "Partition has no non-background parcellation.",
      "ngeo_error_partition"
    )
  }
  region_names <- .ngeo_region_names(region_ids, label$table)
  parcellation <- data.frame(
    region_id = region_ids,
    name = region_names,
    stringsAsFactors = FALSE
  )

  partition <- base::structure(
    list(
      membership = membership,
      source_base_hash = base_hash(x),
      parcellation = parcellation,
      background = background,
      unlabeled_policy = unlabeled_policy,
      overlap_policy = "disallow",
      source = label$name,
      history = list(
        operations = list(.ngeo_operation(
          "ngeo_partition",
          list(
            source = label$name,
            background = background,
            unlabeled_policy = unlabeled_policy
          )
        ))
      )
    ),
    class = "ngeo_partition"
  )
  .ngeo_validate_partition(partition, x)
  partition
}

.ngeo_validate_partition <- function(partition, x = NULL) {
  if (!inherits(partition, "ngeo_partition") ||
      !is.atomic(partition$membership) ||
      !is.data.frame(partition$parcellation) ||
      !"region_id" %in% names(partition$parcellation) ||
      anyDuplicated(partition$parcellation$region_id)) {
    .ngeo_abort("Invalid `ngeo_partition` object.", "ngeo_error_partition")
  }
  membership_ids <- unique(partition$membership[!is.na(partition$membership)])
  if (any(!membership_ids %in% partition$parcellation$region_id)) {
    .ngeo_abort(
      "Partition membership references an unknown region.",
      "ngeo_error_partition"
    )
  }
  if (!is.null(x)) {
    if (!identical(partition$source_base_hash, base_hash(x))) {
      .ngeo_abort(
        "Partition source base hash does not match the dataset.",
        "ngeo_error_base_mismatch"
      )
    }
    if (length(partition$membership) != nrow(x$base$elements)) {
      .ngeo_abort(
        "Partition membership is not aligned with the dataset.",
        "ngeo_error_alignment"
      )
    }
  }
  invisible(TRUE)
}

#' @export
print.ngeo_partition <- function(x, ...) {
  cat(
    "<ngeo_partition>\n",
    "  parcellation: ", nrow(x$parcellation), "\n",
    "  elements: ", length(x$membership), "\n",
    "  excluded: ", sum(is.na(x$membership)), "\n",
    sep = ""
  )
  invisible(x)
}

#' Derive region adjacency from a base topology
#'
#' @param x Base `ngeo` object.
#' @param partition Matching `ngeo_partition`.
#' @param weight Binary relation or base-edge count.
#' @param connectivity Voxel connectivity.
#'
#' @return A sparse region-by-region matrix.
#' @export
ngeo_region_adjacency <- function(x,
                                  partition,
                                  weight = c("binary", "edge_count"),
                                  connectivity = 6L) {
  .ngeo_validate_partition(partition, x)
  weight <- match.arg(weight)
  adjacency <- ngeo_adjacency(x, connectivity = connectivity)
  entries <- Matrix::summary(adjacency)
  entries <- entries[entries$i < entries$j, , drop = FALSE]
  membership <- partition$membership
  left <- membership[entries$i]
  right <- membership[entries$j]
  keep <- !is.na(left) & !is.na(right) & left != right
  left <- left[keep]
  right <- right[keep]

  n_region <- nrow(partition$parcellation)
  if (!length(left)) {
    return(.ngeo_sparse_edges(n_region, NULL))
  }
  region_index <- stats::setNames(
    seq_len(n_region),
    partition$parcellation$region_id
  )
  pairs <- cbind(
    as.integer(region_index[left]),
    as.integer(region_index[right])
  )
  pairs <- t(apply(pairs, 1L, sort))
  key <- paste(pairs[, 1L], pairs[, 2L], sep = ":")
  counts <- table(key)
  unique_pairs <- do.call(
    rbind,
    strsplit(names(counts), ":", fixed = TRUE)
  )
  storage.mode(unique_pairs) <- "integer"
  values <- if (identical(weight, "binary")) {
    rep.int(1, nrow(unique_pairs))
  } else {
    as.numeric(counts)
  }
  .ngeo_sparse_edges(n_region, unique_pairs, values)
}

#' Return base edges crossing partition boundaries
#'
#' @param x Base `ngeo` object.
#' @param partition Matching partition.
#' @param connectivity Voxel connectivity.
#'
#' @return A data frame of base element and region IDs.
#' @export
ngeo_boundary <- function(x, partition, connectivity = 6L) {
  .ngeo_validate_partition(partition, x)
  adjacency <- ngeo_adjacency(x, connectivity = connectivity)
  entries <- Matrix::summary(adjacency)
  entries <- entries[entries$i < entries$j, , drop = FALSE]
  left_region <- partition$membership[entries$i]
  right_region <- partition$membership[entries$j]
  keep <- !is.na(left_region) & !is.na(right_region) &
    left_region != right_region
  data.frame(
    from = x$base$elements$element_id[entries$i[keep]],
    to = x$base$elements$element_id[entries$j[keep]],
    from_region = left_region[keep],
    to_region = right_region[keep],
    stringsAsFactors = FALSE
  )
}

.ngeo_region_support <- function(support, membership, parcellation) {
  vapply(parcellation, function(region) {
    current <- support[membership == region]
    if (!length(current) || all(is.na(current))) {
      NA_real_
    } else {
      sum(current, na.rm = TRUE)
    }
  }, numeric(1))
}

.ngeo_region_centroid <- function(x, membership, parcellation, support) {
  coordinates <- tryCatch(
    .ngeo_element_coordinates(x),
    error = function(...) NULL
  )
  if (is.null(coordinates) || anyNA(coordinates)) {
    return(NULL)
  }
  result <- matrix(NA_real_, nrow = length(parcellation), ncol = ncol(coordinates))
  for (i in seq_along(parcellation)) {
    index <- which(membership == parcellation[[i]])
    spatial_weights <- support[index]
    if (anyNA(spatial_weights) || sum(spatial_weights) <= 0) {
      spatial_weights <- rep.int(1, length(index))
    }
    result[i, ] <- vapply(
      seq_len(ncol(coordinates)),
      function(column) stats::weighted.mean(
        coordinates[index, column],
        spatial_weights
      ),
      numeric(1)
    )
  }
  result
}

#' Aggregate values from a spatial base to a parcellation
#'
#' @param x Base `ngeo` dataset.
#' @param partition Matching crisp partition.
#' @param layers Optional layer selection.
#' @param connectivity Voxel connectivity for region adjacency.
#' @param allocation,unmapped,unknown,budget Passed to [aggregate_to()].
#'
#' @return An `ngeo_parcellation` dataset.
#' @templateVar example_call ngeo_aggregate(source_data, partition)
#' @template stable-neuroimaging-method
#' @export
ngeo_aggregate <- function(x,
                           partition,
                           layers = NULL,
                           connectivity = 6L,
                           allocation = c("error", "normalize"),
                           unmapped = c("error", "drop"),
                           unknown = c("error", "intensive", "extensive"),
                           budget = ngeo_resource_budget()) {
  .ngeo_validate_partition(partition, x)
  membership <- partition$membership
  parcellation <- partition$parcellation$region_id
  support <- ngeo_support_size(x)
  region_support <- .ngeo_region_support(support, membership, parcellation)
  centroid <- .ngeo_region_centroid(
    x,
    membership,
    parcellation,
    support
  )
  adjacency <- if (isTRUE(ngeo_capabilities(x)[["adjacency"]])) {
    ngeo_region_adjacency(
      x,
      partition,
      connectivity = connectivity
    )
  } else {
    NULL
  }
  target <- ngeo_parcellation(
    partition$parcellation,
    membership = membership,
    source_base = x,
    centroid = centroid,
    support_size = region_support,
    adjacency = adjacency,
    coordinate_space = x$base$coordinate_space
  )
  support_map <- ngeo_support_map_from_partition(x, partition, target)
  result <- aggregate_to(
    x,
    target,
    support_map,
    layers = layers,
    allocation = allocation,
    unmapped = unmapped,
    unknown = unknown,
    budget = budget
  )
  result$history$partition <- partition$history
  result$history$operations <- c(
    result$history$operations,
    list(.ngeo_operation(
      "ngeo_aggregate",
      list(
        partition_source = partition$source,
        excluded_elements = sum(is.na(membership)),
        layers = result$layers$layer_id,
        aggregation_rules = stats::setNames(
          result$measures$support_behavior[
            match(result$layers$measure_id, result$measures$measure_id)
          ],
          result$layers$layer_id
        ),
        missing_policy = stats::setNames(
          rep.int("error", nrow(result$layers)),
          result$layers$layer_id
        ),
        output_support_size = region_support
      )
    ))
  )
  ngeo_validate(result, "strict")
  result
}
