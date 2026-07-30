.ngeo_label_values <- function(x, labels) {
  if (!is.character(labels) || length(labels) != 1L) {
    return(list(values = labels, table = NULL, name = "partition"))
  }
  if (labels %in% names(x$labels)) {
    label <- x$labels[[labels]]
    values <- label$values %||% NULL
    if (is.null(values) && !is.null(label$map_id)) {
      map_index <- match(label$map_id, x$maps$map_id)
      values <- x$values[, map_index]
    }
    return(list(
      values = values,
      table = label$table %||% NULL,
      name = labels
    ))
  }
  if (labels %in% x$maps$name || labels %in% x$maps$map_id) {
    map_index <- .ngeo_map_selection(x, labels)
    return(list(
      values = x$values[, map_index],
      table = NULL,
      name = x$maps$name[[map_index]]
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
#'   measures = ngeo_measure(spatial_semantics = "intensive")
#' )
#' partition <- ngeo_partition(surface, c("A", "A", "B", "B"))
#' ngeo_boundary(surface, partition)
#' ngeo_region_adjacency(surface, partition)
#' ngeo_aggregate(surface, partition)
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
      length(membership) != nrow(x$domain$elements)) {
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
      "Partition has no non-background regions.",
      "ngeo_error_partition"
    )
  }
  region_names <- .ngeo_region_names(region_ids, label$table)
  regions <- data.frame(
    region_id = region_ids,
    name = region_names,
    stringsAsFactors = FALSE
  )

  partition <- base::structure(
    list(
      membership = membership,
      base_domain_hash = ngeo_domain_hash(x),
      regions = regions,
      background = background,
      unlabeled_policy = unlabeled_policy,
      overlap_policy = "disallow",
      source = label$name,
      provenance = list(
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
      !is.data.frame(partition$regions) ||
      !"region_id" %in% names(partition$regions) ||
      anyDuplicated(partition$regions$region_id)) {
    .ngeo_abort("Invalid `ngeo_partition` object.", "ngeo_error_partition")
  }
  membership_ids <- unique(partition$membership[!is.na(partition$membership)])
  if (any(!membership_ids %in% partition$regions$region_id)) {
    .ngeo_abort(
      "Partition membership references an unknown region.",
      "ngeo_error_partition"
    )
  }
  if (!is.null(x)) {
    if (!identical(partition$base_domain_hash, ngeo_domain_hash(x))) {
      .ngeo_abort(
        "Partition base-domain hash does not match the dataset.",
        "ngeo_error_domain_mismatch"
      )
    }
    if (length(partition$membership) != nrow(x$domain$elements)) {
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
    "  regions: ", nrow(x$regions), "\n",
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

  n_region <- nrow(partition$regions)
  if (!length(left)) {
    return(.ngeo_sparse_edges(n_region, NULL))
  }
  region_index <- stats::setNames(
    seq_len(n_region),
    partition$regions$region_id
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
    from = x$domain$elements$element_id[entries$i[keep]],
    to = x$domain$elements$element_id[entries$j[keep]],
    from_region = left_region[keep],
    to_region = right_region[keep],
    stringsAsFactors = FALSE
  )
}

.ngeo_mode <- function(values, tie) {
  values <- values[!is.na(values)]
  if (!length(values)) {
    return(NA)
  }
  counts <- table(values)
  winners <- names(counts)[counts == max(counts)]
  if (length(winners) > 1L && identical(tie, "error")) {
    .ngeo_abort(
      "Categorical aggregation produced a tie.",
      "ngeo_error_aggregation_tie"
    )
  }
  winner <- winners[[1L]]
  if (is.numeric(values) || is.integer(values)) {
    as.numeric(winner)
  } else if (is.logical(values)) {
    identical(winner, "TRUE")
  } else {
    winner
  }
}

.ngeo_aggregate_map <- function(values,
                                membership,
                                regions,
                                support,
                                measure,
                                fun,
                                na.rm,
                                tie) {
  result <- vector("list", length(regions))
  semantics <- measure$spatial_semantics[[1L]]
  missing_policy <- measure$missing_policy[[1L]]

  for (i in seq_along(regions)) {
    index <- which(membership == regions[[i]])
    current <- values[index]
    current_support <- support[index]
    if (!isTRUE(na.rm) || identical(missing_policy, "preserve")) {
      if (anyNA(current)) {
        result[[i]] <- NA
        next
      }
    } else {
      keep <- !is.na(current)
      current <- current[keep]
      current_support <- current_support[keep]
    }
    if (!length(current)) {
      result[[i]] <- NA
    } else if (!is.null(fun)) {
      result[[i]] <- fun(current)
    } else if (semantics %in% c("extensive", "count")) {
      result[[i]] <- sum(current)
    } else if (identical(semantics, "intensive")) {
      if (anyNA(current_support) || any(current_support < 0) ||
          sum(current_support) <= 0) {
        .ngeo_abort(
          "Intensive aggregation requires positive support sizes.",
          "ngeo_error_support"
        )
      }
      result[[i]] <- stats::weighted.mean(current, current_support)
    } else if (identical(semantics, "categorical")) {
      result[[i]] <- .ngeo_mode(current, tie)
    } else {
      .ngeo_abort(
        "Unknown measurement semantics require an explicit `fun=`.",
        "ngeo_error_measure_unknown"
      )
    }
  }
  unlist(result, recursive = FALSE, use.names = FALSE)
}

.ngeo_region_support <- function(support, membership, regions) {
  vapply(regions, function(region) {
    current <- support[membership == region]
    if (!length(current) || all(is.na(current))) {
      NA_real_
    } else {
      sum(current, na.rm = TRUE)
    }
  }, numeric(1))
}

.ngeo_region_centroid <- function(x, membership, regions, support) {
  coordinates <- tryCatch(
    .ngeo_element_coordinates(x),
    error = function(...) NULL
  )
  if (is.null(coordinates) || anyNA(coordinates)) {
    return(NULL)
  }
  result <- matrix(NA_real_, nrow = length(regions), ncol = ncol(coordinates))
  for (i in seq_along(regions)) {
    index <- which(membership == regions[[i]])
    weights <- support[index]
    if (anyNA(weights) || sum(weights) <= 0) {
      weights <- rep.int(1, length(index))
    }
    result[i, ] <- vapply(
      seq_len(ncol(coordinates)),
      function(column) stats::weighted.mean(
        coordinates[index, column],
        weights
      ),
      numeric(1)
    )
  }
  result
}

#' Aggregate values from a base domain to regions
#'
#' @param x Base `ngeo` dataset.
#' @param partition Matching crisp partition.
#' @param maps Optional map selection.
#' @param fun Optional explicit aggregation function.
#' @param na.rm Whether missing values may be excluded when policy allows.
#' @param tie Categorical tie policy.
#' @param connectivity Voxel connectivity for region adjacency.
#'
#' @return An `ngeo_regions` dataset.
#' @export
ngeo_aggregate <- function(x,
                           partition,
                           maps = NULL,
                           fun = NULL,
                           na.rm = TRUE,
                           tie = c("first", "error"),
                           connectivity = 6L) {
  .ngeo_validate_partition(partition, x)
  tie <- match.arg(tie)
  if (!is.null(fun) && !is.function(fun)) {
    .ngeo_abort("`fun` must be a function.", "ngeo_error_argument")
  }
  if (is.null(x$values)) {
    .ngeo_abort(
      "Cannot aggregate a dataset with unloaded values.",
      "ngeo_error_values"
    )
  }

  map_index <- .ngeo_map_selection(x, maps)
  membership <- partition$membership
  regions <- partition$regions$region_id
  support <- ngeo_support_size(x)
  values <- vector("list", length(map_index))
  for (i in seq_along(map_index)) {
    map <- map_index[[i]]
    values[[i]] <- .ngeo_aggregate_map(
      x$values[, map],
      membership,
      regions,
      support,
      x$measures[map, , drop = FALSE],
      fun,
      na.rm,
      tie
    )
  }
  values <- do.call(cbind, values)
  maps_out <- x$maps[map_index, , drop = FALSE]
  measures_out <- x$measures[map_index, , drop = FALSE]
  rownames(maps_out) <- NULL
  rownames(measures_out) <- NULL
  if (!is.null(fun)) {
    measures_out$default_aggregation <- "custom"
  }
  colnames(values) <- maps_out$name

  region_support <- .ngeo_region_support(support, membership, regions)
  centroid <- .ngeo_region_centroid(
    x,
    membership,
    regions,
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
  result <- ngeo_regions(
    partition$regions,
    values = values,
    membership = membership,
    base_domain = x,
    centroid = centroid,
    support_size = region_support,
    adjacency = adjacency,
    maps = maps_out,
    measures = measures_out,
    space = x$domain$space
  )
  result$provenance$source_dataset <- list(
    domain_hash = ngeo_domain_hash(x),
    provenance = x$provenance
  )
  result$provenance$partition <- partition$provenance
  result$provenance$operations <- c(
    result$provenance$operations,
    list(.ngeo_operation(
      "ngeo_aggregate",
      list(
        partition_source = partition$source,
        excluded_elements = sum(is.na(membership)),
        maps = maps_out$map_id,
        aggregation_rules = stats::setNames(
          if (is.null(fun)) {
            measures_out$default_aggregation
          } else {
            rep.int("custom", nrow(maps_out))
          },
          maps_out$map_id
        ),
        custom_function = !is.null(fun),
        na_rm = na.rm,
        missing_policy = stats::setNames(
          measures_out$missing_policy,
          maps_out$map_id
        ),
        output_support_size = region_support
      )
    ))
  )
  ngeo_validate(result, "strict")
  result
}
