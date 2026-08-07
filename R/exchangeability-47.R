.ngeo_exchange_units <- function(unit_id) {
  if (!is.character(unit_id) || length(unit_id) < 2L || anyNA(unit_id) ||
      any(!nzchar(unit_id)) || anyDuplicated(unit_id)) {
    .ngeo_abort(
      "`unit_id` must contain at least two unique non-empty identifiers.",
      "ngeo_error_independent_unit"
    )
  }
  unit_id
}

.ngeo_exchange_blocks <- function(blocks, unit_id, required = FALSE) {
  if (is.null(blocks)) {
    if (required) {
      .ngeo_abort("Within-block exchangeability requires `blocks`.",
                  "ngeo_error_exchangeability")
    }
    return(NULL)
  }
  if (length(blocks) != length(unit_id) || anyNA(blocks)) {
    .ngeo_abort("`blocks` must align exactly with `unit_id`.",
                "ngeo_error_alignment")
  }
  blocks <- as.character(blocks)
  if (any(!nzchar(blocks))) {
    .ngeo_abort("Exchangeability blocks may not be empty.",
                "ngeo_error_exchangeability")
  }
  stats::setNames(blocks, unit_id)
}

.ngeo_all_permutations <- function(values) {
  if (length(values) == 1L) return(matrix(values, nrow = 1L))
  do.call(rbind, lapply(seq_along(values), function(i) {
    cbind(values[[i]], .ngeo_all_permutations(values[-i]))
  }))
}

.ngeo_exact_permutations <- function(n, blocks = NULL) {
  identity <- seq_len(n)
  if (is.null(blocks)) {
    result <- .ngeo_all_permutations(identity)
  } else {
    result <- matrix(identity, nrow = 1L)
    groups <- split(identity, blocks)
    for (group in groups) {
      permutations <- .ngeo_all_permutations(group)
      expanded <- vector("list", nrow(result) * nrow(permutations))
      position <- 0L
      for (row in seq_len(nrow(result))) {
        for (candidate in seq_len(nrow(permutations))) {
          position <- position + 1L
          current <- result[row, ]
          current[group] <- permutations[candidate, ]
          expanded[[position]] <- current
        }
      }
      result <- do.call(rbind, expanded)
    }
  }
  result[!apply(result, 1L, identical, y = identity), , drop = FALSE]
}

.ngeo_exact_signs <- function(n) {
  result <- as.matrix(expand.grid(rep(list(c(-1L, 1L)), n)))
  storage.mode(result) <- "integer"
  result[!apply(result, 1L, function(row) all(row == 1L)), , drop = FALSE]
}

.ngeo_random_schedule <- function(n, target, transformation, blocks, seed) {
  keys <- new.env(hash = TRUE, parent = emptyenv())
  identity <- seq_len(n)
  groups <- if (is.null(blocks)) NULL else split(identity, blocks)
  draw <- switch(
    transformation,
    permutation = function() {
      if (is.null(groups)) return(sample.int(n))
      current <- identity
      for (group in groups) current[group] <- sample(group)
      current
    },
    sign_flip = function() sample(c(-1L, 1L), n, replace = TRUE)
  )
  .ngeo_with_seed(seed, function() {
    result <- matrix(NA_integer_, target, n)
    accepted <- 0L
    attempts <- 0L
    maximum_attempts <- max(1000L, 200L * target)
    while (accepted < target && attempts < maximum_attempts) {
      attempts <- attempts + 1L
      candidate <- draw()
      is_identity <- if (identical(transformation, "permutation")) {
        identical(candidate, identity)
      } else {
        all(candidate == 1L)
      }
      key <- paste(candidate, collapse = ",")
      if (!is_identity && !exists(key, envir = keys, inherits = FALSE)) {
        accepted <- accepted + 1L
        result[accepted, ] <- candidate
        assign(key, TRUE, envir = keys)
      }
    }
    if (accepted < target) {
      .ngeo_abort(
        "Could not generate the requested number of unique transformations.",
        "ngeo_error_exchangeability"
      )
    }
    result
  })
}

.ngeo_validate_user_schedule <- function(schedule, unit_id, blocks) {
  if (!is.matrix(schedule) || !is.numeric(schedule) ||
      nrow(schedule) < 1L || ncol(schedule) != length(unit_id) ||
      anyNA(schedule) || any(!is.finite(schedule)) ||
      any(schedule != floor(schedule))) {
    .ngeo_abort("A user schedule must be one finite integer matrix.",
                "ngeo_error_exchangeability")
  }
  if (is.null(colnames(schedule)) ||
      !identical(colnames(schedule), unit_id)) {
    .ngeo_abort("User schedule columns must exactly equal `unit_id`.",
                "ngeo_error_alignment")
  }
  storage.mode(schedule) <- "integer"
  sign_flip <- all(schedule %in% c(-1L, 1L))
  transformation <- if (sign_flip) "sign_flip" else "permutation"
  identity <- if (sign_flip) rep.int(1L, length(unit_id)) else
    seq_along(unit_id)
  if (any(apply(schedule, 1L, function(row) {
    identical(unname(row), identity)
  }))) {
    .ngeo_abort(
      "The observed identity transformation must not appear in `schedule`.",
      "ngeo_error_exchangeability"
    )
  }
  keys <- apply(schedule, 1L, paste, collapse = ",")
  if (anyDuplicated(keys)) {
    .ngeo_abort("A user schedule may not contain duplicate transformations.",
                "ngeo_error_exchangeability")
  }
  if (!sign_flip) {
    valid <- apply(schedule, 1L, function(row) {
      identical(unname(sort(row)), seq_along(unit_id))
    })
    if (any(!valid)) {
      .ngeo_abort("Every permutation row must contain each unit index once.",
                  "ngeo_error_exchangeability")
    }
    if (!is.null(blocks)) {
      respects <- apply(schedule, 1L, function(row) {
        identical(unname(blocks[row]), unname(blocks))
      })
      if (any(!respects)) {
        .ngeo_abort("A user permutation crosses declared blocks.",
                    "ngeo_error_exchangeability")
      }
    }
  }
  list(schedule = schedule, transformation = transformation)
}

#' Declare subject-level exchangeability transformations
#'
#' Transformations are stored in rows and independent unit in columns. The
#' observed identity transformation is kept outside the schedule.
#'
#' @param unit_id Ordered unique independent-unit identifiers.
#' @param scheme Free, within-block, sign-flip, or user schedule.
#' @param blocks Optional unit-aligned exchangeability blocks.
#' @param schedule User-supplied integer transformation matrix.
#' @param permutations Requested Monte Carlo transformations.
#' @param seed Optional reproducible seed.
#' @param budget Hard execution resource limits.
#'
#' @return An `ngeo_exchangeability` object.
#' @export
ngeo_exchangeability <- function(
    unit_id,
    scheme = c("free", "within_block", "sign_flip", "user"),
    blocks = NULL,
    schedule = NULL,
    permutations = 4999L,
    seed = NULL,
    budget = ngeo_resource_budget()) {
  unit_id <- .ngeo_exchange_units(unit_id)
  scheme <- match.arg(scheme)
  seed <- .ngeo_seed(seed)
  blocks <- .ngeo_exchange_blocks(
    blocks, unit_id, required = identical(scheme, "within_block")
  )
  if (identical(scheme, "within_block") &&
      length(unique(blocks)) < 2L) {
    .ngeo_abort("Within-block exchangeability requires at least two blocks.",
                "ngeo_error_exchangeability")
  }

  if (identical(scheme, "user")) {
    validated <- .ngeo_validate_user_schedule(schedule, unit_id, blocks)
    normalized <- validated$schedule
    transformation <- validated$transformation
    status <- "user"
    unique_transformations <- nrow(normalized)
  } else {
    if (!is.null(schedule)) {
      .ngeo_abort("`schedule` is only valid for `scheme = \"user\"`.",
                  "ngeo_error_argument")
    }
    permutations <- .ngeo_permutations(permutations)
    if (permutations < 1L) {
      .ngeo_abort("At least one transformation is required.",
                  "ngeo_error_exchangeability")
    }
    n <- length(unit_id)
    transformation <- if (identical(scheme, "sign_flip"))
      "sign_flip" else "permutation"
    log_total <- if (identical(scheme, "free")) {
      lgamma(n + 1)
    } else if (identical(scheme, "within_block")) {
      sum(lgamma(as.numeric(table(blocks)) + 1))
    } else {
      n * log(2)
    }
    total_with_identity <- if (log_total > log(.Machine$double.xmax)) Inf else
      round(exp(log_total))
    unique_transformations <- total_with_identity - 1
    if (unique_transformations < 1) {
      .ngeo_abort("The declared scheme has no non-identity transformation.",
                  "ngeo_error_exchangeability")
    }
    target <- as.integer(min(permutations, unique_transformations))
    .ngeo_budget_assert(
      budget, "materialized_elements", as.double(target) * n
    )
    .ngeo_budget_assert(budget, "memory_bytes", as.double(target) * n * 4)
    exact_limit <- getOption("neurogeo.max_exact_transformations", 100000L)
    exact <- is.finite(unique_transformations) &&
      permutations >= unique_transformations &&
      unique_transformations <= exact_limit
    normalized <- if (exact && identical(transformation, "sign_flip")) {
      .ngeo_exact_signs(n)
    } else if (exact) {
      .ngeo_exact_permutations(
        n, if (identical(scheme, "within_block")) blocks else NULL
      )
    } else {
      .ngeo_random_schedule(
        n, target, transformation,
        if (identical(scheme, "within_block")) blocks else NULL,
        seed
      )
    }
    status <- if (exact) "exact" else "monte_carlo"
  }
  colnames(normalized) <- unit_id
  .ngeo_budget_assert(
    budget, "materialized_elements", as.double(length(normalized))
  )
  .ngeo_budget_assert(
    budget, "memory_bytes", as.double(length(normalized)) * 4
  )
  identity <- list(
    unit_id = unit_id, scheme = scheme, transformation = transformation,
    blocks = blocks, schedule = normalized
  )
  result <- list(
    unit_id = unit_id,
    schedule = normalized,
    scheme = scheme,
    transformation = transformation,
    blocks = blocks,
    permutations = nrow(normalized),
    unique_transformations = unique_transformations,
    status = status,
    seed = seed,
    schedule_hash = .ngeo_layer_digest(identity),
    identity_included = FALSE,
    diagnostics = list(
      duplicate_transformations = FALSE,
      row_order_fixed = TRUE,
      complex_exchangeability_tree = FALSE
    )
  )
  class(result) <- "ngeo_exchangeability"
  result
}

#' @export
print.ngeo_exchangeability <- function(x, ...) {
  cat(
    "<ngeo_exchangeability>\n",
    "  scheme: ", x$scheme, "\n",
    "  transformation: ", x$transformation, "\n",
    "  unit: ", length(x$unit_id), "\n",
    "  transformations: ", nrow(x$schedule), " (", x$status, ")\n",
    "  schedule hash: ", x$schedule_hash, "\n",
    sep = ""
  )
  invisible(x)
}
