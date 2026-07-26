#' Construct one delayed, strictly aligned values block
#'
#' @param reader Function `(rows, columns)` returning a matrix, or a binary
#'   file path containing column-major doubles.
#' @param dim Two positive dimensions.
#' @param map_names Optional column names.
#' @param source Auditable source description.
#'
#' @return An `ngeo_delayed_values`.
#' @export
ngeo_delayed_values <- function(reader, dim, map_names = NULL, source = NULL) {
  dim <- .ngeo_as_integer(dim, "dim")
  if (length(dim) != 2L || any(dim < 1L)) {
    .ngeo_abort("`dim` must contain two positive integers.", "ngeo_error_argument")
  }
  if (!(is.function(reader) ||
      (is.character(reader) && length(reader) == 1L && file.exists(reader)))) {
    .ngeo_abort("`reader` must be a callback or existing binary file.",
                "ngeo_error_argument")
  }
  if (!is.null(map_names) && (
      length(map_names) != dim[[2L]] || anyNA(map_names) ||
      any(!nzchar(map_names)) || anyDuplicated(map_names)
  )) {
    .ngeo_abort("`map_names` must uniquely name every column.",
                "ngeo_error_alignment")
  }
  if (is.character(reader)) {
    expected_size <- prod(as.double(dim)) * 8
    actual_size <- as.double(file.info(reader)$size)
    if (!is.finite(actual_size) || actual_size != expected_size) {
      .ngeo_abort(
        "Binary delayed values must contain exactly `prod(dim)` doubles.",
        "ngeo_error_alignment"
      )
    }
  }
  structure(
    list(
      reader = reader,
      dim = dim,
      dimnames = list(NULL, map_names),
      source = source %||% if (is.character(reader)) {
        normalizePath(reader, winslash = "/", mustWork = TRUE)
      } else {
        "callback"
      }
    ),
    class = "ngeo_delayed_values"
  )
}

#' @export
dim.ngeo_delayed_values <- function(x) x$dim

#' @export
dimnames.ngeo_delayed_values <- function(x) x$dimnames

#' @export
`dimnames<-.ngeo_delayed_values` <- function(x, value) {
  x$dimnames <- value
  x
}

.ngeo_delayed_index <- function(index, n, names = NULL) {
  if (missing(index)) return(seq_len(n))
  if (is.character(index)) {
    matched <- match(index, names)
    if (anyNA(matched)) {
      .ngeo_abort("Delayed value names do not align.", "ngeo_error_alignment")
    }
    return(matched)
  }
  seq_len(n)[index]
}

.ngeo_read_delayed_binary <- function(path, dim, rows, columns) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  value <- matrix(NA_real_, nrow = length(rows), ncol = length(columns))
  row_count <- as.double(dim[[1L]])
  for (column_position in seq_along(columns)) {
    column <- as.double(columns[[column_position]])
    for (row_position in seq_along(rows)) {
      row <- as.double(rows[[row_position]])
      offset <- ((column - 1) * row_count + row - 1) * 8
      seek(connection, where = offset, origin = "start")
      current <- readBin(connection, "double", n = 1L, size = 8L)
      if (length(current) != 1L) {
        .ngeo_abort(
          "Binary delayed values ended before the requested cell.",
          "ngeo_error_io"
        )
      }
      value[row_position, column_position] <- current
    }
  }
  value
}

#' @export
`[.ngeo_delayed_values` <- function(x, i, j, ..., drop = TRUE) {
  rows <- if (missing(i)) seq_len(x$dim[[1L]]) else
    .ngeo_delayed_index(i, x$dim[[1L]], x$dimnames[[1L]])
  columns <- if (missing(j)) seq_len(x$dim[[2L]]) else
    .ngeo_delayed_index(j, x$dim[[2L]], x$dimnames[[2L]])
  value <- if (is.function(x$reader)) {
    x$reader(rows, columns)
  } else {
    .ngeo_read_delayed_binary(x$reader, x$dim, rows, columns)
  }
  value <- as.matrix(value)
  if (!identical(dim(value), c(length(rows), length(columns)))) {
    .ngeo_abort("Delayed reader returned a misaligned block.",
                "ngeo_error_alignment")
  }
  dimnames(value) <- list(
    x$dimnames[[1L]][rows],
    x$dimnames[[2L]][columns]
  )
  if (drop) drop(value) else value
}

#' @export
as.matrix.ngeo_delayed_values <- function(x, ...) {
  x[, , drop = FALSE]
}

#' Iterate over aligned value chunks
#'
#' @param x An `ngeo` dataset or `ngeo_delayed_values`.
#' @param chunk_size Positive row count.
#' @param maps Optional map selection.
#' @param FUN Optional function called with `(block, rows)`. When omitted an
#'   iterator closure is returned.
#'
#' @return An iterator or list of callback results.
#' @export
ngeo_value_chunks <- function(x, chunk_size = 65536L, maps = NULL, FUN = NULL) {
  values <- if (inherits(x, "ngeo")) x$values else x
  if (is.null(values)) {
    .ngeo_abort("No values are loaded.", "ngeo_error_values")
  }
  chunk_size <- .ngeo_as_integer(chunk_size, "chunk_size")
  if (length(chunk_size) != 1L || chunk_size < 1L) {
    .ngeo_abort("`chunk_size` must be positive.", "ngeo_error_argument")
  }
  columns <- if (is.null(maps)) seq_len(ncol(values)) else if (
    inherits(x, "ngeo")
  ) .ngeo_map_selection(x, maps) else
    .ngeo_delayed_index(maps, ncol(values), colnames(values))
  position <- 1L
  iterator <- function() {
    if (position > nrow(values)) return(NULL)
    rows <- seq.int(position, min(nrow(values), position + chunk_size - 1L))
    position <<- max(rows) + 1L
    list(rows = rows, values = values[rows, columns, drop = FALSE])
  }
  if (is.null(FUN)) return(iterator)
  output <- list()
  repeat {
    current <- iterator()
    if (is.null(current)) break
    output[[length(output) + 1L]] <- FUN(current$values, current$rows)
  }
  output
}
