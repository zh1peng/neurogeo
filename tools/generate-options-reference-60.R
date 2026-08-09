args <- commandArgs(trailingOnly = TRUE)
csv_output <- if (length(args)) args[[1L]] else
  file.path("inst", "spec", "options-6.0.csv")
markdown_output <- if (length(args) >= 2L) args[[2L]] else
  file.path("website", "concepts", "options.md")

calls <- list()
walk <- function(value, source) {
  if (is.call(value) && identical(value[[1L]], as.name("getOption")) &&
      length(value) >= 2L && is.character(value[[2L]]) &&
      grepl("^neurogeo[.]", value[[2L]])) {
    default <- if (length(value) >= 3L) {
      paste(deparse(value[[3L]]), collapse = " ")
    } else {
      "NULL"
    }
    calls[[length(calls) + 1L]] <<- data.frame(
      option = value[[2L]],
      default = default,
      consumer = source,
      stringsAsFactors = FALSE
    )
  }
  if (is.recursive(value)) lapply(as.list(value), walk, source = source)
  invisible(NULL)
}
for (path in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) {
  expressions <- parse(path, keep.source = FALSE)
  lapply(as.list(expressions), walk, source = gsub("\\\\", "/", path))
}
raw <- do.call(rbind, calls)
if (is.null(raw) || !nrow(raw)) stop("No neurogeo options were found.")
groups <- split(seq_len(nrow(raw)), raw$option)
registry <- do.call(rbind, lapply(names(groups), function(option) {
  rows <- raw[groups[[option]], , drop = FALSE]
  defaults <- unique(rows$default)
  if (length(defaults) != 1L) {
    stop("Option has conflicting defaults: ", option)
  }
  default <- defaults[[1L]]
  type <- if (grepl("L$", default)) {
    "positive integer"
  } else if (grepl("^(TRUE|FALSE)$", default)) {
    "logical"
  } else if (grepl("^[0-9.eE+-]+$", default)) {
    "positive number"
  } else {
    "documented scalar"
  }
  data.frame(
    option = option,
    default = default,
    accepted = if (identical(type, "logical")) "TRUE or FALSE" else type,
    lifecycle = "stable",
    owner = "runtime-maintainer",
    consumers = paste(sort(unique(rows$consumer)), collapse = ";"),
    stringsAsFactors = FALSE
  )
}))
registry <- registry[order(registry$option), , drop = FALSE]
rownames(registry) <- NULL
dir.create(dirname(csv_output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  registry, csv_output, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

lines <- c(
  "---",
  "title: Runtime options",
  "description: Generated limits used by neurogeo 6.0",
  "---",
  "",
  "# Runtime options",
  "",
  "This page is generated from every `getOption(\"neurogeo.*\")` call in the",
  "R sources. Set an option only for the current analysis scope and restore it",
  "afterward. These global safety ceilings are distinct from the per-operation",
  "limits declared by `ngeo_resource_budget()`.",
  "",
  "| Option | Default | Accepted value | Lifecycle | Consumer source |",
  "|---|---:|---|---|---|"
)
for (i in seq_len(nrow(registry))) {
  lines <- c(lines, sprintf(
    "| `%s` | `%s` | %s | %s | `%s` |",
    registry$option[[i]], registry$default[[i]], registry$accepted[[i]],
    registry$lifecycle[[i]], registry$consumers[[i]]
  ))
}
lines <- c(
  lines, "", paste0(
    "Generated from ", nrow(registry),
    " unique options; edit the R call site and rerun the generator."
  )
)
dir.create(dirname(markdown_output), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, markdown_output, useBytes = TRUE)
cat("Options registry:", nrow(registry), "options.\n")
