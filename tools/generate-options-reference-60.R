args <- commandArgs(trailingOnly = TRUE)
csv_output <- if (length(args)) args[[1L]] else
  file.path("inst", "spec", "options-6.0.csv")
markdown_output <- if (length(args) >= 2L) args[[2L]] else
  file.path("website", "concepts", "options.md")
english_markdown_output <- if (length(args) >= 3L) args[[3L]] else
  file.path("website", "en", "concepts", "options.md")

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
  "title: 运行时选项",
  "description: 从 neurogeo 6.0 源码生成的全局安全上限",
  "---",
  "",
  "# 运行时选项",
  "",
  "**语言：** [English](/en/concepts/options)",
  "",
  "本页由 R 源码中的所有 `getOption(\"neurogeo.*\")` 调用自动生成。",
  "只应在当前分析范围内设置选项，并在分析结束后恢复原值。这里列出的",
  "全局安全上限不同于 `ngeo_resource_budget()` 声明的单次操作资源限额。",
  "",
  "| 选项 | 默认值 | 可接受值 | 生命周期 | 使用该选项的源码 |",
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
    "共从源码生成 ", nrow(registry),
    " 个唯一选项；如需修改，请编辑 R 调用位置并重新运行生成器。"
  )
)
dir.create(dirname(markdown_output), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, markdown_output, useBytes = TRUE)

english_lines <- c(
  "---",
  "title: Runtime options",
  "description: Generated limits used by neurogeo 6.0",
  "---",
  "",
  "# Runtime options",
  "",
  "**Language:** [简体中文](/concepts/options)",
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
  english_lines <- c(english_lines, sprintf(
    "| `%s` | `%s` | %s | %s | `%s` |",
    registry$option[[i]], registry$default[[i]], registry$accepted[[i]],
    registry$lifecycle[[i]], registry$consumers[[i]]
  ))
}
english_lines <- c(
  english_lines, "", paste0(
    "Generated from ", nrow(registry),
    " unique options; edit the R call site and rerun the generator."
  )
)
dir.create(dirname(english_markdown_output), recursive = TRUE,
           showWarnings = FALSE)
writeLines(english_lines, english_markdown_output, useBytes = TRUE)
cat("Options registry:", nrow(registry), "options.\n")
