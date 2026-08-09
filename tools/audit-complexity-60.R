args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else
  file.path("inst", "validation", "function-complexity-6.0.csv")

branch_calls <- c(
  "if", "for", "while", "repeat", "&&", "||", "switch", "tryCatch"
)

count_branches <- function(value) {
  if (!is.recursive(value)) return(0L)
  own <- as.integer(
    is.call(value) && as.character(value[[1L]])[[1L]] %in% branch_calls
  )
  own + sum(vapply(as.list(value), count_branches, integer(1)))
}

rows <- list()
for (path in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) {
  expressions <- parse(path, keep.source = TRUE)
  references <- attr(expressions, "srcref")
  for (expression_index in seq_along(expressions)) {
    expression <- expressions[[expression_index]]
    if (!is.call(expression) ||
        !as.character(expression[[1L]])[[1L]] %in% c("<-", "=") ||
        length(expression) < 3L || !is.call(expression[[3L]]) ||
        !identical(expression[[3L]][[1L]], as.name("function"))) {
      next
    }
    reference <- references[[expression_index]]
    if (is.null(reference)) next
    rows[[length(rows) + 1L]] <- data.frame(
      source = gsub("\\\\", "/", path),
      function_name = paste(deparse(expression[[2L]]), collapse = ""),
      line_count = as.integer(reference[[3L]] - reference[[1L]] + 1L),
      branch_points = count_branches(expression[[3L]][[3L]]),
      stringsAsFactors = FALSE
    )
  }
}

inventory <- do.call(rbind, rows)
inventory <- inventory[order(
  -inventory$line_count, -inventory$branch_points,
  inventory$source, inventory$function_name
), , drop = FALSE]
rownames(inventory) <- NULL
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  inventory, output, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)
cat("Function complexity inventory:", nrow(inventory), "functions.\n")
