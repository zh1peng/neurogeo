args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else
  file.path("inst", "validation", "stable-api-doc-coverage-6.0.csv")
strict <- "--require-complete" %in% args

lifecycle <- utils::read.csv(
  file.path("inst", "spec", "api-lifecycle-6.0.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
symbols <- lifecycle[
  lifecycle$type == "export" & lifecycle$lifecycle == "stable",
  c("symbol", "owner"),
  drop = FALSE
]

flatten_rd <- function(value) {
  paste(unlist(lapply(value, function(item) {
    if (is.character(item)) item else if (is.list(item)) flatten_rd(item) else ""
  })), collapse = " ")
}
records <- list()
for (path in sort(list.files("man", pattern = "[.]Rd$", full.names = TRUE))) {
  rd <- tools::parse_Rd(path)
  tags <- vapply(rd, attr, character(1), "Rd_tag")
  aliases <- vapply(rd[tags == "\\alias"], flatten_rd, character(1))
  if (!length(aliases)) next
  sections <- rd[tags == "\\section"]
  section_titles <- tolower(vapply(sections, function(section) {
    flatten_rd(section[[1L]])
  }, character(1)))
  text <- tolower(flatten_rd(rd))
  record <- list(
    source = gsub("\\\\", "/", path),
    purpose = any(tags == "\\title") && any(tags == "\\description"),
    when_not = any(grepl("when|use|scope|limitation|not", section_titles)) ||
      grepl("must not|do not use|not intended|requires", text),
    units = grepl("unit|millimet|voxel|vertex|second|time axis|distance", text),
    assumptions = any(grepl("assumption|contract|requirement", section_titles)) ||
      grepl("assum|requires|must ", text),
    return_schema = any(tags == "\\value"),
    example = any(tags == "\\examples"),
    see_also = any(tags == "\\seealso"),
    reference = any(tags == "\\references"),
    validation = grepl("validation|conformance|fixture|tested against", text)
  )
  for (alias in aliases) records[[alias]] <- record
}

coverage <- do.call(rbind, lapply(seq_len(nrow(symbols)), function(i) {
  symbol <- symbols$symbol[[i]]
  record <- records[[symbol]]
  if (is.null(record)) {
    record <- list(
      source = "", purpose = FALSE, when_not = FALSE, units = FALSE,
      assumptions = FALSE, return_schema = FALSE, example = FALSE,
      see_also = FALSE, reference = FALSE, validation = FALSE
    )
  }
  data.frame(
    symbol = symbol,
    owner = symbols$owner[[i]],
    source = record$source,
    purpose = record$purpose,
    when_not = record$when_not,
    units = record$units,
    assumptions = record$assumptions,
    return_schema = record$return_schema,
    example = record$example,
    see_also = record$see_also,
    reference = record$reference,
    validation = record$validation,
    stringsAsFactors = FALSE
  )
}))
coverage$complete <- apply(
  coverage[setdiff(names(coverage), c("symbol", "owner", "source"))],
  1L,
  all
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(coverage, output, row.names = FALSE, fileEncoding = "UTF-8")
cat(
  "Stable API documentation:", sum(coverage$complete), "of", nrow(coverage),
  "exports satisfy every reviewed content field.\n"
)
if (strict && any(!coverage$complete)) {
  missing <- colSums(!coverage[vapply(coverage, is.logical, logical(1))])
  print(sort(missing, decreasing = TRUE))
  quit(status = 2L)
}
