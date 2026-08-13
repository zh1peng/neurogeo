args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else
  file.path("inst", "spec", "api-contracts-6.0.json")
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
required <- c("digest", "jsonlite", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Contract generation requires: ", paste(missing, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))
or_empty <- function(x) if (is.null(x)) "" else x

registry <- utils::read.csv(
  file.path("inst", "spec", "api-lifecycle-6.0.csv"),
  stringsAsFactors = FALSE
)
stable <- registry[registry$lifecycle != "experimental" &
  registry$type %in% c("export", "s3_method"), , drop = FALSE]

rd_alias_hash <- list()
for (path in list.files("man", pattern = "\\.Rd$", full.names = TRUE)) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  aliases <- regmatches(text, gregexpr("(?<=\\\\alias\\{)[^}]+", text, perl = TRUE))[[1L]]
  if (!length(aliases)) next
  hash <- digest::digest(text, algo = "sha256", serialize = FALSE)
  for (alias in aliases) rd_alias_hash[[alias]] <- hash
}

function_contract <- function(symbol, type) {
  lookup <- symbol
  if (identical(type, "s3_method")) lookup <- symbol
  object <- get0(lookup, envir = asNamespace("neurogeo"), inherits = FALSE)
  if (!is.function(object) && identical(type, "s3_method")) {
    split <- strsplit(symbol, "\\.")[[1L]]
    generic <- split[[1L]]
    class <- paste(split[-1L], collapse = ".")
    object <- getS3method(generic, class, optional = TRUE)
  }
  if (!is.function(object)) {
    return(list(
      symbol = symbol,
      type = type,
      formals = NULL,
      condition_classes = character(),
      documentation_sha256 = or_empty(rd_alias_hash[[symbol]])
    ))
  }
  formal_values <- lapply(formals(object), function(value) {
    paste(deparse(value, width.cutoff = 500L), collapse = " ")
  })
  body_text <- paste(deparse(body(object), width.cutoff = 500L), collapse = "\n")
  conditions <- regmatches(
    body_text,
    gregexpr("ngeo_(?:error|warning)_[a-z0-9_]+", body_text, perl = TRUE)
  )[[1L]]
  declared_conditions <- attr(object, "ngeo_condition_classes", exact = TRUE)
  if (!is.null(declared_conditions) && !is.character(declared_conditions)) {
    stop("Declared condition classes must be a character vector: ", symbol)
  }
  conditions <- c(conditions, declared_conditions)
  if (identical(conditions, character(0))) conditions <- character()
  list(
    symbol = symbol,
    type = type,
    formals = formal_values,
    condition_classes = sort(unique(conditions)),
    documentation_sha256 = or_empty(rd_alias_hash[[symbol]])
  )
}
contracts <- Map(function(symbol, type) {
  function_contract(symbol, type)
}, stable$symbol, stable$type)
report <- list(
  schema = "neurogeo/api-contracts/1",
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  stable_count = length(contracts),
  contracts = contracts
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, auto_unbox = TRUE, pretty = TRUE, null = "null"
)
cat("Stable API contracts:", length(contracts), "entries.\n")
