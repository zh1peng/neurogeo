args <- commandArgs(trailingOnly = TRUE)
manifest_path <- if (length(args)) args[[1L]] else
  file.path("inst", "spec", "documentation-manifest-6.0.csv")
if (!file.exists(manifest_path)) stop("Missing documentation manifest.")

temporary <- tempfile(fileext = ".csv")
on.exit(unlink(temporary), add = TRUE)
status <- system2(
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }),
  c("tools/generate-documentation-manifest-60.R", temporary)
)
if (!identical(status, 0L)) stop("Could not regenerate documentation manifest.")
expected <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                            check.names = FALSE)
observed <- utils::read.csv(temporary, stringsAsFactors = FALSE,
                            check.names = FALSE)
if (!identical(expected, observed)) {
  stop(
    paste(
      "Documentation routes or source content are stale.",
      "Run: Rscript tools/generate-documentation-manifest-60.R"
    ),
    call. = FALSE
  )
}
if (anyDuplicated(expected$source) || anyDuplicated(expected$route) ||
    any(!file.exists(expected$source))) {
  stop("Documentation sources and routes must be present and unique.")
}
code_sources <- expected$code_source[nzchar(expected$code_source)]
if (any(!file.exists(code_sources)) || any(!nzchar(
    expected$code_sha256[nzchar(expected$code_source)]
))) {
  stop("Canonical tutorial code is missing or unhashed.")
}

vignettes <- sort(gsub("\\\\", "/", list.files(
  "vignettes", pattern = "\\.Rmd$", full.names = TRUE
)))
declared_vignettes <- sort(expected$source[grepl("^vignettes/", expected$source)])
if (!identical(vignettes, declared_vignettes)) {
  stop("Every vignette must appear exactly once in the documentation manifest.")
}

paired <- expected$translation_status == "paired"
if (any(!expected$counterpart_route[paired] %in% expected$route)) {
  stop("A bilingual route points to an undeclared counterpart.")
}
expected_edit <- paste0(
  "https://github.com/zh1peng/neurogeo/edit/main/", expected$source
)
if (!identical(expected$edit_url, expected_edit)) {
  stop("Documentation edit links are stale or do not target their source.")
}

read_utf8 <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  value <- iconv(rawToChar(raw), from = "UTF-8", to = "UTF-8", sub = NA)
  if (is.na(value)) stop("Invalid UTF-8 documentation source: ", path)
  value
}
invisible(lapply(expected$source, read_utf8))

valid_routes <- unique(sub("/$", "", expected$route))
markdown_sources <- expected$source[grepl("\\.(md|Rmd)$", expected$source)]
for (path in markdown_sources) {
  text <- read_utf8(path)
  matches <- gregexpr("\\]\\((/[^)#?[:space:]]+)", text, perl = TRUE)
  links <- regmatches(text, matches)[[1L]]
  if (!length(links) || identical(links, character(0))) next
  links <- sub("^\\]\\(", "", links)
  links <- sub("/$", "", links)
  links <- links[!links %in% c("/logo.png", "/favicon.png")]
  links <- links[!grepl("^/api/", links)]
  missing <- setdiff(unique(links), valid_routes)
  if (length(missing)) {
    stop("Undeclared internal route in ", path, ": ",
         paste(missing, collapse = ", "))
  }
}
cat(
  "Documentation manifest is current:", nrow(expected),
  "sources and", length(unique(expected$route)), "routes.\n"
)
