if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
required <- c("knitr", "neurogeo")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Quickstart check requires: ", paste(missing, collapse = ", "))

sources <- c(
  en = file.path("vignettes", "getting-started.Rmd"),
  zh = file.path("vignettes", "getting-started-zh.Rmd")
)
text <- vapply(sources, function(path) {
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}, character(1))
forbidden <- c(
  "eval\\s*=\\s*FALSE", "(/path/to|your-file|example/path)",
  "\\$(measures|layers|values|base|history)\\s*<-"
)
for (pattern in forbidden) {
  hit <- vapply(text, grepl, logical(1), pattern = pattern,
                ignore.case = TRUE, perl = TRUE)
  if (any(hit)) stop("Quickstart contains forbidden placeholder or mutation: ", pattern)
}

temporary <- tempfile("quickstart-")
dir.create(temporary)
on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
scripts <- file.path(temporary, c("en.R", "zh.R"))
invisible(knitr::purl(sources[["en"]], output = scripts[[1L]], quiet = TRUE))
invisible(knitr::purl(sources[["zh"]], output = scripts[[2L]], quiet = TRUE))
code <- lapply(scripts, readLines, warn = FALSE, encoding = "UTF-8")
if (!identical(code[[1L]], code[[2L]])) {
  stop("English and Chinese quickstarts do not execute identical code.")
}

environment <- new.env(parent = globalenv())
old_mode <- Sys.getenv("NEUROGEO_TUTORIAL_DATA_MODE", unset = NA_character_)
Sys.setenv(NEUROGEO_TUTORIAL_DATA_MODE = "synthetic")
on.exit({
  if (is.na(old_mode)) Sys.unsetenv("NEUROGEO_TUTORIAL_DATA_MODE") else
    Sys.setenv(NEUROGEO_TUTORIAL_DATA_MODE = old_mode)
}, add = TRUE)
started <- proc.time()[["elapsed"]]
plot_file <- tempfile(fileext = ".pdf")
grDevices::pdf(plot_file)
on.exit({
  if (grDevices::dev.cur() > 1L) grDevices::dev.off()
  unlink(plot_file)
}, add = TRUE)
sys.source(scripts[[1L]], envir = environment)
elapsed <- proc.time()[["elapsed"]] - started
if (!inherits(environment$spatial_result, "ngeo_global_stat") ||
    !inherits(environment$local, "ngeo_lisa") ||
    !inherits(environment$contract, "ngeo_inference_contract") ||
    !is.finite(environment$spatial_result$estimate)) {
  stop("Quickstart did not produce its documented scientific results.")
}
cat(
  "Bilingual quickstart code is identical and executable; internal runtime:",
  format(elapsed, digits = 3L), "seconds.\n"
)
