args <- commandArgs(trailingOnly = TRUE)
baseline <- utils::read.csv(
  file.path("inst", "spec", "package-size-baseline-6.0.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stopifnot(
  identical(
    names(baseline),
    c("artifact", "observed_bytes", "maximum_bytes", "measurement")
  ),
  !anyDuplicated(baseline$artifact),
  all(is.finite(baseline$maximum_bytes) & baseline$maximum_bytes > 0)
)

ignore <- readLines(".Rbuildignore", warn = FALSE)
required_ignores <- c(
  "^design$", "^release$", "^website$", "^check-output.*$",
  "^neurogeo5\\.1_refactor_plan_zh\\.md$",
  "^neurogeo_6\\.0_audit_and_improvement_plan_zh\\.md$"
)
if (!all(required_ignores %in% ignore)) {
  stop(".Rbuildignore does not exclude every reviewed internal boundary.")
}

description <- read.dcf("DESCRIPTION")
suggests <- trimws(unlist(strsplit(description[[1L, "Suggests"]], ",")))
suggests <- sub("\\s*\\(.*$", "", suggests)
search_files <- c(
  list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  list.files("tests", pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  list.files("tools", pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  list.files("vignettes", pattern = "[.]Rmd$", recursive = TRUE, full.names = TRUE)
)
source <- paste(unlist(lapply(search_files, readLines, warn = FALSE)), collapse = "\n")
unused <- suggests[!vapply(suggests, function(package) {
  grepl(paste0("\\b", package, "(?:::|\\\"|')"), source, perl = TRUE)
}, logical(1))]
allowed_tooling <- c("covr", "knitr", "rmarkdown", "testthat")
unused <- setdiff(unused, allowed_tooling)
if (length(unused)) {
  stop("Unused Suggests remain: ", paste(unused, collapse = ", "))
}

if (length(args)) {
  tarball <- normalizePath(args[[1L]], mustWork = TRUE)
  maximum <- baseline$maximum_bytes[baseline$artifact == "source-tarball"]
  if (file.info(tarball)$size > maximum) {
    stop("Source tarball exceeds the reviewed size ceiling.")
  }
  contents <- utils::untar(tarball, list = TRUE)
  forbidden <- paste0(
    "(^|/)(design|release|website|check-output[^/]*)(/|$)|",
    "development_plan|audit_and_improvement_plan|refactor_plan"
  )
  if (any(grepl(forbidden, contents, ignore.case = TRUE))) {
    stop("Source tarball contains an internal plan or generated boundary.")
  }
  cat("Source tarball boundary:", file.info(tarball)$size, "bytes.\n")
} else {
  message("Static package boundary passed; tarball size awaits the clean candidate.")
}
cat("Package boundary and Suggests inventory passed.\n")
