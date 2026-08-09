if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
workflows <- c("volume", "surface", "cifti", "roi-cohort")
for (workflow in workflows) {
  code <- file.path("inst", "tutorial-code", paste0("workflow-", workflow, ".R"))
  english <- file.path("vignettes", paste0("workflow-", workflow, ".Rmd"))
  chinese <- file.path("vignettes", paste0("workflow-", workflow, "-zh.Rmd"))
  stopifnot(file.exists(code), file.exists(english), file.exists(chinese))
  expected_reference <- paste0("workflow-", workflow, ".R")
  expected_chunk <- paste0("workflow-", workflow)
  for (page in c(english, chinese)) {
    text <- paste(readLines(page, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (!grepl(expected_reference, text, fixed = TRUE) ||
        !grepl(paste0("{r ", expected_chunk, "}"), text, fixed = TRUE) ||
        grepl("eval\\s*=\\s*FALSE|/fake/|C:/fake|\\$measures\\s*<-|\\$layers\\s*<-", text)) {
      stop("Format tutorial is not bound safely to canonical code: ", page)
    }
  }
  environment <- new.env(parent = asNamespace("neurogeo"))
  sys.source(code, envir = environment)
}
cat("Four bilingual format workflows are paired and executable.\n")
