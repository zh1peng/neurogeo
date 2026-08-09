expected_csv <- tempfile(fileext = ".csv")
expected_markdown <- tempfile(fileext = ".md")
expected_english_markdown <- tempfile(fileext = ".md")
on.exit(unlink(c(
  expected_csv, expected_markdown, expected_english_markdown
)), add = TRUE)
status <- system2(
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }),
  c(
    "tools/generate-options-reference-60.R",
    expected_csv,
    expected_markdown,
    expected_english_markdown
  )
)
if (status != 0L) stop("Could not generate the expected options reference.")
current <- c(
  file.path("inst", "spec", "options-6.0.csv"),
  file.path("website", "concepts", "options.md"),
  file.path("website", "en", "concepts", "options.md")
)
expected <- c(expected_csv, expected_markdown, expected_english_markdown)
if (!all(file.exists(current)) ||
    !all(vapply(seq_along(current), function(i) {
      identical(readBin(current[[i]], "raw", file.info(current[[i]])$size),
                readBin(expected[[i]], "raw", file.info(expected[[i]])$size))
    }, logical(1)))) {
  stop("Generated options reference is stale.")
}
registry <- utils::read.csv(current[[1L]], stringsAsFactors = FALSE)
stopifnot(
  !anyDuplicated(registry$option),
  all(grepl("^neurogeo[.]", registry$option)),
  all(nzchar(registry$default)),
  all(nzchar(registry$accepted)),
  all(registry$lifecycle %in% c("stable", "experimental", "deprecated"))
)
cat("Options reference is current:", nrow(registry), "options.\n")
