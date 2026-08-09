current <- file.path(
  "inst", "validation", "stable-api-doc-coverage-6.0.csv"
)
temporary <- tempfile(fileext = ".csv")
on.exit(unlink(temporary), add = TRUE)
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
  "Rscript.exe"
} else {
  "Rscript"
})
status <- system2(
  rscript,
  c("tools/audit-stable-api-docs-60.R", temporary)
)
if (status != 0L || !file.exists(current)) {
  stop("Could not generate or find stable API documentation coverage.")
}
expected <- utils::read.csv(temporary, stringsAsFactors = FALSE)
observed <- utils::read.csv(current, stringsAsFactors = FALSE)
if (!identical(observed, expected)) {
  stop("Stable API documentation coverage is stale.")
}
cat(
  "Stable API documentation coverage is current:",
  sum(observed$complete), "of", nrow(observed), "complete.\n"
)
