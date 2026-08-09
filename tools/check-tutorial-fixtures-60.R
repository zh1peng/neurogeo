args <- commandArgs(trailingOnly = TRUE)
manifest_path <- if (length(args)) args[[1L]] else
  file.path("inst", "extdata", "tutorial-fixtures-6.0.csv")
if (!file.exists(manifest_path)) stop("Missing tutorial fixture manifest.")
temporary <- tempfile(fileext = ".csv")
on.exit(unlink(temporary), add = TRUE)
status <- system2(
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }),
  c("tools/generate-tutorial-fixtures-60.R", temporary)
)
if (!identical(status, 0L)) stop("Could not regenerate fixture manifest.")
expected <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
observed <- utils::read.csv(temporary, stringsAsFactors = FALSE)
if (!identical(expected, observed)) {
  stop(
    paste(
      "Tutorial fixture hashes or metadata are stale.",
      "Run: Rscript tools/generate-tutorial-fixtures-60.R"
    ),
    call. = FALSE
  )
}
required_workflows <- c("quickstart", "volume", "surface", "grayordinate", "ROI/cohort")
if (!setequal(expected$workflow, required_workflows) ||
    any(expected$license != "CC0-1.0") ||
    any(expected$redistribution != "bundled") ||
    any(!nzchar(expected$expected_result))) {
  stop("Tutorial corpus does not cover all licensed offline workflows.")
}
cat("Tutorial fixture corpus is current and fully licensed.\n")
