expected_path <- file.path("inst", "spec", "api-lifecycle-6.0.csv")
if (!file.exists(expected_path)) stop("Missing lifecycle registry.")
temporary <- tempfile(fileext = ".csv")
on.exit(unlink(temporary), add = TRUE)
status <- system2(
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }),
  c("tools/generate-api-lifecycle-60.R", temporary)
)
if (!identical(status, 0L)) stop("Could not regenerate lifecycle registry.")
expected <- utils::read.csv(expected_path, stringsAsFactors = FALSE)
observed <- utils::read.csv(temporary, stringsAsFactors = FALSE)
if (!identical(expected, observed)) {
  stop(
    paste(
      "Public API lifecycle registry is stale.",
      "Run: Rscript tools/generate-api-lifecycle-60.R"
    ),
    call. = FALSE
  )
}
if (!identical(sum(expected$type == "s3_method"), 96L)) {
  stop("The registered S3 method count changed without an ADR.")
}
cat("Public API lifecycle registry is complete and current.\n")
