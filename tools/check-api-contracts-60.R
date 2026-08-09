expected <- file.path("inst", "spec", "api-contracts-6.0.json")
if (!file.exists(expected)) stop("Missing stable API contract snapshot.")
temporary <- tempfile(fileext = ".json")
on.exit(unlink(temporary), add = TRUE)
rscript <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
status <- system2(rscript, c("tools/generate-api-contracts-60.R", temporary))
if (!identical(status, 0L)) stop("Could not regenerate API contracts.")
expected_contract <- jsonlite::read_json(expected, simplifyVector = FALSE)
observed_contract <- jsonlite::read_json(temporary, simplifyVector = FALSE)
if (!identical(expected_contract, observed_contract)) {
  stop(
    paste(
      "Stable API contract snapshot is stale.",
      "Review the change and run tools/generate-api-contracts-60.R."
    ),
    call. = FALSE
  )
}
cat("Stable API formals, conditions, and documentation snapshots are current.\n")
