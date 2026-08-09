path <- file.path("inst", "spec", "validation-registry-6.0.csv")
registry <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
required <- c(
  "suite_id", "level", "script", "inputs", "arguments", "output",
  "timeout_seconds", "evidence_schema", "network", "required"
)
stopifnot(
  identical(names(registry), required),
  nrow(registry) > 0L,
  !anyDuplicated(registry$suite_id),
  !anyDuplicated(registry$script),
  all(registry$level %in% c("smoke", "full", "external", "performance")),
  all(file.exists(registry$script)),
  all(nzchar(registry$inputs)),
  all(nzchar(registry$output)),
  all(is.finite(registry$timeout_seconds) & registry$timeout_seconds > 0),
  all(tolower(as.character(registry$network)) %in% c("true", "false")),
  all(tolower(as.character(registry$required)) %in% c("true", "false"))
)
runner <- paste(readLines("tools/run-validation-suite.ps1", warn = FALSE), collapse = "\n")
if (!grepl("validation-registry-6.0.csv", runner, fixed = TRUE)) {
  stop("Local validation runner does not consume the canonical registry.")
}
workflow <- paste(
  readLines(".github/workflows/R-CMD-check.yaml", warn = FALSE),
  collapse = "\n"
)
if (!grepl("check-validation-registry-60.R", workflow, fixed = TRUE)) {
  stop("CI does not validate the canonical validation registry.")
}
cat(
  "Validation registry:", nrow(registry), "suites across",
  paste(sort(unique(registry$level)), collapse = ", "), "levels.\n"
)
