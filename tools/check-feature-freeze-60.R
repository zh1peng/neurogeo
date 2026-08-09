namespace <- readLines("NAMESPACE", warn = FALSE)
exports <- sub(
  "^export\\((.*)\\)$",
  "\\1",
  grep("^export\\(", namespace, value = TRUE)
)
exports <- sort(exports)
registry <- utils::read.csv(
  file.path("inst", "spec", "api-lifecycle-6.0.csv"),
  stringsAsFactors = FALSE
)
declared <- sort(registry$symbol[registry$type == "export"])
if (!identical(exports, declared)) {
  stop(
    paste(
      "Every public export must be declared in the lifecycle registry.",
      "Run tools/generate-api-lifecycle-60.R after an approved API change."
    ),
    call. = FALSE
  )
}

cat("Lifecycle gate:", length(exports), "public exports are declared.\n")
