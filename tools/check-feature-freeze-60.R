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
      "Regenerate NAMESPACE and then run tools/generate-api-lifecycle-60.R."
    ),
    call. = FALSE
  )
}
s3_methods <- sort(grep("^S3method\\(", namespace, value = TRUE))
declared_s3 <- sort(registry$namespace_entry[registry$type == "s3_method"])
if (!identical(s3_methods, declared_s3)) {
  stop(
    paste(
      "Every registered S3 method must be declared in the lifecycle registry.",
      "Regenerate NAMESPACE and then run tools/generate-api-lifecycle-60.R."
    ),
    call. = FALSE
  )
}

cat(
  "Lifecycle gate:", length(exports), "public exports and",
  length(s3_methods), "registered S3 methods are declared.\n"
)
