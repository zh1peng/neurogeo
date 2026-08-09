paths <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE),
  list.files("website", pattern = "\\.md$", full.names = TRUE, recursive = TRUE),
  "README.md"
)
deny <- c(
  "spatial spatial_weights",
  "distance_method-eligible",
  "non-distance_method",
  "GIFTI distance_method",
  "GIFTI geometry, distance_method",
  "distance distance_method",
  "MVP CIFTI",
  "CIFTI MVP",
  "by this prototype",
  "promotion gate"
)

failures <- character()
for (path in paths) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  matches <- deny[vapply(deny, grepl, logical(1), x = text, fixed = TRUE)]
  if (length(matches)) {
    failures <- c(failures, paste0(path, ": ", paste(matches, collapse = ", ")))
  }
}
if (length(failures)) {
  stop(
    "Controlled terminology check failed:\n", paste(failures, collapse = "\n"),
    call. = FALSE
  )
}
cat("Controlled terminology check passed for", length(paths), "files.\n")
