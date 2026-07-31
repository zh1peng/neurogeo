args <- commandArgs(trailingOnly = TRUE)
cache <- if (length(args)) args[[1L]] else
  file.path(".tools", "reference-5.0")
manifest_path <- file.path(
  "inst", "extdata", "reference-5.0", "manifest.csv"
)
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Reference-data verification requires digest.")
}
manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                            check.names = FALSE)
required <- c(
  "name", "file", "source_commit", "source_url", "license_record",
  "terms_url", "redistribution", "size", "sha256", "expected_use"
)
valid <- all(required %in% names(manifest)) &&
  !anyNA(manifest[, required, drop = FALSE]) &&
  all(nzchar(as.matrix(manifest[, required, drop = FALSE]))) &&
  !anyDuplicated(manifest$name) && !anyDuplicated(manifest$file) &&
  all(manifest$redistribution == "download-only") &&
  all(grepl("^[0-9a-f]{40}$", manifest$source_commit)) &&
  all(grepl("^[0-9a-f]{64}$", manifest$sha256))
if (!valid) stop("The 5.0 reference-data manifest is invalid.")

dir.create(cache, recursive = TRUE, showWarnings = FALSE)
verify <- function(path, row) {
  file.exists(path) &&
    identical(as.numeric(file.info(path)$size), as.numeric(row$size)) &&
    identical(
      digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE),
      row$sha256
    )
}
for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, , drop = FALSE]
  output <- file.path(cache, row$file)
  if (!verify(output, row)) {
    temporary <- tempfile(".reference-50-", tmpdir = cache)
    on.exit(unlink(temporary), add = TRUE)
    old_timeout <- getOption("timeout")
    options(timeout = max(300, old_timeout))
    on.exit(options(timeout = old_timeout), add = TRUE)
    status <- tryCatch(
      utils::download.file(row$source_url, temporary, mode = "wb", quiet = FALSE),
      error = identity
    )
    if (inherits(status, "error") || !identical(status, 0L) ||
        !verify(temporary, row)) {
      stop("Download or integrity verification failed for `", row$name, "`.")
    }
    if (file.exists(output)) unlink(output)
    if (!file.rename(temporary, output)) {
      stop("Could not promote verified fixture `", row$name, "`.")
    }
  }
  cat(row$name, normalizePath(output, winslash = "/"), "\n")
}
