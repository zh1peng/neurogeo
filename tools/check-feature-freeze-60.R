if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Feature-freeze validation requires digest.")
}

namespace <- readLines("NAMESPACE", warn = FALSE)
exports <- sub(
  "^export\\((.*)\\)$",
  "\\1",
  grep("^export\\(", namespace, value = TRUE)
)
exports <- sort(exports)
expected_count <- 226L
expected_sha256 <-
  "713fe6ff24b3bcd8468254e2a9a0577112c0df37d2ffc37c6b7ab7b7670a2019"
observed_sha256 <- digest::digest(
  paste(exports, collapse = "\n"),
  algo = "sha256",
  serialize = FALSE
)

if (length(exports) != expected_count ||
    !identical(observed_sha256, expected_sha256)) {
  stop(
    paste(
      "The Phase 0 feature freeze prohibits public-export changes.",
      "Expected", expected_count, "exports with SHA-256", expected_sha256,
      "but observed", length(exports), "with SHA-256", observed_sha256
    ),
    call. = FALSE
  )
}

cat("Phase 0 feature freeze: 226 public exports unchanged.\n")
