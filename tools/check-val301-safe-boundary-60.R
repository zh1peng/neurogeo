args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val301-safe-boundary-60.json")
required <- c("digest", "jsonlite", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-301 safe-boundary checking requires: ", paste(missing, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
capture <- function(expression) tryCatch({
  force(expression)
  NULL
}, error = identity)

tetrahedron <- rbind(
  c(1, 1, 1), c(1, -1, -1), c(-1, 1, -1), c(-1, -1, 1)
) / sqrt(3)
surface <- ngeo_surface(
  list(anatomical = 10 * tetrahedron, sphere = tetrahedron),
  rbind(c(1, 2, 3), c(1, 2, 4), c(1, 3, 4), c(2, 3, 4)),
  values = cbind(signal = seq_len(4L)),
  coordinate_roles = c("anatomical", "registration")
)
spin_error <- capture(ngeo_spin_null(
  surface, "signal", coordinates = "sphere", nsim = 1L
))
point <- ngeo_point(
  cbind(x = 0:5, y = c(0, 1, 0, 1, 0, 1)),
  values = cbind(signal = seq_len(6L))
)
weights <- ngeo_spatial_weights(
  point, method = "knn", k = 2L, symmetry = "union", style = "W"
)
moran_error <- capture(ngeo_moran_null(
  point, weights, "signal", nsim = 1L
))
lifecycle <- utils::read.csv(
  file.path("inst", "spec", "api-lifecycle-6.0.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
experimental_symbols <- c("ngeo_spin_null", "ngeo_moran_null")
lifecycle_rows <- lifecycle[
  lifecycle$type == "export" & lifecycle$symbol %in% experimental_symbols,
]
checks <- list(
  phase3_hash_locked = identical(design_hash, locked_hash),
  spin_default_rejected = inherits(spin_error, "ngeo_error_experimental"),
  moran_default_rejected = inherits(moran_error, "ngeo_error_experimental"),
  lifecycle_experimental = nrow(lifecycle_rows) == 2L &&
    all(lifecycle_rows$lifecycle == "experimental"),
  design_audit_present = file.exists(file.path(
    "inst", "validation", "val301-design-audit-6.0.md"
  ))
)
passed <- all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/phase3-safe-boundary/1",
  validation_id = "VAL-301", design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = if (passed) "safe-boundary-passed-design-amendment-required" else
    "safe-boundary-failed",
  calibration_evidence = FALSE,
  checks = checks,
  evidence_boundary = paste(
    "This report verifies experimental containment only. VAL-301 and C02",
    "remain pending until a pre-result design amendment defines the tested",
    "procedure, statistic, generative null, and multiplicity family."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
