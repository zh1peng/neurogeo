path <- file.path("inst", "validation", "phase3-design-6.0.json")
hash_path <- file.path("inst", "validation", "phase3-design-6.0.sha256")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("Phase 3 design validation requires jsonlite and digest.")
}
stopifnot(file.exists(path), file.exists(hash_path))
design <- jsonlite::read_json(path, simplifyVector = FALSE)
stopifnot(
  identical(design$schema, "neurogeo/phase3-design/1"),
  identical(design$status, "frozen-before-primary-results"),
  identical(vapply(design$validations, `[[`, character(1), "id"),
            paste0("VAL-", 301:308)),
  identical(design$common$replicates_per_calibration_cell, 5000L),
  identical(design$common$attempted_replicates_are_denominator, TRUE),
  identical(design$common$primary_results_are_evaluated_once, TRUE)
)
required <- c(
  "id", "simulation_id", "seed_base", "expand_grid", "factors",
  "comparators", "metrics", "gates", "stop_rule"
)
for (validation in design$validations) {
  stopifnot(
    all(required %in% names(validation)),
    identical(validation$expand_grid, TRUE),
    length(validation$factors) > 0L,
    length(validation$comparators) > 0L,
    length(validation$metrics) > 0L,
    length(validation$gates) > 0L,
    nzchar(validation$stop_rule)
  )
}
observed <- digest::digest(path, algo = "sha256", file = TRUE,
                           serialize = FALSE)
expected <- trimws(readLines(hash_path, warn = FALSE))
if (!identical(observed, expected)) {
  stop("Frozen Phase 3 design changed without a new explicit lock.")
}
cat("Frozen Phase 3 design: VAL-301 through VAL-308 are locked.\n")
