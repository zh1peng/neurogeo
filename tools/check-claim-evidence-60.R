matrix_path <- file.path(
  "inst", "validation", "claim-evidence-matrix-6.0.csv"
)
plan_path <- file.path("inst", "validation", "analysis-plan-6.0.md")
lock_path <- file.path("inst", "validation", "analysis-plan-6.0.sha256")
stopifnot(file.exists(matrix_path), file.exists(plan_path), file.exists(lock_path))
claims <- utils::read.csv(matrix_path, stringsAsFactors = FALSE, check.names = FALSE)
required <- c(
  "claim_id", "claim", "lifecycle", "estimand", "formula_reference",
  "simulation_id", "dataset_id", "figure_id", "workflow", "primary_gate",
  "stop_rule", "evidence_status"
)
stopifnot(
  identical(names(claims), required),
  !anyDuplicated(claims$claim_id),
  all(nzchar(as.matrix(claims))),
  all(claims$lifecycle %in% c("stable", "experimental")),
  all(claims$evidence_status %in% c(
    "pending-preregistered", "current-internal", "external-replicated"
  )),
  all(file.exists(claims$formula_reference))
)
combined <- paste(
  digest::digest(matrix_path, algo = "sha256", file = TRUE, serialize = FALSE),
  digest::digest(plan_path, algo = "sha256", file = TRUE, serialize = FALSE),
  sep = ":"
)
expected <- trimws(readLines(lock_path, warn = FALSE))
if (!identical(combined, expected)) {
  stop("Frozen claim-evidence matrix or analysis plan changed without a new lock.")
}
cat(
  "Frozen claim-evidence matrix:", nrow(claims), "claims;",
  sum(claims$evidence_status == "pending-preregistered"), "pending gates.\n"
)
