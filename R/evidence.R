.ngeo_validate_evidence_identity <- function(report, expected, seeds = FALSE) {
  failures <- character()
  required <- c("schema", "suite", "candidate", "checks", "pass")
  missing <- setdiff(required, names(report))
  if (length(missing)) {
    failures <- c(
      failures,
      paste("missing fields:", paste(missing, collapse = ", "))
    )
  }
  if (!identical(report$schema, "neurogeo/evidence-report/1")) {
    failures <- c(failures, "schema mismatch")
  }
  candidate <- report$candidate
  if (!is.list(candidate)) {
    failures <- c(failures, "candidate identity missing")
  } else {
    comparisons <- list(
      package_version = identical(
        candidate$package_version, expected$package_version
      ),
      source_commit = identical(
        candidate$source_commit, expected$source_commit
      ),
      tar_sha256 = identical(
        candidate$candidate_tar$sha256, expected$candidate_tar$sha256
      ),
      dependency_sha256 = identical(
        candidate$dependencies$sha256, expected$dependencies$sha256
      ),
      fixtures = identical(candidate$fixtures, expected$fixtures)
    )
    failures <- c(failures, names(comparisons)[!unlist(comparisons)])
  }
  if (isTRUE(seeds) &&
      (!is.list(report$seeds) || !length(report$seeds))) {
    failures <- c(failures, "seed registry missing")
  }
  if (!isTRUE(report$pass)) {
    failures <- c(failures, "suite did not pass")
  }
  list(pass = !length(failures), failures = unique(failures))
}
