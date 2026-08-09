args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "github-governance-60.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("GitHub governance checking requires jsonlite.")
}

description <- read.dcf("DESCRIPTION", fields = c("Package", "Version", "URL"))
url <- strsplit(description[[1L, "URL"]], ",", fixed = TRUE)[[1L]][[1L]]
repository <- sub("^https://github[.]com/", "", trimws(url))
repository <- sub("[.]git$", "", repository)
if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
  stop("Could not derive the GitHub repository from DESCRIPTION.")
}
gh <- Sys.which("gh")
if (!nzchar(gh)) stop("GitHub CLI is required for remote governance evidence.")

run_gh <- function(arguments) {
  value <- suppressWarnings(system2(
    gh, arguments, stdout = TRUE, stderr = TRUE
  ))
  status <- attr(value, "status")
  if (is.null(status)) status <- 0L
  list(output = value, status = status)
}
protection_call <- run_gh(c(
  "api", paste0("repos/", repository, "/branches/main/protection")
))
if (protection_call$status != 0L) {
  stop("Could not read protected-main state: ",
       paste(protection_call$output, collapse = "\n"))
}
protection <- jsonlite::fromJSON(
  paste(protection_call$output, collapse = "\n"), simplifyVector = TRUE
)
commit_call <- run_gh(c(
  "api", paste0("repos/", repository, "/commits/main"), "--jq", ".sha"
))
if (commit_call$status != 0L) stop("Could not read the remote main commit.")
remote_main <- trimws(commit_call$output[[1L]])

required_contexts <- c(
  "minimum R 4.2 with Imports only",
  "ubuntu-latest (R release)",
  "line coverage and examples",
  "Phase 0 candidate attestation"
)
contexts <- protection$required_status_checks$contexts
if (is.data.frame(contexts) && "context" %in% names(contexts)) {
  contexts <- contexts$context
}
contexts <- as.character(contexts)
reviews <- protection$required_pull_request_reviews
checks <- list(
  branch_protected = TRUE,
  required_status_checks = all(required_contexts %in% contexts) &&
    isTRUE(protection$required_status_checks$strict),
  admins_enforced = isTRUE(protection$enforce_admins$enabled),
  approving_review_required =
    is.numeric(reviews$required_approving_review_count) &&
      reviews$required_approving_review_count >= 1L,
  code_owner_review_required = isTRUE(reviews$require_code_owner_reviews),
  last_push_approval_required = isTRUE(reviews$require_last_push_approval),
  stale_reviews_dismissed = isTRUE(reviews$dismiss_stale_reviews),
  linear_history_required = isTRUE(protection$required_linear_history$enabled),
  conversations_resolved =
    isTRUE(protection$required_conversation_resolution$enabled),
  force_push_disabled = !isTRUE(protection$allow_force_pushes$enabled),
  branch_deletion_disabled = !isTRUE(protection$allow_deletions$enabled)
)
pass <- all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/github-governance/1",
  package = description[[1L, "Package"]],
  package_version = description[[1L, "Version"]],
  repository = repository, branch = "main", remote_main = remote_main,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  required_contexts = required_contexts,
  observed_contexts = contexts,
  checks = checks, pass = pass,
  evidence_boundary = paste(
    "This is a live GitHub API observation. Branch settings remain mutable",
    "and must be rechecked when the release receipt is finalized."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) quit(status = 2L)
