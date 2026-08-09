workflows <- list.files(
  file.path(".github", "workflows"),
  pattern = "[.]ya?ml$", full.names = TRUE
)
if (!length(workflows)) stop("No GitHub Actions workflows found.")
lines <- unlist(lapply(workflows, readLines, warn = FALSE), use.names = FALSE)
uses <- trimws(grep("uses:", lines, value = TRUE))
remote <- uses[grepl("uses: [^./][^ ]*@", uses)]
if (!length(remote) || any(!grepl("@[0-9a-f]{40}(?:\\s+#.*)?$", remote))) {
  stop("Every remote GitHub Action must be pinned to a 40-character SHA.")
}
required <- c(
  "SECURITY.md", "CONTRIBUTING.md", ".github/CODEOWNERS",
  ".github/dependabot.yml"
)
if (!all(file.exists(required))) {
  stop("Supply-chain governance files are incomplete.")
}
for (path in workflows) {
  workflow <- paste(readLines(path, warn = FALSE), collapse = "\n")
  if (!grepl("permissions:", workflow, fixed = TRUE)) {
    stop("Workflow has no explicit permissions block: ", path)
  }
}
check_workflow <- paste(
  readLines(file.path(".github", "workflows", "R-CMD-check.yaml"), warn = FALSE),
  collapse = "\n"
)
if (!grepl("permissions:\n  contents: read", check_workflow, fixed = TRUE)) {
  stop("R CMD check workflow does not declare read-only contents permission.")
}
cat("Supply-chain baseline: pinned Actions, owners, updates, and security policy.\n")
