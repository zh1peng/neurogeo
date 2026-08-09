args <- commandArgs(trailingOnly = TRUE)
strict <- "--require-release" %in% args
output_arg <- grep("^--output=", args, value = TRUE)
receipt_arg <- grep("^--evidence=", args, value = TRUE)
output <- if (length(output_arg)) sub("^--output=", "", output_arg[[1L]]) else
  file.path("check-output", "external-release-readiness-60.json")
receipt_path <- if (length(receipt_arg)) sub("^--evidence=", "", receipt_arg[[1L]]) else
  file.path("release", "external-release-evidence-6.0.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("Release readiness checking requires jsonlite and digest.")
}

required <- c(
  "CITATION.cff", "codemeta.json", "inst/CITATION",
  "inst/spec/release-process-6.0.md",
  "inst/spec/external-release-evidence-schema-6.0.json",
  "inst/spec/validation-registry-6.0.csv",
  "inst/spec/package-size-baseline-6.0.csv"
)
if (!all(file.exists(required))) stop("Release metadata is incomplete.")
codemeta <- jsonlite::read_json("codemeta.json", simplifyVector = TRUE)
version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
citation <- paste(readLines("CITATION.cff", warn = FALSE), collapse = "\n")
metadata_consistent <- identical(codemeta$name, "neurogeo") &&
  identical(codemeta$version, version) &&
  grepl(paste0("version: ", version), citation, fixed = TRUE)
if (!metadata_consistent) stop("Release metadata is internally inconsistent.")

receipt <- if (file.exists(receipt_path)) {
  jsonlite::read_json(receipt_path, simplifyVector = FALSE)
} else {
  NULL
}
scalar <- function(value, default = NULL) {
  if (is.null(value) || !length(value)) default else value[[1L]]
}
git_status <- function(arguments) {
  output <- suppressWarnings(system2(
    "git", arguments, stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) 0L else as.integer(status)
}
git_output <- function(arguments) {
  suppressWarnings(system2("git", arguments, stdout = TRUE, stderr = TRUE))
}
sha256 <- function(value) is.character(value) && length(value) == 1L &&
  grepl("^[0-9a-f]{64}$", value)

tag <- scalar(receipt$tag)
source_commit <- scalar(receipt$source_commit)
tar_path <- scalar(receipt$source_tar_path)
tar_hash <- scalar(receipt$source_tar_sha256)
tag_exists <- !is.null(tag) && tag %in% git_output(c("tag", "--list"))
tag_commit <- if (tag_exists) scalar(git_output(c("rev-list", "-n", "1", tag))) else NULL
signed_tag <- tag_exists && git_status(c("tag", "-v", tag)) == 0L
tar_present <- !is.null(tar_path) && file.exists(tar_path)
observed_tar_hash <- if (tar_present) digest::digest(
  tar_path, algo = "sha256", file = TRUE, serialize = FALSE
) else NULL
reviews <- receipt$governance$independent_reviews
review_commits <- if (is.null(reviews)) character() else vapply(
  reviews, function(review) scalar(review$approved_commit, ""), character(1)
)
review_urls <- if (is.null(reviews)) character() else vapply(
  reviews, function(review) scalar(review$url, ""), character(1)
)
github_hash <- scalar(receipt$github_release$asset_sha256)
zenodo_hash <- scalar(receipt$zenodo$asset_sha256)
doi <- scalar(receipt$zenodo$doi, "")
checks <- list(
  metadata_consistent = metadata_consistent,
  receipt_present = !is.null(receipt),
  receipt_schema = identical(
    scalar(receipt$schema), "neurogeo/external-release-evidence/1"
  ),
  package_version = identical(scalar(receipt$package_version), version),
  candidate_tag = !is.null(tag) && grepl(
    paste0("^v", gsub("\\.", "\\\\.", version), "(-rc[0-9]+)?$"), tag
  ),
  tag_exists = tag_exists,
  signed_tag = signed_tag,
  receipt_commit_matches_tag = !is.null(source_commit) &&
    identical(source_commit, tag_commit),
  source_tar_present = tar_present,
  source_tar_sha256 = sha256(tar_hash) && identical(tar_hash, observed_tar_hash),
  github_release = grepl(
    "^https://github[.]com/.+/releases/tag/", scalar(receipt$github_release$url, "")
  ) && sha256(github_hash) && identical(github_hash, tar_hash),
  zenodo_record = grepl("^10[.]", doi) && grepl(
    "^https://zenodo[.]org/records/", scalar(receipt$zenodo$record_url, "")
  ) && sha256(zenodo_hash) && identical(zenodo_hash, tar_hash) &&
    grepl(doi, citation, fixed = TRUE),
  protected_main = isTRUE(scalar(receipt$governance$protected_main, FALSE)),
  independent_review = length(review_commits) >= 1L &&
    all(review_commits == source_commit) &&
    all(grepl("^https://", review_urls))
)
pass <- all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/external-release-readiness/1",
  package_version = version,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  receipt_path = receipt_path,
  status = if (pass) "complete" else "pending",
  checks = checks,
  pass = pass,
  evidence_boundary = paste(
    "The receipt is untracked external evidence. URLs and governance records",
    "still require human review; source metadata alone cannot assert them."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (strict && !pass) quit(status = 2L)
