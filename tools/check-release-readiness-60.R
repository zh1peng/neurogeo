args <- commandArgs(trailingOnly = TRUE)
strict <- "--require-release" %in% args
required <- c(
  "CITATION.cff", "codemeta.json", "inst/CITATION",
  "inst/spec/release-process-6.0.md",
  "inst/spec/validation-registry-6.0.csv",
  "inst/spec/package-size-baseline-6.0.csv"
)
if (!all(file.exists(required))) stop("Release metadata is incomplete.")
codemeta <- jsonlite::read_json("codemeta.json", simplifyVector = TRUE)
version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
citation <- paste(readLines("CITATION.cff", warn = FALSE), collapse = "\n")
stopifnot(
  identical(codemeta$name, "neurogeo"),
  identical(codemeta$version, version),
  grepl(paste0("version: ", version), citation, fixed = TRUE)
)
tags <- system2("git", "tag --list", stdout = TRUE, stderr = TRUE)
release_tag <- paste0("v", version)
candidate_tags <- tags[startsWith(tags, paste0(release_tag, "-rc")) |
                         tags == release_tag]
external <- list(
  immutable_tag = length(candidate_tags) > 0L,
  signed_tag = FALSE,
  github_release = FALSE,
  zenodo_doi = grepl("doi.org/10[.]", citation),
  protected_main_and_independent_review = FALSE
)
if (strict && !all(unlist(external, use.names = FALSE))) {
  print(external)
  stop("External immutable-release evidence is incomplete.")
}
cat(
  "Release metadata is internally consistent; external release gate:",
  if (all(unlist(external, use.names = FALSE))) "complete" else "pending",
  "\n"
)
