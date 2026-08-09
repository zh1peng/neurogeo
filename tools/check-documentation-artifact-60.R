args <- commandArgs(trailingOnly = TRUE)
artifact_path <- if (length(args)) args[[1L]] else
  file.path("website", ".vitepress", "dist", "documentation-build.json")
if (!requireNamespace("digest", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Documentation artifact verification requires digest and jsonlite.")
}
if (!file.exists(artifact_path)) stop("Missing built documentation identity artifact.")
artifact <- jsonlite::read_json(artifact_path, simplifyVector = TRUE)
manifest_path <- file.path("inst", "spec", "documentation-manifest-6.0.csv")
manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
expected <- list(
  schema = "neurogeo/documentation-artifact/1",
  package_version = as.character(read.dcf("DESCRIPTION", fields = "Version")[[1L]]),
  manifest_sha256 = digest::digest(
    manifest_path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  route_count = nrow(manifest)
)
if (!identical(artifact, expected)) {
  stop("Built documentation does not match current route/content identity.")
}
cat("Documentation artifact matches the current package and content manifest.\n")
