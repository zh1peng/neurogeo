args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "release-evidence-50.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("The 5.0 evidence audit requires jsonlite and digest.")
}
read_report <- function(path) {
  if (!file.exists(path)) stop("Missing required evidence: ", path)
  jsonlite::read_json(path, simplifyVector = FALSE)
}
evidence_paths <- c(
  unit = "check-output/unit-50.json",
  api_freeze = "check-output/freeze-50-audit.json",
  group_calibration = "check-output/group-inference-47-full-validation.json",
  support_calibration = "check-output/support-family-48-full-validation.json",
  real_data = "check-output/real-multilayer-50-validation.json",
  performance = "check-output/multilayer-50-performance.json",
  validation_suite = "check-output/validation-suite-50.json",
  installed = "check-output/installed-conformance-50.json"
)
reports <- lapply(evidence_paths, read_report)
check_log <- file.path(
  "check-output", "rcheck-50", "neurogeo.Rcheck", "00check.log"
)
if (!file.exists(check_log)) stop("Missing R CMD check log: ", check_log)
check_lines <- readLines(check_log, warn = FALSE)
check_ok <- any(grepl("^Status: OK$", check_lines))
site_files <- c(
  "website/.vitepress/dist/index.html",
  "website/.vitepress/dist/modules/reference-vs-subject-inference.html",
  "website/.vitepress/dist/en/modules/multilayer-inference.html"
)
source_files <- c(
  "inst/spec/API-5.0.md", "inst/spec/NGCS-5.0.md",
  "inst/spec/migration-5.0.md", "inst/spec/validation-5.0.md",
  "vignettes/multilayer-inference.Rmd",
  "vignettes/reference-vs-subject-inference-zh.Rmd"
)
checks <- list(
  version = identical(as.character(utils::packageVersion("neurogeo")), "5.0.0"),
  unit = isTRUE(reports$unit$pass),
  api_freeze = isTRUE(reports$api_freeze$pass),
  full_group_calibration = isTRUE(reports$group_calibration$pass) &&
    isTRUE(reports$group_calibration$full_calibration),
  full_support_calibration = isTRUE(reports$support_calibration$pass) &&
    isTRUE(reports$support_calibration$full_calibration),
  real_data = isTRUE(reports$real_data$pass),
  full_performance = isTRUE(reports$performance$pass) &&
    isTRUE(reports$performance$full_basis_matrix),
  complete_validation_suite = isTRUE(reports$validation_suite$pass),
  installed_conformance = isTRUE(reports$installed$pass),
  r_cmd_check = check_ok,
  website = all(file.exists(site_files)),
  documentation_sources = all(file.exists(source_files)),
  cross_platform_ci = {
    workflow <- readLines(".github/workflows/R-CMD-check.yaml", warn = FALSE)
    all(vapply(c("ubuntu-latest", "macos-latest", "windows-latest"),
               function(os) any(grepl(os, workflow, fixed = TRUE)), logical(1)))
  }
)
pass <- all(unlist(checks, use.names = FALSE))
artifacts <- lapply(c(evidence_paths, r_cmd_check = check_log), function(path) {
  list(
    path = path, bytes = as.numeric(file.info(path)$size),
    sha256 = digest::digest(
      path, algo = "sha256", file = TRUE, serialize = FALSE
    )
  )
})
dependencies <- as.data.frame(utils::installed.packages()[, c("Package", "Version")],
                              stringsAsFactors = FALSE)
dependencies <- dependencies[order(dependencies$Package), , drop = FALSE]
report <- list(
  schema = "neurogeo/release-evidence-5.0",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  policy = list(pull_request = FALSE, tag = FALSE, github_release = FALSE),
  checks = checks, artifacts = artifacts,
  dependency_versions = dependencies, pass = pass
)
if (!pass) stop("The 5.0 release evidence audit failed.", call. = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE,
                     null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
