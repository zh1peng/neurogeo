if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
if (!requireNamespace("digest", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Release construction requires digest and jsonlite.")
}

description <- read.dcf("DESCRIPTION")
package <- description[1L, "Package"]
version <- description[1L, "Version"]
archive_name <- paste0(package, "_", version, ".tar.gz")
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
release_dir <- file.path(root, "release")
dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)
run_stamp <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
run_dir <- file.path(
  release_dir,
  "runs",
  paste(package, version, run_stamp, sep = "-")
)
if (dir.exists(run_dir)) {
  run_dir <- paste0(run_dir, "-", Sys.getpid())
}
check_dir <- file.path(run_dir, "check")
dir.create(check_dir, recursive = TRUE)

validation_catalog <- data.frame(
  minimum_version = c(
    "1.0.0", "1.0.0", "1.0.0", "1.3.0", "2.0.0",
    "2.1.0", "2.1.0", "2.1.0", "2.2.0", "2.3.0",
    "2.4.0", "2.5.0", "2.6.0", "2.7.0", "2.8.0",
    "2.9.0", "2.9.1", "3.0.0", "3.1.0", "3.2.0",
    "3.3.0", "3.4.0", "3.5.0", "4.1.0"
  ),
  report = c(
    "conformance.json",
    "performance.json",
    "external-workflows.json",
    "simulation.json",
    "support-map-conformance.json",
    "support-builder-conformance.json",
    "support-workflows.json",
    "support-inference-validation.json",
    "support-uncertainty-validation.json",
    "support-inference-23-validation.json",
    "spatial-models-24-validation.json",
    "scalable-io-25-validation.json",
    "execution-26-validation.json",
    "model-uncertainty-27-validation.json",
    "space-graph-28-validation.json",
    "interoperability-29-validation.json",
    "maintenance-291-validation.json",
    "schema-30-validation.json",
    "file-backed-31-validation.json",
    "resampling-32-validation.json",
    "spatiotemporal-33-validation.json",
    "iterative-models-34-validation.json",
    "reproducibility-35-validation.json",
    "format-reference-41-validation.json"
  ),
  stringsAsFactors = FALSE
)
required_reports <- validation_catalog$report[vapply(
  validation_catalog$minimum_version,
  function(minimum) utils::compareVersion(version, minimum) >= 0L,
  logical(1)
)]

specification_catalog <- data.frame(
  minimum_version = c(
    "1.0.0", "2.0.0", "2.1.0", "2.2.0", "2.3.0",
    "2.4.0", "2.5.0", "2.6.0", "2.7.0", "2.8.0",
    "2.9.0", "2.9.1", "3.0.0", "3.1.0", "3.2.0",
    "3.3.0", "3.4.0", "3.5.0"
  ),
  specification = paste(
    "NGCS",
    c(
      "1.0", "2.0", "2.1", "2.2", "2.3", "2.4",
      "2.5", "2.6", "2.7", "2.8", "2.9", "2.9.1",
      "3.0", "3.1", "3.2", "3.3", "3.4", "3.5"
    )
  ),
  stringsAsFactors = FALSE
)
eligible_specification <- vapply(
  specification_catalog$minimum_version,
  function(minimum) utils::compareVersion(version, minimum) >= 0L,
  logical(1)
)
specification <- tail(
  specification_catalog$specification[eligible_specification],
  1L
)
missing_reports <- required_reports[
  !file.exists(file.path(release_dir, required_reports))
]
if (length(missing_reports)) {
  stop(
    "Run release validations first; missing: ",
    paste(missing_reports, collapse = ", ")
  )
}
validation_dir <- file.path(run_dir, "validation")
dir.create(validation_dir)
validation_reports <- file.path(validation_dir, required_reports)
if (!all(file.copy(
  file.path(release_dir, required_reports),
  validation_reports
))) {
  stop("Could not snapshot release validation reports.")
}

r_binary <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "R.exe" else "R"
)
environment <- c(
  LC_ALL = "C",
  LANG = "C",
  LC_COLLATE = "C",
  LC_CTYPE = "C",
  LC_MONETARY = "C",
  LC_TIME = "C",
  `_R_CHECK_CRAN_INCOMING_` = "FALSE",
  `_R_CHECK_CRAN_INCOMING_REMOTE_` = "FALSE",
  `_R_CHECK_SYSTEM_CLOCK_` = "FALSE"
)
if (dir.exists(file.path(root, ".r-lib"))) {
  environment[["R_LIBS_USER"]] <- normalizePath(
    file.path(root, ".r-lib"),
    winslash = "/"
  )
}
environment[["R_PROFILE_USER"]] <- normalizePath(
  file.path(root, "tools", "check-profile.R"),
  winslash = "/",
  mustWork = TRUE
)
check_repository <- normalizePath(
  file.path(root, "tools", "check-repository"),
  winslash = "/",
  mustWork = TRUE
)
environment[["NEUROGEO_CHECK_REPOSITORY"]] <- paste0(
  "file:///",
  check_repository
)
if (!nzchar(Sys.getenv("R_QPDF"))) {
  qpdf_candidates <- list.files(
    file.path(root, ".tools"),
    pattern = if (.Platform$OS.type == "windows") {
      "^qpdf[.]exe$"
    } else {
      "^qpdf$"
    },
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(qpdf_candidates)) {
    environment[["R_QPDF"]] <- normalizePath(
      qpdf_candidates[[1L]],
      winslash = "/"
    )
  }
}
old_environment <- Sys.getenv(names(environment), unset = NA_character_)
do.call(Sys.setenv, as.list(environment))
on.exit({
  restore <- !is.na(old_environment)
  if (any(restore)) {
    do.call(Sys.setenv, as.list(old_environment[restore]))
  }
  if (any(!restore)) {
    Sys.unsetenv(names(old_environment)[!restore])
  }
}, add = TRUE)

build_log <- tempfile("neurogeo-build-", fileext = ".log")
build_status <- system2(
  r_binary,
  c("CMD", "build", "."),
  stdout = build_log,
  stderr = build_log
)
invisible(file.copy(
  build_log,
  file.path(run_dir, "build.log"),
  overwrite = TRUE
))
if (!identical(build_status, 0L)) {
  stop("R CMD build failed; see ", file.path(run_dir, "build.log"), ".")
}
source_archive <- file.path(root, archive_name)
if (!file.exists(source_archive)) {
  stop("R CMD build did not create ", source_archive)
}
release_archive <- file.path(run_dir, archive_name)
if (!file.copy(source_archive, release_archive)) {
  stop("Could not copy the source archive into the release run.")
}

old_directory <- getwd()
setwd(check_dir)
on.exit(setwd(old_directory), add = TRUE)
check_log <- file.path(check_dir, "R-CMD-check.log")
check_status <- system2(
  r_binary,
  c(
    "CMD",
    "check",
    "--as-cran",
    "--no-manual",
    shQuote(release_archive)
  ),
  stdout = check_log,
  stderr = check_log
)
setwd(old_directory)
if (!identical(check_status, 0L)) {
  stop("R CMD check failed; see ", check_log, ".")
}

check_output <- file.path(check_dir, paste0(package, ".Rcheck"))
check_summary <- file.path(check_output, "00check.log")
if (!file.exists(check_summary) ||
    !any(grepl("^Status: OK$", readLines(check_summary, warn = FALSE)))) {
  stop("R CMD check did not report Status: OK.")
}

session_path <- file.path(run_dir, "session-info.txt")
writeLines(capture.output(sessionInfo()), session_path)

artifact_paths <- c(
  release_archive,
  validation_reports,
  session_path,
  check_summary
)
manifest <- data.frame(
  file = substring(
    normalizePath(artifact_paths, winslash = "/", mustWork = TRUE),
    nchar(root) + 2L
  ),
  size = file.info(artifact_paths)$size,
  md5 = unname(tools::md5sum(artifact_paths)),
  sha256 = vapply(
    artifact_paths,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  ),
  stringsAsFactors = FALSE
)
rownames(manifest) <- NULL
jsonlite::write_json(
  list(
    package = package,
    version = version,
    specification = specification,
    generated_at_utc = format(
      Sys.time(),
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    ),
    local_check = paste(
      "R CMD check --as-cran: Status OK",
      "(CRAN incoming and system-clock network checks disabled locally)"
    ),
    platform = R.version$platform,
    artifacts = manifest
  ),
  file.path(run_dir, "manifest.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)
writeLines(
  substring(
    normalizePath(run_dir, winslash = "/", mustWork = TRUE),
    nchar(root) + 2L
  ),
  file.path(release_dir, "LATEST")
)
cat(normalizePath(release_archive, winslash = "/"), "\n")
