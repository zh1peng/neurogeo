args <- commandArgs(trailingOnly = TRUE)
source_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(
  if (length(args)) args[[1L]] else file.path("check-output", "p0-evidence-60"),
  winslash = "/",
  mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

git_status <- system2(
  "git", c("status", "--porcelain"), stdout = TRUE, stderr = TRUE
)
if (length(git_status)) {
  stop("P0 attestation requires a clean source tree.", call. = FALSE)
}

r_binary <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
  "R.exe"
} else {
  "R"
})
rscript_binary <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
work <- tempfile("neurogeo-p0-")
dir.create(work)
on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
library_path <- file.path(work, "library")
build_path <- file.path(work, "build")
check_path <- file.path(work, "check")
dir.create(library_path)
dir.create(build_path)
dir.create(check_path)

run <- function(command, arguments, wd = source_root, env = character()) {
  previous <- setwd(wd)
  on.exit(setwd(previous), add = TRUE)
  output <- system2(command, arguments, stdout = TRUE, stderr = TRUE, env = env)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  cat(paste(output, collapse = "\n"), "\n")
  if (status != 0L) {
    stop("Command failed: ", command, " ", paste(arguments, collapse = " "),
         call. = FALSE)
  }
  invisible(output)
}

archive <- file.path(work, "source.tar")
snapshot <- file.path(work, "source", "neurogeo")
dir.create(snapshot, recursive = TRUE)
run(
  "git",
  c("archive", "--format=tar", paste0("--output=", archive), "HEAD")
)
utils::untar(archive, exdir = snapshot)

run(
  r_binary,
  c(
    "CMD", "build", snapshot, "--no-manual",
    "--no-resave-data"
  ),
  wd = build_path
)
candidate <- list.files(
  build_path,
  pattern = "^neurogeo_[0-9.]+[.]tar[.]gz$",
  full.names = TRUE
)
if (length(candidate) != 1L) stop("Build did not produce one candidate tarball.")
candidate <- normalizePath(candidate, winslash = "/")
run(
  r_binary,
  c("CMD", "INSTALL", paste0("--library=", library_path), candidate),
  wd = work
)
previous_r_libs_user <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
previous_candidate <- Sys.getenv("NEUROGEO_CANDIDATE_TAR", unset = NA_character_)
library_paths <- c(
  normalizePath(library_path, winslash = "/"),
  normalizePath(.libPaths(), winslash = "/", mustWork = TRUE)
)
if (dir.exists(file.path(source_root, ".r-lib"))) {
  library_paths <- c(
    library_paths,
    normalizePath(file.path(source_root, ".r-lib"), winslash = "/")
  )
}
library_paths <- unique(library_paths)
Sys.setenv(
  R_LIBS_USER = paste(library_paths, collapse = .Platform$path.sep),
  NEUROGEO_CANDIDATE_TAR = candidate
)
on.exit({
  if (is.na(previous_r_libs_user)) {
    Sys.unsetenv("R_LIBS_USER")
  } else {
    Sys.setenv(R_LIBS_USER = previous_r_libs_user)
  }
  if (is.na(previous_candidate)) {
    Sys.unsetenv("NEUROGEO_CANDIDATE_TAR")
  } else {
    Sys.setenv(NEUROGEO_CANDIDATE_TAR = previous_candidate)
  }
}, add = TRUE)

run(rscript_binary, c("tools/run-unit-60.R", file.path(
  output_dir, "unit-60.json"
)))
run(rscript_binary, c(
  "tools/run-audit-corpus-60.R", candidate,
  file.path(output_dir, "audit-corpus-60.json")
))
run(rscript_binary, c(
  "tools/check-doc-entrypoints-60.R",
  file.path(output_dir, "doc-entrypoints-60.json")
))
run(rscript_binary, "tools/check-feature-freeze-60.R")
run(rscript_binary, "tools/check-user-terminology-60.R")

run(
  r_binary,
  c("CMD", "check", candidate, "--no-manual", "--no-build-vignettes"),
  wd = check_path
)
check_log <- file.path(check_path, "neurogeo.Rcheck", "00check.log")
run(rscript_binary, c(
  "tools/report-rcheck-60.R", candidate, check_log,
  file.path(output_dir, "r-cmd-check-60.json")
))
run(rscript_binary, c(
  "tools/aggregate-p0-evidence-60.R", candidate, output_dir,
  file.path(output_dir, "attestation.json")
))

cat("P0 attestation:", file.path(output_dir, "attestation.json"), "\n")
