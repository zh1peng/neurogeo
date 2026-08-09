args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val302-external-prerequisites-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-302 prerequisite checking requires: ", paste(missing, collapse = ", "))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
commands <- c(
  connectome_workbench = "wb_command",
  freesurfer_surface = "mri_surf2surf",
  freesurfer_volume = "mri_vol2vol"
)
paths <- Sys.which(commands)
available <- nzchar(paths)
status <- if (all(available)) "ready-for-external-validation" else
  "blocked-external-prerequisites"
result <- list(
  schema = "neurogeo/phase3-external-prerequisites/1",
  validation_id = "VAL-302", design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = status,
  commands = unname(lapply(seq_along(commands), function(index) list(
    tool = names(commands)[[index]], command = commands[[index]],
    available = available[[index]],
    path = if (available[[index]]) unname(paths[[index]]) else NULL
  ))),
  fixtures = c(
    "inst/extdata/golden/tiny.nii.gz",
    "inst/extdata/golden/tetra.surf.gii",
    "inst/extdata/golden/tiny.dscalar.nii",
    "inst/extdata/reference-4.2.2/manifest.csv"
  ),
  evidence_boundary = paste(
    "This report checks executable prerequisites only. It is not a",
    "Workbench or FreeSurfer parity result and cannot satisfy VAL-302."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!all(available)) quit(status = 2L)
