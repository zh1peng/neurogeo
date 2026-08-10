args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val302-external-prerequisites-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("VAL-302 prerequisite checking requires: ", paste(missing, collapse = ", "))
}

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))

manifest_path <- file.path(
  "inst", "validation", "val302-tool-manifest-6.0.csv"
)
manifest <- utils::read.csv(
  manifest_path, stringsAsFactors = FALSE, check.names = FALSE,
  strip.white = TRUE
)
manifest_fields <- c(
  "tool", "command", "required_version", "platform",
  "environment_variable", "local_hint", "archive_hint", "archive_sha256",
  "executable_sha256", "source_url", "license_url", "license_required"
)
stopifnot(
  identical(names(manifest), manifest_fields), nrow(manifest) == 3L,
  !anyDuplicated(manifest$tool),
  setequal(manifest$tool, c(
    "connectome_workbench", "freesurfer_surface", "freesurfer_volume"
  )),
  all(nzchar(manifest$command)), all(nzchar(manifest$required_version)),
  all(grepl("^https://", manifest$source_url)),
  all(tolower(as.character(manifest$license_required)) %in% c("true", "false"))
)

first_existing <- function(candidates) {
  candidates <- candidates[nzchar(candidates)]
  if (!length(candidates)) return("")
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) return("")
  normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE)
}
remote_capture <- function(host, command) {
  if (!grepl("^[A-Za-z0-9._-]+$", host)) {
    stop("Unsafe FreeSurfer validation host.")
  }
  output <- suppressWarnings(system2(
    "ssh", c("-o", "BatchMode=yes", host, shQuote(command)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(output = output, status = status)
}
resolve_tool <- function(row) {
  remote_host <- if (startsWith(row$tool, "freesurfer_")) {
    Sys.getenv("NEUROGEO_FREESURFER_HOST", unset = "")
  } else {
    ""
  }
  override <- Sys.getenv(row$environment_variable, unset = "")
  remote <- nzchar(remote_host)
  path_value <- if (remote) "" else unname(Sys.which(row$command))
  path <- if (remote) {
    candidate <- c(override, row$local_hint)
    candidate <- candidate[nzchar(candidate)]
    if (length(candidate)) candidate[[1L]] else ""
  } else {
    first_existing(c(override, row$local_hint, path_value))
  }
  if (remote && nzchar(path) &&
      !grepl("^/[A-Za-z0-9._/-]+$", path)) {
    stop("Unsafe remote FreeSurfer executable path.")
  }
  available <- if (remote && nzchar(path)) {
    identical(remote_capture(
      remote_host,
      paste("test -x", shQuote(path, type = "sh"))
    )$status, 0L)
  } else {
    nzchar(path)
  }
  version_args <- if (identical(row$tool, "connectome_workbench")) {
    "-version"
  } else {
    "--version"
  }
  version_output <- character()
  version_exit <- NULL
  if (available) {
    if (remote) {
      version <- remote_capture(
        remote_host,
        paste(shQuote(path, type = "sh"), version_args)
      )
      version_output <- version$output
      version_exit <- version$status
    } else {
      version_output <- suppressWarnings(system2(
        path, version_args, stdout = TRUE, stderr = TRUE
      ))
      version_exit <- attr(version_output, "status")
      if (is.null(version_exit)) version_exit <- 0L
    }
  }
  version_text <- paste(version_output, collapse = "\n")
  version_ok <- available && identical(version_exit, 0L) &&
    grepl(row$required_version, version_text, fixed = TRUE)
  executable_hash <- if (available && remote) {
    hash <- remote_capture(
      remote_host,
      paste("sha256sum", shQuote(path, type = "sh"))
    )
    if (identical(hash$status, 0L) && length(hash$output)) {
      strsplit(hash$output[[1L]], "[[:space:]]+")[[1L]][[1L]]
    } else {
      NULL
    }
  } else if (available) {
    digest::digest(
      path, algo = "sha256", file = TRUE, serialize = FALSE
    )
  } else NULL
  expected_executable_hash <- trimws(row$executable_sha256)
  executable_hash_ok <- available && (
    !nzchar(expected_executable_hash) ||
      identical(executable_hash, expected_executable_hash)
  )
  archive_path <- first_existing(row$archive_hint)
  archive_hash <- if (nzchar(archive_path)) digest::digest(
    archive_path, algo = "sha256", file = TRUE, serialize = FALSE
  ) else NULL
  expected_archive_hash <- trimws(row$archive_sha256)
  archive_hash_ok <- !nzchar(expected_archive_hash) || (
    nzchar(archive_path) && identical(archive_hash, expected_archive_hash)
  )
  license_required <- identical(
    tolower(as.character(row$license_required)), "true"
  )
  license_candidates <- c(
    Sys.getenv("FS_LICENSE", unset = ""),
    if (remote && nzchar(path)) {
      file.path(dirname(dirname(path)), "license.txt")
    } else if (nzchar(Sys.getenv("FREESURFER_HOME", unset = ""))) {
      file.path(Sys.getenv("FREESURFER_HOME"), "license.txt")
    } else ""
  )
  license_path <- if (license_required && remote) {
    candidate <- license_candidates[nzchar(license_candidates)]
    candidate <- if (length(candidate)) candidate[[1L]] else ""
    if (nzchar(candidate) && identical(remote_capture(
      remote_host,
      paste("test -s", shQuote(candidate, type = "sh"))
    )$status, 0L)) candidate else ""
  } else if (license_required) {
    first_existing(license_candidates)
  } else ""
  license_ok <- !license_required || nzchar(license_path)
  license_hash <- if (remote && nzchar(license_path)) {
    hash <- remote_capture(
      remote_host,
      paste("sha256sum", shQuote(license_path, type = "sh"))
    )
    if (identical(hash$status, 0L) && length(hash$output)) {
      strsplit(hash$output[[1L]], "[[:space:]]+")[[1L]][[1L]]
    } else NULL
  } else if (nzchar(license_path)) {
    digest::digest(
      license_path, algo = "sha256", file = TRUE, serialize = FALSE
    )
  } else NULL
  ready <- available && version_ok && executable_hash_ok &&
    archive_hash_ok && license_ok
  list(
    tool = row$tool, command = row$command,
    required_version = row$required_version, platform = row$platform,
    execution_host = if (remote) remote_host else "local",
    source_url = row$source_url, license_url = row$license_url,
    available = available, ready = ready,
    path = if (available) path else NULL,
    version_exit = version_exit,
    version_output = if (length(version_output)) version_output else NULL,
    version_ok = version_ok,
    executable_sha256 = executable_hash,
    executable_hash_ok = executable_hash_ok,
    archive_path = if (nzchar(archive_path)) archive_path else NULL,
    archive_sha256 = archive_hash,
    archive_hash_ok = archive_hash_ok,
    license_required = license_required,
    license_available = license_ok,
    license_path = if (nzchar(license_path)) license_path else NULL,
    license_sha256 = license_hash
  )
}
tools <- lapply(seq_len(nrow(manifest)), function(index) {
  resolve_tool(as.list(manifest[index, , drop = FALSE]))
})
names(tools) <- manifest$tool

fixture_paths <- c(
  "inst/extdata/golden/tiny.nii.gz",
  "inst/extdata/golden/tetra.surf.gii",
  "inst/extdata/golden/tiny.dscalar.nii",
  "inst/extdata/reference-4.2.2/manifest.csv"
)
fixture_checks <- unname(lapply(fixture_paths, function(path) list(
  path = path, present = file.exists(path),
  sha256 = if (file.exists(path)) digest::digest(
    path, algo = "sha256", file = TRUE, serialize = FALSE
  ) else NULL
)))
all_ready <- all(vapply(tools, `[[`, logical(1), "ready")) &&
  all(vapply(fixture_checks, `[[`, logical(1), "present"))
status <- if (all_ready) "ready-for-external-validation" else
  "blocked-external-prerequisites"
result <- list(
  schema = "neurogeo/phase3-external-prerequisites/2",
  validation_id = "VAL-302", design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = status, all_ready = all_ready,
  tool_manifest_sha256 = digest::digest(
    manifest_path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  tools = unname(tools), fixtures = fixture_checks,
  evidence_boundary = paste(
    "This report verifies executable identity, versions, available pinned",
    "archives, licenses, and fixture presence only. It is not Workbench or",
    "FreeSurfer parity evidence and cannot satisfy VAL-302."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!all_ready) quit(status = 2L)
