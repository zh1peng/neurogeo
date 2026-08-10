args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val302-external-evidence-60.json")
required <- c("digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-302 evidence checking requires: ", paste(missing, collapse = ", "))

root <- file.path("inst", "validation")
report_path <- file.path(root, "val302-external-reference-evidence-6.0.json")
cells_path <- file.path(root, "val302-external-reference-cells-6.0.csv")
receipts_path <- file.path(root, "val302-external-command-receipts-6.0.csv")
if (any(!file.exists(c(report_path, cells_path, receipts_path)))) {
  stop("Committed VAL-302 external evidence is incomplete.")
}
sha_file <- function(path) digest::digest(
  path, algo = "sha256", file = TRUE, serialize = FALSE
)
sha_canonical_text <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection))
  bytes <- readBin(connection, what = "raw", n = file.info(path)$size)
  drop_cr <- bytes == as.raw(13L) & c(bytes[-1L] == as.raw(10L), FALSE)
  digest::digest(bytes[!drop_cr], algo = "sha256", serialize = FALSE)
}
report <- jsonlite::read_json(report_path, simplifyVector = FALSE)
cells <- utils::read.csv(cells_path, stringsAsFactors = FALSE, check.names = FALSE)
receipts <- utils::read.csv(receipts_path, stringsAsFactors = FALSE, check.names = FALSE)
design_path <- file.path(root, "phase3-design-6.0.json")
design_hash <- sha_file(design_path)
locked_hash <- trimws(readLines(file.path(root, "phase3-design-6.0.sha256"), warn = FALSE))
design <- jsonlite::read_json(design_path, simplifyVector = TRUE)
validation <- design$validations[design$validations$id == "VAL-302", , drop = FALSE]
factor_table <- validation$factors
factors <- list(
  geometry = factor_table$geometry[[1L]],
  operation = factor_table$operation[[1L]],
  phantom = factor_table$phantom[[1L]],
  coverage = factor_table$coverage[[1L]]
)
expected <- expand.grid(
  geometry = unlist(factors$geometry), operation = unlist(factors$operation),
  phantom = unlist(factors$phantom), coverage = unlist(factors$coverage),
  stringsAsFactors = FALSE
)
keys <- c("geometry", "operation", "phantom", "coverage")
ordered_key <- function(x) do.call(paste, c(x[keys], sep = "|"))
supported <- as.logical(cells$executed)
checks <- list(
  schema = identical(report$schema[[1L]], "neurogeo/val302-external-reference/1"),
  validation_id = identical(report$validation_id[[1L]], "VAL-302"),
  package_version = identical(
    report$package_version[[1L]], read.dcf("DESCRIPTION", fields = "Version")[[1L]]
  ),
  frozen_design = identical(design_hash, locked_hash) &&
    identical(report$design_sha256[[1L]], design_hash),
  complete_grid = nrow(cells) == 108L && !anyDuplicated(cells[keys]) &&
    setequal(ordered_key(cells), ordered_key(expected)),
  attempted_denominator = all(as.logical(cells$attempted)),
  expected_supported_boundary = sum(supported) == 60L && sum(!supported) == 48L,
  supported_pass = all(cells$cell_status[supported] == "passed") &&
    all(as.logical(cells$gate_pass[supported])),
  unsupported_explicit = all(
    cells$cell_status[!supported] == "unsupported-by-declared-api" &
      nzchar(cells$unsupported_reason[!supported])
  ),
  receipt_denominator = nrow(receipts) == 90L && !anyDuplicated(receipts$receipt_id),
  receipt_success = all(receipts$exit_status == 0L) &&
    all(nzchar(receipts$input_sha256)) && all(nzchar(receipts$output_sha256)),
  all_external_tools_present = setequal(
    unique(receipts$tool),
    c("connectome_workbench", "freesurfer_surface", "freesurfer_volume")
  ),
  artifact_hashes = identical(
    report$artifacts$hash_convention[[1L]],
    "sha256-after-crlf-to-lf-normalization"
  ) && identical(
    report$artifacts$cells$sha256[[1L]], sha_canonical_text(cells_path)
  ) && identical(
    report$artifacts$receipts$sha256[[1L]], sha_canonical_text(receipts_path)
  ),
  report_pass = isTRUE(report$validation_evidence[[1L]])
)
pass <- all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/phase3-validation/1",
  validation_id = "VAL-302",
  package_version = report$package_version[[1L]],
  source_commit = report$source_commit[[1L]],
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  status = if (pass) "passed-with-declared-unsupported-cells" else "failed",
  validation_evidence = pass,
  attempted_cells = nrow(cells), executed_cells = sum(supported),
  unsupported_cells = sum(!supported), command_receipts = nrow(receipts),
  checks = checks,
  evidence_boundary = report$evidence_boundary[[1L]]
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) quit(status = 2L)
