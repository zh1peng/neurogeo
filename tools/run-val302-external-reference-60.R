args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output_dir <- if (length(args)) args[[1L]] else
  file.path("inst", "validation")
host <- if (length(args) >= 2L) args[[2L]] else
  Sys.getenv("NEUROGEO_FREESURFER_HOST", unset = "")
if (!nzchar(host) || !grepl("^[A-Za-z0-9._-]+$", host)) {
  stop("Provide a safe FreeSurfer SSH host as argument 2 or NEUROGEO_FREESURFER_HOST.")
}
required <- c(
  "digest", "jsonlite", "Matrix", "RNifti", "freesurferformats"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-302 runner requires: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages(library(neurogeo))

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
command_result <- function(command, arguments) {
  output <- suppressWarnings(system2(
    command, arguments, stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(output = output, status = status)
}
must_run <- function(command, arguments, label) {
  result <- command_result(command, arguments)
  if (!identical(result$status, 0L)) {
    stop(label, " failed:\n", paste(result$output, collapse = "\n"), call. = FALSE)
  }
  result
}
remote_run <- function(command, label) must_run(
  "ssh", c("-o", "BatchMode=yes", host, shQuote(command)), label
)
safe_id <- function(...) {
  gsub("[^A-Za-z0-9]+", "-", paste(..., sep = "-"))
}
metric <- function(observed, expected, mass = FALSE) {
  difference <- as.numeric(observed) - as.numeric(expected)
  list(
    rmse = sqrt(mean(difference^2)),
    maximum_error = max(abs(difference)),
    relative_mass_error = if (mass) {
      abs(sum(observed) - sum(expected)) / max(abs(sum(expected)), 1e-30)
    } else NA_real_
  )
}
hash_values <- function(values) digest::digest(
  as.numeric(values), algo = "sha256"
)

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- sha_file(design_path)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
design <- jsonlite::read_json(design_path, simplifyVector = TRUE)
validation <- design$validations[design$validations$id == "VAL-302", , drop = FALSE]
if (nrow(validation) != 1L) stop("Frozen VAL-302 design is missing or duplicated.")
factor_table <- validation$factors
factors <- list(
  geometry = factor_table$geometry[[1L]],
  operation = factor_table$operation[[1L]],
  phantom = factor_table$phantom[[1L]],
  coverage = factor_table$coverage[[1L]]
)
grid <- expand.grid(
  geometry = unlist(factors$geometry),
  operation = unlist(factors$operation),
  phantom = unlist(factors$phantom),
  coverage = unlist(factors$coverage),
  stringsAsFactors = FALSE
)
stopifnot(nrow(grid) == 108L, !anyDuplicated(grid))

source_commit <- trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE))
source_dirty <- length(system2(
  "git", c("status", "--porcelain"), stdout = TRUE
)) > 0L
if (source_dirty) stop("VAL-302 evidence must run from a clean source tree.")
package_version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]

dir.create(".tools", showWarnings = FALSE)
work <- tempfile(paste0("val302-", substr(source_commit, 1L, 8L), "-"), tmpdir = ".tools")
inputs <- file.path(work, "inputs")
local_outputs <- file.path(work, "local-outputs")
remote_download <- file.path(work, "remote-download")
dir.create(inputs, recursive = TRUE)
dir.create(local_outputs, recursive = TRUE)
dir.create(remote_download, recursive = TRUE)
remote_name <- paste0("neurogeo-val302-", substr(source_commit, 1L, 12L))
remote_home_result <- remote_run("pwd", "remote home discovery")
remote_home <- trimws(tail(remote_home_result$output, 1L))
if (!grepl("^/[^\r\n]+$", remote_home)) {
  stop("FreeSurfer host did not report a safe absolute home path.")
}
remote_root <- paste0(sub("/+$", "", remote_home), "/", remote_name)

prerequisite_path <- file.path(work, "prerequisites.json")
previous_host <- Sys.getenv("NEUROGEO_FREESURFER_HOST", unset = NA_character_)
Sys.setenv(NEUROGEO_FREESURFER_HOST = host)
on.exit({
  if (is.na(previous_host)) Sys.unsetenv("NEUROGEO_FREESURFER_HOST") else
    Sys.setenv(NEUROGEO_FREESURFER_HOST = previous_host)
}, add = TRUE)
must_run(
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
  c("tools/check-val302-external-prerequisites-60.R", prerequisite_path),
  "VAL-302 prerequisite check"
)
prerequisites <- jsonlite::read_json(prerequisite_path, simplifyVector = FALSE)
if (!isTRUE(prerequisites$all_ready[[1L]])) stop("VAL-302 prerequisites are not ready.")
tool_rows <- prerequisites$tools
tool_by_name <- function(name) {
  index <- which(vapply(tool_rows, function(x) identical(x$tool[[1L]], name), logical(1)))
  if (length(index) != 1L) stop("Missing tool metadata: ", name)
  tool_rows[[index]]
}
workbench <- tool_by_name("connectome_workbench")
surface_tool <- tool_by_name("freesurfer_surface")
volume_tool <- tool_by_name("freesurfer_volume")
wb_command <- workbench$path[[1L]]
fs_surface <- surface_tool$path[[1L]]
fs_volume <- volume_tool$path[[1L]]
fs_home <- dirname(dirname(fs_surface))
fs_license <- surface_tool$license_path[[1L]]
fs_subjects <- paste0(fs_home, "/subjects")
remote_sphere <- paste0(fs_subjects, "/fsaverage6/surf/lh.sphere.reg")

remote_run(
  paste(
    "mkdir -p", shQuote(paste0(remote_root, "/inputs"), type = "sh"),
    shQuote(paste0(remote_root, "/outputs"), type = "sh")
  ),
  "remote VAL-302 directory creation"
)
sphere_path <- file.path(inputs, "fsaverage6-lh.sphere.reg")
must_run(
  "scp", c("-q", paste0(host, ":", remote_sphere), shQuote(sphere_path)),
  "FreeSurfer sphere retrieval"
)
sphere <- freesurferformats::read.fs.surface(sphere_path)
surface_xyz <- sphere$vertices / 100
surface_faces <- sphere$faces
freesurferformats::write.fs.surface.gii(
  file.path(inputs, "fsaverage6-lh.sphere.surf.gii"),
  sphere$vertices, sphere$faces
)

phantom_values <- function(coordinates, phantom, eligible) {
  values <- switch(
    phantom,
    constant = rep(1, nrow(coordinates)),
    linear = coordinates[, 1L] + 0.5 * coordinates[, 2L] - 0.25 * coordinates[, 3L],
    `smooth-sinusoid` = sin(pi * coordinates[, 1L]) +
      0.5 * cos(pi * coordinates[, 2L]),
    `compact-mass` = {
      value <- numeric(nrow(coordinates))
      index <- which(eligible)[[1L]]
      value[[index]] <- 1
      value
    },
    stop("Unknown phantom: ", phantom)
  )
  values[!eligible] <- 0
  values
}
coverage_mask <- function(coordinates, coverage) {
  switch(
    coverage,
    complete = rep(TRUE, nrow(coordinates)),
    `partial-mask` = coordinates[, 1L] >= 0,
    disconnected = abs(coordinates[, 1L]) >= 0.45,
    stop("Unknown coverage: ", coverage)
  )
}

volume_index <- as.matrix(expand.grid(x = -2:2, y = -2:2, z = -2:2))
volume_template <- array(0, dim = c(5L, 5L, 5L))
volume_template_path <- file.path(inputs, "volume-template.nii.gz")
RNifti::writeNifti(volume_template, volume_template_path)

fixture_index <- list()
for (coverage in unlist(factors$coverage)) {
  volume_mask <- coverage_mask(volume_index / 2, coverage)
  volume_mask_path <- file.path(inputs, paste0("volume-mask-", safe_id(coverage), ".nii.gz"))
  RNifti::writeNifti(array(as.numeric(volume_mask), dim = c(5L, 5L, 5L)), volume_mask_path)
  surface_mask <- coverage_mask(surface_xyz, coverage)
  surface_mask_mgh <- file.path(inputs, paste0("surface-mask-", safe_id(coverage), ".mgh"))
  surface_mask_gii <- file.path(inputs, paste0("surface-mask-", safe_id(coverage), ".func.gii"))
  freesurferformats::write.fs.mgh(surface_mask_mgh, as.numeric(surface_mask))
  freesurferformats::write.fs.morph.gii(surface_mask_gii, as.numeric(surface_mask))
  for (phantom in unlist(factors$phantom)) {
    volume_values <- phantom_values(volume_index / 2, phantom, volume_mask)
    volume_path <- file.path(
      inputs, paste0("volume-", safe_id(phantom, coverage), ".nii.gz")
    )
    RNifti::writeNifti(array(volume_values, dim = c(5L, 5L, 5L)), volume_path)
    surface_values <- phantom_values(surface_xyz, phantom, surface_mask)
    surface_mgh <- file.path(
      inputs, paste0("surface-", safe_id(phantom, coverage), ".mgh")
    )
    surface_gii <- file.path(
      inputs, paste0("surface-", safe_id(phantom, coverage), ".func.gii")
    )
    freesurferformats::write.fs.mgh(surface_mgh, surface_values)
    freesurferformats::write.fs.morph.gii(surface_gii, surface_values)
    fixture_index[[paste("volume", phantom, coverage, sep = "|")]] <- list(
      values = volume_values, path = volume_path, mask = volume_mask,
      mask_path = volume_mask_path
    )
    fixture_index[[paste("surface", phantom, coverage, sep = "|")]] <- list(
      values = surface_values, mgh = surface_mgh, gii = surface_gii,
      mask = surface_mask, mask_mgh = surface_mask_mgh, mask_gii = surface_mask_gii
    )
  }
}

must_run(
  "scp", c("-q", "-r", shQuote(inputs), paste0(host, ":", remote_name, "/")),
  "VAL-302 input upload"
)

receipts <- list()
add_receipt <- function(
    tool, geometry, operation, phantom, coverage,
    command, result, input_path, output_path, execution_host) {
  if (!file.exists(output_path)) stop("Missing external output: ", output_path)
  id <- sprintf("VAL302-CMD-%03d", length(receipts) + 1L)
  receipts[[length(receipts) + 1L]] <<- data.frame(
    receipt_id = id,
    tool = tool,
    execution_host = execution_host,
    geometry = geometry,
    operation = operation,
    phantom = phantom,
    coverage = coverage,
    command = command,
    exit_status = result$status,
    input_sha256 = sha_file(input_path),
    output_sha256 = sha_file(output_path),
    console_sha256 = digest::digest(
      paste(result$output, collapse = "\n"), algo = "sha256", serialize = FALSE
    ),
    stringsAsFactors = FALSE
  )
  id
}

wb_pending <- list()
fs_pending <- list()
for (coverage in unlist(factors$coverage)) {
  for (phantom in c("coverage-mask", unlist(factors$phantom))) {
    volume_fixture <- if (identical(phantom, "coverage-mask")) {
      fixture_index[[paste("volume", "constant", coverage, sep = "|")]]$mask_path
    } else {
      fixture_index[[paste("volume", phantom, coverage, sep = "|")]]$path
    }
    for (operation in c("gather-nearest", "gather-linear-or-barycentric")) {
      method_wb <- if (identical(operation, "gather-nearest")) {
        "ENCLOSING_VOXEL"
      } else "TRILINEAR"
      method_fs <- if (identical(operation, "gather-nearest")) "nearest" else "trilin"
      stem <- safe_id("volume", operation, phantom, coverage)
      wb_output <- file.path(local_outputs, paste0("wb-", stem, ".nii.gz"))
      wb_args <- c(
        "-volume-resample", volume_fixture, volume_template_path,
        method_wb, wb_output
      )
      wb_result <- must_run(wb_command, wb_args, paste("Workbench", stem))
      wb_pending[[stem]] <- list(
        result = wb_result, input = volume_fixture, output = wb_output,
        command = paste(shQuote(wb_command), paste(shQuote(wb_args), collapse = " ")),
        geometry = "volume", operation = operation, phantom = phantom,
        coverage = coverage
      )
      remote_input <- basename(volume_fixture)
      remote_output <- paste0("fs-", stem, ".nii.gz")
      fs_command <- paste(
        paste0("export FREESURFER_HOME=", shQuote(fs_home, type = "sh"), ";"),
        paste0("export SUBJECTS_DIR=", shQuote(fs_subjects, type = "sh"), ";"),
        paste0("export FS_LICENSE=", shQuote(fs_license, type = "sh"), ";"),
        shQuote(fs_volume, type = "sh"),
        "--mov", shQuote(paste0(remote_root, "/inputs/", remote_input), type = "sh"),
        "--targ", shQuote(paste0(remote_root, "/inputs/", basename(volume_template_path)), type = "sh"),
        "--regheader --o", shQuote(paste0(remote_root, "/outputs/", remote_output), type = "sh"),
        "--no-save-reg --interp", method_fs
      )
      fs_result <- remote_run(fs_command, paste("FreeSurfer", stem))
      fs_pending[[stem]] <- list(
        result = fs_result, input = volume_fixture,
        remote_output = remote_output, command = fs_command,
        geometry = "volume", operation = operation, phantom = phantom,
        coverage = coverage
      )
    }
  }
  for (phantom in c("coverage-mask", unlist(factors$phantom))) {
    surface_fixture <- fixture_index[[
      paste("surface", if (identical(phantom, "coverage-mask")) "constant" else phantom,
            coverage, sep = "|")
    ]]
    input_mgh <- if (identical(phantom, "coverage-mask")) {
      surface_fixture$mask_mgh
    } else surface_fixture$mgh
    input_gii <- if (identical(phantom, "coverage-mask")) {
      surface_fixture$mask_gii
    } else surface_fixture$gii
    stem_fs <- safe_id("surface", "gather-nearest", phantom, coverage)
    remote_output <- paste0("fs-", stem_fs, ".mgh")
    fs_command <- paste(
      paste0("export FREESURFER_HOME=", shQuote(fs_home, type = "sh"), ";"),
      paste0("export SUBJECTS_DIR=", shQuote(fs_subjects, type = "sh"), ";"),
      paste0("export FS_LICENSE=", shQuote(fs_license, type = "sh"), ";"),
      shQuote(fs_surface, type = "sh"),
      "--srcsubject fsaverage6 --trgsubject fsaverage6 --hemi lh",
      "--sval", shQuote(paste0(remote_root, "/inputs/", basename(input_mgh)), type = "sh"),
      "--tval", shQuote(paste0(remote_root, "/outputs/", remote_output), type = "sh"),
      "--mapmethod nnf"
    )
    fs_result <- remote_run(fs_command, paste("FreeSurfer", stem_fs))
    fs_pending[[stem_fs]] <- list(
      result = fs_result, input = input_mgh,
      remote_output = remote_output, command = fs_command,
      geometry = "surface", operation = "gather-nearest", phantom = phantom,
      coverage = coverage
    )
    stem_wb <- safe_id("surface", "gather-linear-or-barycentric", phantom, coverage)
    wb_output <- file.path(local_outputs, paste0("wb-", stem_wb, ".func.gii"))
    wb_args <- c(
      "-metric-resample", input_gii,
      file.path(inputs, "fsaverage6-lh.sphere.surf.gii"),
      file.path(inputs, "fsaverage6-lh.sphere.surf.gii"),
      "BARYCENTRIC", wb_output
    )
    wb_result <- must_run(wb_command, wb_args, paste("Workbench", stem_wb))
    wb_pending[[stem_wb]] <- list(
      result = wb_result, input = input_gii, output = wb_output,
      command = paste(shQuote(wb_command), paste(shQuote(wb_args), collapse = " ")),
      geometry = "surface", operation = "gather-linear-or-barycentric",
      phantom = phantom, coverage = coverage
    )
  }
}

must_run(
  "scp", c(
    "-q", "-r", paste0(host, ":", remote_name, "/outputs"),
    shQuote(remote_download)
  ),
  "VAL-302 output download"
)
remote_outputs <- file.path(remote_download, "outputs")
for (item in wb_pending) {
  add_receipt(
    "connectome_workbench", item$geometry, item$operation,
    item$phantom, item$coverage, item$command, item$result,
    item$input, item$output, "local"
  )
}
for (item in fs_pending) {
  output_path <- file.path(remote_outputs, item$remote_output)
  add_receipt(
    if (identical(item$geometry, "surface")) "freesurfer_surface" else
      "freesurfer_volume",
    item$geometry, item$operation, item$phantom, item$coverage,
    item$command, item$result, item$input, output_path, host
  )
}
receipts <- do.call(rbind, receipts)

identity_path <- function(space) {
  registry <- ngeo_coordinate_space_registry(list(space))
  graph <- ngeo_transform_graph(registry)
  hash <- ngeo_coordinate_space_hash(space)
  ngeo_transform_path(graph, hash, hash)
}
volume_space <- ngeo_coordinate_space("VAL302-volume", kind = "volume")
volume_source <- ngeo_volume(
  values = array(0, dim = c(5L, 5L, 5L)), dim = c(5L, 5L, 5L),
  affine = diag(4), measures = ngeo_measure(support_behavior = "intensive"),
  coordinate_space = volume_space, index_base = "zero"
)
volume_target <- ngeo_volume(
  dim = c(5L, 5L, 5L), affine = diag(4),
  coordinate_space = volume_space, index_base = "zero"
)
volume_maps <- lapply(c(nearest = "nearest", linear = "linear", overlap = "overlap"), function(method) {
  plan <- ngeo_resampling_plan(
    volume_source, volume_target, identity_path(volume_space), method = method
  )
  ngeo_build_resampling_map(plan, authorize = TRUE)
})

surface_space <- ngeo_coordinate_space(
  "fsaverage6-VAL302", kind = "surface", structure = "CORTEX_LEFT"
)
surface_source <- ngeo_surface(
  sphere$vertices, surface_faces,
  values = cbind(signal = numeric(nrow(surface_xyz))),
  measures = ngeo_measure(support_behavior = "intensive"),
  coordinate_space = surface_space
)
surface_target <- ngeo_surface(
  sphere$vertices, surface_faces, coordinate_space = surface_space
)
surface_plan <- ngeo_resampling_plan(
  surface_source, surface_target, identity_path(surface_space), method = "nearest"
)
surface_nearest_map <- ngeo_build_resampling_map(surface_plan, authorize = TRUE)

small_xyz <- rbind(
  c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0),
  c(0, -1, 0), c(0, 0, 1), c(0, 0, -1)
)
small_faces <- rbind(
  c(1, 3, 5), c(3, 2, 5), c(2, 4, 5), c(4, 1, 5),
  c(3, 1, 6), c(2, 3, 6), c(4, 2, 6), c(1, 4, 6)
)
small_space <- ngeo_coordinate_space(
  "VAL302-conservative-surface", kind = "surface", structure = "CORTEX_LEFT"
)
small_source <- ngeo_surface(
  small_xyz, small_faces, values = cbind(signal = numeric(6L)),
  measures = ngeo_measure(support_behavior = "intensive"),
  coordinate_space = small_space
)
small_target <- ngeo_surface(small_xyz, small_faces, coordinate_space = small_space)
small_plan <- ngeo_resampling_plan(
  small_source, small_target, identity_path(small_space), method = "barycentric"
)
small_barycentric_map <- ngeo_build_resampling_map(small_plan, authorize = TRUE)

receipt_ids <- function(tool, geometry, operation, phantom, coverage) {
  rows <- receipts$tool == tool & receipts$geometry == geometry &
    receipts$operation == operation & receipts$phantom == phantom &
    receipts$coverage == coverage
  receipts$receipt_id[rows]
}
receipt_output <- function(id) {
  row <- receipts[match(id, receipts$receipt_id), , drop = FALSE]
  stem <- safe_id(row$geometry, row$operation, row$phantom, row$coverage)
  if (row$tool == "connectome_workbench") {
    extension <- if (row$geometry == "volume") ".nii.gz" else ".func.gii"
    file.path(local_outputs, paste0("wb-", stem, extension))
  } else {
    extension <- if (row$geometry == "volume") ".nii.gz" else ".mgh"
    file.path(remote_outputs, paste0("fs-", stem, extension))
  }
}
read_external <- function(path, geometry) {
  if (geometry == "volume") as.numeric(RNifti::readNifti(path)) else
    if (grepl("[.]mgh$", path)) as.numeric(freesurferformats::read.fs.mgh(path)) else
      as.numeric(freesurferformats::read.fs.morph.gii(path))
}

cell_rows <- vector("list", nrow(grid))
for (index in seq_len(nrow(grid))) {
  cell <- grid[index, , drop = FALSE]
  geometry <- cell$geometry
  operation <- cell$operation
  phantom <- cell$phantom
  coverage <- cell$coverage
  supported <- !identical(geometry, "cifti") && !(
    identical(geometry, "surface") &&
      identical(operation, "gather-linear-or-barycentric")
  )
  unsupported_reason <- if (identical(geometry, "cifti")) {
    "CIFTI-to-CIFTI resampling is rejected; component maps must be explicit."
  } else if (!supported) {
    paste(
      "Workbench target-gather barycentric interpolation is not the",
      "neurogeo conservative source-scatter barycentric operation."
    )
  } else ""
  ids <- character()
  package_values <- reference_values <- numeric()
  input_values <- numeric()
  coverage_error <- candidate_miss <- NA_real_
  if (supported && geometry == "volume") {
    fixture <- fixture_index[[paste("volume", phantom, coverage, sep = "|")]]
    input_values <- fixture$values
    method <- switch(
      operation,
      `gather-nearest` = "nearest",
      `gather-linear-or-barycentric` = "linear",
      `conservative-remap` = "overlap"
    )
    package_values <- as.numeric(volume_maps[[method]]$operator %*% input_values)
    package_mask <- as.numeric(volume_maps[[method]]$operator %*% as.numeric(fixture$mask))
    coverage_error <- max(abs(package_mask - as.numeric(fixture$mask)))
    candidate_miss <- 0
    if (!identical(operation, "conservative-remap")) {
      ids <- c(
        receipt_ids("connectome_workbench", geometry, operation, phantom, coverage),
        receipt_ids("freesurfer_volume", geometry, operation, phantom, coverage)
      )
      references <- lapply(ids, function(id) read_external(receipt_output(id), geometry))
      reference_values <- Reduce(`+`, references) / length(references)
      mask_ids <- c(
        receipt_ids("connectome_workbench", geometry, operation, "coverage-mask", coverage),
        receipt_ids("freesurfer_volume", geometry, operation, "coverage-mask", coverage)
      )
      external_masks <- lapply(mask_ids, function(id) read_external(receipt_output(id), geometry))
      coverage_error <- max(
        coverage_error,
        vapply(external_masks, function(x) max(abs(x - as.numeric(fixture$mask))), numeric(1))
      )
    } else reference_values <- input_values
  } else if (supported && geometry == "surface" &&
             identical(operation, "gather-nearest")) {
    fixture <- fixture_index[[paste("surface", phantom, coverage, sep = "|")]]
    input_values <- fixture$values
    package_values <- as.numeric(surface_nearest_map$operator %*% input_values)
    ids <- receipt_ids("freesurfer_surface", geometry, operation, phantom, coverage)
    reference_values <- read_external(receipt_output(ids), geometry)
    package_mask <- as.numeric(surface_nearest_map$operator %*% as.numeric(fixture$mask))
    mask_id <- receipt_ids(
      "freesurfer_surface", geometry, operation, "coverage-mask", coverage
    )
    external_mask <- read_external(receipt_output(mask_id), geometry)
    coverage_error <- max(
      abs(package_mask - as.numeric(fixture$mask)),
      abs(external_mask - as.numeric(fixture$mask))
    )
    candidate_miss <- mean(Matrix::colSums(surface_nearest_map$operator) == 0)
  } else if (supported && geometry == "surface") {
    eligible <- coverage_mask(small_xyz, coverage)
    input_values <- phantom_values(small_xyz, phantom, eligible)
    package_values <- as.numeric(small_barycentric_map$operator %*% input_values)
    reference_values <- input_values
    package_mask <- as.numeric(small_barycentric_map$operator %*% as.numeric(eligible))
    coverage_error <- max(abs(package_mask - as.numeric(eligible)))
    candidate_miss <- mean(Matrix::colSums(small_barycentric_map$operator) == 0)
  } else if (!supported && geometry == "surface") {
    ids <- receipt_ids(
      "connectome_workbench", geometry, operation, phantom, coverage
    )
  }
  observed_metric <- if (supported) {
    metric(
      package_values, reference_values,
      mass = identical(operation, "conservative-remap") &&
        identical(phantom, "compact-mass")
    )
  } else list(rmse = NA_real_, maximum_error = NA_real_, relative_mass_error = NA_real_)
  gates <- c(
    if (supported && identical(phantom, "constant"))
      observed_metric$maximum_error <= 1e-10 else TRUE,
    if (supported && identical(phantom, "linear"))
      observed_metric$rmse <= 1e-6 else TRUE,
    if (supported && identical(operation, "conservative-remap") &&
        identical(phantom, "compact-mass"))
      observed_metric$relative_mass_error <= 1e-8 else TRUE,
    if (supported) coverage_error <= 1e-10 else TRUE,
    if (supported) candidate_miss <= 1e-4 else TRUE
  )
  cell_rows[[index]] <- data.frame(
    validation_id = "VAL-302",
    geometry = geometry, operation = operation,
    phantom = phantom, coverage = coverage,
    attempted = TRUE, executed = supported,
    cell_status = if (supported && all(gates)) "passed" else if (supported) "failed" else
      "unsupported-by-declared-api",
    unsupported_reason = unsupported_reason,
    reference_tools = paste(unique(receipts$tool[receipts$receipt_id %in% ids]), collapse = ";"),
    receipt_ids = paste(ids, collapse = ";"),
    input_sha256 = if (length(input_values)) hash_values(input_values) else "",
    package_output_sha256 = if (length(package_values)) hash_values(package_values) else "",
    reference_output_sha256 = if (length(reference_values)) hash_values(reference_values) else "",
    rmse = observed_metric$rmse,
    maximum_error = observed_metric$maximum_error,
    relative_mass_error = observed_metric$relative_mass_error,
    declared_coverage_error = coverage_error,
    candidate_miss_rate = candidate_miss,
    gate_pass = if (supported) all(gates) else NA,
    stringsAsFactors = FALSE
  )
}
cells <- do.call(rbind, cell_rows)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cells_path <- file.path(output_dir, "val302-external-reference-cells-6.0.csv")
receipts_path <- file.path(output_dir, "val302-external-command-receipts-6.0.csv")
report_path <- file.path(output_dir, "val302-external-reference-evidence-6.0.json")
utils::write.csv(cells, cells_path, row.names = FALSE, na = "")
utils::write.csv(receipts, receipts_path, row.names = FALSE, na = "")
supported <- cells$executed
checks <- list(
  frozen_grid_complete = nrow(cells) == 108L && !anyDuplicated(cells[c(
    "geometry", "operation", "phantom", "coverage"
  )]),
  every_cell_attempted = all(cells$attempted),
  declared_unsupported_retained = sum(!cells$executed) == 48L,
  supported_cells_pass = all(cells$gate_pass[supported]),
  no_supported_failures = !any(cells$cell_status[supported] == "failed"),
  command_receipts_complete = nrow(receipts) == 90L &&
    all(receipts$exit_status == 0L) &&
    all(nzchar(receipts$input_sha256)) && all(nzchar(receipts$output_sha256)),
  workbench_identity_verified = any(receipts$tool == "connectome_workbench"),
  freesurfer_surface_identity_verified = any(receipts$tool == "freesurfer_surface"),
  freesurfer_volume_identity_verified = any(receipts$tool == "freesurfer_volume")
)
pass <- all(unlist(checks, use.names = FALSE))
report <- list(
  schema = "neurogeo/val302-external-reference/1",
  validation_id = "VAL-302",
  status = if (pass) "passed-with-declared-unsupported-cells" else "failed",
  validation_evidence = pass,
  package_version = package_version,
  source_commit = source_commit,
  design_sha256 = design_hash,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  external_environment = list(
    freesurfer_host = host,
    freesurfer_remote_work = remote_root,
    tools = prerequisites$tools,
    source_surface = list(
      remote_path = remote_sphere,
      sha256 = sha_file(sphere_path),
      vertices = nrow(surface_xyz), faces = nrow(surface_faces)
    )
  ),
  summary = list(
    attempted_cells = nrow(cells), executed_cells = sum(cells$executed),
    unsupported_cells = sum(!cells$executed), failed_cells = sum(cells$cell_status == "failed"),
    command_receipts = nrow(receipts)
  ),
  checks = checks,
  artifacts = list(
    hash_convention = "sha256-after-crlf-to-lf-normalization",
    cells = list(
      path = basename(cells_path), sha256 = sha_canonical_text(cells_path)
    ),
    receipts = list(
      path = basename(receipts_path), sha256 = sha_canonical_text(receipts_path)
    )
  ),
  evidence_boundary = paste(
    "FreeSurfer and Connectome Workbench are external validation comparators only",
    "and are not package runtime dependencies. CIFTI-to-CIFTI and Workbench-style",
    "surface target-gather cells remain explicitly unsupported; neurogeo surface",
    "barycentric evidence applies only to its conservative source-scatter contract."
  )
)
jsonlite::write_json(report, report_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(normalizePath(report_path, winslash = "/", mustWork = TRUE), "\n")
if (!pass) quit(status = 2L)
