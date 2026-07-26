args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "scalable-io-25-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Scalable I/O validation requires jsonlite.")
}
if (!exists("ngeo_delayed_values", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

n <- 1000000L
n_target <- 1000L
elapsed <- system.time({
  delayed <- ngeo_delayed_values(
    function(rows, columns) matrix(
      as.numeric(rows),
      nrow = length(rows),
      ncol = length(columns)
    ),
    dim = c(n, 1L),
    map_names = "signal",
    source = "deterministic validation callback"
  )
  source <- ngeo_points(
    cbind(x = seq_len(n), y = 0),
    values = delayed,
    measures = ngeo_measure(spatial_semantics = "intensive"),
    space = ngeo_space("million-element-validation")
  )
  target <- ngeo_regions(
    data.frame(region_id = paste0("region_", seq_len(n_target))),
    support_size = rep(NA_real_, n_target),
    space = source$domain$space
  )
  operator <- Matrix::sparseMatrix(
    i = ((seq_len(n) - 1L) %% n_target) + 1L,
    j = seq_len(n),
    x = 1,
    dims = c(n_target, n)
  )
  map <- ngeo_support_map(
    source,
    target,
    operator,
    source_support = rep.int(1, n)
  )
  block <- ngeo_block_support_map(
    map,
    row_block_size = 250L,
    source_block_size = 250000L
  )
  changed <- ngeo_change_support_block(source, target, block)
})[["elapsed"]]

expected <- vapply(seq_len(n_target), function(i) {
  mean(seq.int(i, n, by = n_target))
}, numeric(1))
maximum_error <- max(abs(changed$values[, 1L] - expected))
time_limit <- 180
if (maximum_error > 1e-10 || elapsed > time_limit ||
    !identical(ngeo_support_map_hash(
      ngeo_materialize_support_map(block)
    ), map |> ngeo_support_map_hash())) {
  stop("Million-element sparse/delayed validation failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  million_element = list(
    source_elements = n,
    target_elements = n_target,
    nonzero = length(map$operator@x),
    blocks = length(block$row_groups) * length(block$column_groups),
    elapsed_seconds = elapsed,
    elapsed_limit_seconds = time_limit,
    maximum_value_error = maximum_error,
    delayed_object_bytes = as.numeric(object.size(delayed)),
    sparse_operator_bytes = as.numeric(object.size(map$operator)),
    logical_hash_match = TRUE
  ),
  runtime_policy = list(
    external_binaries_required = FALSE,
    dense_whole_operator = FALSE,
    one_aligned_values_block = TRUE
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
