args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
full <- identical(tolower(Sys.getenv("NEUROGEO_50_FULL_PERF")), "true")
output <- if (length(args)) args[[1L]] else file.path(
  "check-output",
  if (full) "multilayer-50-performance.json" else
    "multilayer-50-performance-quick.json"
)
required <- c("jsonlite", "Matrix", "RSpectra")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("5.0 performance validation requires: ",
                          paste(missing, collapse = ", "))
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
rss <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) return(NA_real_)
  as.numeric(ps::ps_memory_info()[["rss"]])
}
profile_case <- function(expression) {
  before <- rss()
  timing <- system.time(value <- force(expression))
  list(
    value = value,
    elapsed_seconds = unname(timing[["elapsed"]]),
    maximum_observed_rss_bytes = max(c(before, rss()), na.rm = TRUE)
  )
}
grid_adjacency <- function(nrow, ncol) {
  id <- matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol)
  first_h <- as.vector(id[-nrow, , drop = FALSE])
  second_h <- as.vector(id[-1L, , drop = FALSE])
  first_v <- as.vector(id[, -ncol, drop = FALSE])
  second_v <- as.vector(id[, -1L, drop = FALSE])
  Matrix::sparseMatrix(
    i = c(first_h, second_h, first_v, second_v),
    j = c(second_h, first_h, second_v, first_v), x = 1,
    dims = rep(nrow * ncol, 2L)
  )
}
make_grid <- function(nrow, ncol) {
  n <- nrow * ncol
  x <- ngeo_parcellation(
    data.frame(region_id = seq_len(n)),
    support_size = rep.int(1, n),
    adjacency = grid_adjacency(nrow, ncol)
  )
  list(x = x, spatial_weights = ngeo_spatial_weights(
    x, method = "region_contiguity", style = "B"
  ))
}
run_basis_case <- function(label, nrow, ncol, modes) {
  fixture <- make_grid(nrow, ncol)
  measured <- profile_case(ngeo_spatial_basis(
    fixture$x, fixture$spatial_weights, n_modes = modes,
    budget = ngeo_resource_budget(
      memory_bytes = 5e9, materialized_elements = 2e8
    )
  ))
  basis <- measured$value
  list(
    id = label, elements = nrow * ncol, modes = modes,
    elapsed_seconds = measured$elapsed_seconds,
    maximum_observed_rss_bytes = measured$maximum_observed_rss_bytes,
    basis_bytes = as.numeric(object.size(basis)),
    maximum_residual = basis$diagnostics$max_residual,
    maximum_orthogonality_error =
      basis$diagnostics$max_orthogonality_error,
    dense_element_pair_matrix = basis$diagnostics$dense_full_base_matrix,
    pass = !basis$diagnostics$dense_full_base_matrix &&
      basis$diagnostics$max_residual < 1e-6 &&
      basis$diagnostics$max_orthogonality_error < 1e-6
  )
}

make_file_stack <- function(subjects, layers) {
  elements <- 128L
  maps_n <- subjects * layers
  map_names <- sprintf("map_%06d", seq_len(maps_n))
  path <- tempfile(fileext = ".bin")
  connection <- file(path, "wb")
  on.exit(close(connection))
  for (column in seq_len(maps_n)) {
    writeBin(
      as.numeric(sin(seq_len(elements) / 13 + column / 17)),
      connection, size = 4L, endian = "little"
    )
  }
  close(connection)
  on.exit(NULL)
  values <- ngeo_file_values(
    path, c(elements, maps_n), map_names, "raw-performance",
    selection = list(
      layout = "volume", element_index = 0:(elements - 1L),
      layer_index = 0:(maps_n - 1L), full_element_count = elements
    ),
    binary = list(
      what = "numeric", bytes = 4L, signed = TRUE, endian = "little",
      data_offset = 0, compressed = FALSE, slope = 0, intercept = 0
    ),
    verify = "metadata",
    budget = ngeo_resource_budget(
      memory_bytes = 128 * 1024^2, materialized_elements = 2e6
    )
  )
  map_table <- data.frame(
    layer_id = map_names, name = map_names,
    subject_id = rep(sprintf("s%04d", seq_len(subjects)), each = layers),
    feature = rep(sprintf("layer_%02d", seq_len(layers)), subjects),
    stringsAsFactors = FALSE
  )
  measure <- ngeo_measure(support_behavior = "intensive", unit = "a.u.")
  measures <- measure[rep.int(1L, maps_n), , drop = FALSE]
  x <- ngeo_parcellation(
    data.frame(region_id = seq_len(elements)), values = values,
    support_size = rep.int(1, elements),
    adjacency = grid_adjacency(16L, 8L), layers = map_table,
    measures = measures
  )
  list(x = x, path = path)
}
run_layer_case <- function(subjects, layers) {
  fixture <- make_file_stack(subjects, layers)
  on.exit(unlink(fixture$path), add = TRUE)
  measured <- profile_case({
    index <- ngeo_validate_layers(
      fixture$x, required_layers = sprintf("layer_%02d", seq_len(layers)),
      complete = "error"
    )
    spatial_weights <- ngeo_spatial_weights(
      fixture$x, method = "region_contiguity", style = "B"
    )
    # The 16 x 8 grid has a two-dimensional eigenspace at this boundary.
    # Retain the complete cluster instead of weakening the degeneracy guard.
    basis <- ngeo_spatial_basis(fixture$x, spatial_weights, n_modes = 9L)
    features <- ngeo_basis_project(
      fixture$x, basis, index = index,
      bands = list(retained = 1:9),
      summaries = c("absolute_energy", "roughness"),
      chunk_rows = 64L, chunk_layers = 10L
    )
    list(index = index, features = features)
  })
  list(
    subjects = subjects, layers = layers,
    source = "verified file-backed float32 values",
    source_bytes = as.numeric(file.info(fixture$path)$size),
    elapsed_seconds = measured$elapsed_seconds,
    maximum_observed_rss_bytes = measured$maximum_observed_rss_bytes,
    feature_rows = nrow(measured$value$features$values),
    feature_endpoints = ncol(measured$value$features$values),
    input_materialized = FALSE,
    pass = nrow(measured$value$features$values) == subjects &&
      ncol(measured$value$features$values) == layers * 2L &&
      all(is.finite(measured$value$features$values)) &&
      inherits(fixture$x$values, "ngeo_file_values")
  )
}

make_features <- function(values, ids, support) {
  colnames(values) <- sprintf("endpoint_%03d", seq_len(ncol(values)))
  endpoints <- data.frame(
    endpoint_id = colnames(values), family = "full-corpus",
    estimand = "band_energy_x", layer_x = "x", layer_y = NA_character_,
    direction = "none", component = "cortex", band = "retained",
    scale_type = "rank_matched", bounds = "unbounded",
    recommended_transform = "none", support_hash = support,
    stringsAsFactors = FALSE
  )
  structure(list(
    values = values,
    units = data.frame(unit_id = ids, stringsAsFactors = FALSE),
    endpoints = endpoints, diagnostics = list(),
    history = list(support_hash = support)
  ), class = "ngeo_subject_features")
}
run_group_case <- function(subjects, endpoints, permutations, supports = 1L) {
  ids <- sprintf("s%04d", seq_len(subjects))
  group <- factor(rep(c("control", "case"), length.out = subjects))
  age <- seq(-1, 1, length.out = subjects)
  common <- matrix(rnorm(subjects * endpoints), subjects, endpoints)
  features <- stats::setNames(lapply(seq_len(supports), function(i) {
    values <- sqrt(0.85) * common + sqrt(0.15) *
      matrix(rnorm(subjects * endpoints), subjects, endpoints)
    make_features(values, ids, paste0("support-", i))
  }), paste0("support_", seq_len(supports)))
  if (supports == 1L) features <- features[[1L]]
  design <- data.frame(unit_id = ids, age = age, group = group)
  schedule <- ngeo_exchangeability(
    ids, permutations = permutations, seed = 5002L
  )
  measured <- profile_case(ngeo_group_test(
    features, design, ~ age + group, "group", schedule,
    transform = "none", adjustment = "maxT", retain_null = FALSE
  ))
  result <- measured$value
  expected_endpoints <- endpoints * supports
  list(
    subjects = subjects, supports = supports,
    endpoints_per_support = endpoints,
    family_endpoints = nrow(result$tests), permutations = permutations,
    elapsed_seconds = measured$elapsed_seconds,
    maximum_observed_rss_bytes = measured$maximum_observed_rss_bytes,
    result_bytes = as.numeric(object.size(result)),
    endpoint_null_retained = "endpoint" %in% names(result$null),
    schedule_hash = schedule$schedule_hash,
    pass = nrow(result$tests) == expected_endpoints &&
      !"endpoint" %in% names(result$null) &&
      all(is.finite(result$tests$p_raw)) &&
      all(is.finite(result$tests$p_maxT))
  )
}

set.seed(5000L)
checks <- list(
  file_layers_100_by_5 = run_layer_case(100L, 5L),
  file_layers_1000_by_20 = run_layer_case(1000L, 20L),
  group_1000_by_50_by_999 = run_group_case(1000L, 50L, 999L),
  support_family_100_by_5_by_100_by_4999 =
    run_group_case(100L, 100L, 4999L, 5L)
)
if (full) {
  checks$basis_32k_by_64 <- run_basis_case("32k-by-64", 180L, 180L, 64L)
  checks$basis_91k_by_64 <- run_basis_case("91k-by-64", 300L, 304L, 64L)
  checks$basis_91k_by_128 <- run_basis_case("91k-by-128", 300L, 304L, 128L)
}
pass <- all(vapply(checks, `[[`, logical(1), "pass"))
report <- list(
  schema = "neurogeo/multilayer-50-performance",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  full_basis_matrix = full,
  platform = R.version$platform, r_version = R.version.string,
  machine = list(
    sysname = Sys.info()[["sysname"]], release = Sys.info()[["release"]],
    machine = Sys.info()[["machine"]], logical_cores = parallel::detectCores()
  ),
  memory_measurement = paste(
    "maximum RSS observed before and after each case when package ps is",
    "available; bounded allocation and object sizes are also reported"
  ),
  dependencies = lapply(c("neurogeo", required), function(package) list(
    package = package,
    version = as.character(utils::packageVersion(package))
  )),
  checks = checks, pass = pass
)
assert(pass, "The 5.0 multilayer performance corpus failed.")
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE,
                     digits = 16, null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
