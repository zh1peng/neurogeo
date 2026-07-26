args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "execution-26-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Bounded execution validation requires jsonlite.")
}
if (!exists("ngeo_resource_budget", mode = "function")) {
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
    source = "NGCS 2.6 deterministic callback"
  )
  source <- ngeo_points(
    cbind(x = seq_len(n), y = 0),
    values = delayed,
    measures = ngeo_measure(spatial_semantics = "intensive"),
    space = ngeo_space("million-element-execution-26")
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
  budget <- ngeo_resource_budget(
    memory_bytes = 64 * 1024^2,
    elapsed_seconds = 180,
    blocks = 16,
    materialized_elements = n_target
  )
  changed <- ngeo_change_support_block(
    source, target, block, budget = budget
  )
  diagnostics <- ngeo_block_diagnostics(block, budget = budget)
  streamed <- ngeo_stream_summary(source, chunk_size = 100000L)
})[["elapsed"]]

expected <- vapply(seq_len(n_target), function(i) {
  mean(seq.int(i, n, by = n_target))
}, numeric(1))
maximum_error <- max(abs(changed$values[, 1L] - expected))
summary_error <- max(
  abs(streamed$mean[[1L]] - (n + 1) / 2),
  abs(streamed$variance[[1L]] - stats::var(seq_len(n)))
)
if (maximum_error > 1e-10 || summary_error > 1e-6 ||
    elapsed > 180 || diagnostics$materialized_operator ||
    diagnostics$nonzero != n) {
  stop("One-million-source bounded execution gate failed.")
}

budget_rejected <- inherits(tryCatch(
  {
    ngeo_change_support_block(
      source,
      target,
      block,
      budget = ngeo_resource_budget(blocks = 15)
    )
    NULL
  },
  error = identity
), "ngeo_error_resource")
if (!budget_rejected) stop("Low resource budget was not rejected.")

checkpoint <- tempfile(fileext = ".json")
tasks <- as.list(seq_len(5L))
plan <- ngeo_execution_plan(
  "validation-square",
  tasks,
  function(task, index) task^2,
  identity = list(
    domain_hash = ngeo_domain_hash(source),
    operator_hash = block$logical_hash,
    semantics = "intensive"
  ),
  checkpoint = checkpoint
)
partial <- ngeo_execute(plan, stop_after = 2L)
resumed <- ngeo_execute(plan)
if (partial$complete || !resumed$complete ||
    !identical(as.numeric(unlist(resumed$results)), (1:5)^2)) {
  stop("Checkpoint resume validation failed.")
}

cache <- ngeo_cache(tempfile("ngeo-validation-cache-"))
calls <- 0L
first <- ngeo_cache_compute(
  cache,
  list(domain = ngeo_domain_hash(source), semantics = "intensive"),
  function() {
    calls <<- calls + 1L
    26L
  }
)
second <- ngeo_cache_compute(
  cache,
  list(domain = ngeo_domain_hash(source), semantics = "intensive"),
  function() {
    calls <<- calls + 1L
    99L
  }
)
if (first$hit || !second$hit || calls != 1L ||
    !identical(second$value, 26L)) {
  stop("Content cache validation failed.")
}

atomic_path <- tempfile(fileext = ".txt")
atomic <- ngeo_atomic_write(
  atomic_path,
  function(path) writeLines("complete", path)
)
failed_path <- tempfile(fileext = ".txt")
atomic_failed_cleanly <- inherits(tryCatch(
  {
    ngeo_atomic_write(
      failed_path,
      function(path) stop("simulated interruption")
    )
    NULL
  },
  error = identity
), "error") && !file.exists(failed_path)
if (!file.exists(atomic$path) || !atomic_failed_cleanly) {
  stop("Atomic output validation failed.")
}

set.seed(2601)
random_errors <- numeric(20L)
for (iteration in seq_along(random_errors)) {
  source_n <- sample(20:80, 1L)
  target_n <- sample(3:10, 1L)
  value <- stats::rnorm(source_n)
  current_source <- ngeo_points(
    cbind(x = seq_len(source_n), y = 0),
    values = cbind(signal = value),
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  current_target <- ngeo_regions(
    data.frame(region_id = paste0("r", seq_len(target_n))),
    support_size = rep(NA_real_, target_n),
    space = current_source$domain$space
  )
  current_map <- ngeo_support_map(
    current_source,
    current_target,
    Matrix::sparseMatrix(
      i = sample(seq_len(target_n), source_n, replace = TRUE),
      j = seq_len(source_n),
      x = 1,
      dims = c(target_n, source_n)
    ),
    source_support = stats::runif(source_n, 0.5, 2)
  )
  current_block <- ngeo_block_support_map(
    current_map,
    row_block_size = sample(1:4, 1L),
    source_block_size = sample(3:12, 1L)
  )
  direct <- ngeo_change_support(
    current_source, current_target, current_map
  )
  bounded <- ngeo_change_support_block(
    current_source, current_target, current_block
  )
  random_errors[[iteration]] <- max(
    abs(direct$values - bounded$values)
  )
}
if (max(random_errors) > 1e-12) {
  stop("Randomized block equivalence validation failed.")
}

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  validation = "passed",
  million_source = list(
    source_elements = n,
    target_elements = n_target,
    nonzero = diagnostics$nonzero,
    blocks = diagnostics$blocks,
    elapsed_seconds = elapsed,
    elapsed_limit_seconds = 180,
    maximum_change_error = maximum_error,
    maximum_stream_summary_error = summary_error,
    delayed_object_bytes = as.numeric(object.size(delayed)),
    block_object_bytes = as.numeric(object.size(block)),
    materialized_operator = diagnostics$materialized_operator
  ),
  deterministic_controls = list(
    low_budget_rejected = budget_rejected,
    resumed_tasks = resumed$completed,
    checkpoint_complete = resumed$complete,
    cache_hit_verified = second$hit,
    atomic_failure_clean = atomic_failed_cleanly,
    atomic_sha256 = atomic$sha256
  ),
  randomized_equivalence = list(
    iterations = length(random_errors),
    maximum_error = max(random_errors)
  ),
  runtime_policy = list(
    external_binaries_required = FALSE,
    one_aligned_values_block = TRUE,
    dense_whole_operator_fallback = FALSE
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
