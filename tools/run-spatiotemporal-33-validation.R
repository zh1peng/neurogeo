args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "spatiotemporal-33-validation.json")
required <- c("digest", "jsonlite", "Matrix")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Spatiotemporal 3.3 validation requires: ",
       paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
rejected_as <- function(expression, class) {
  inherits(
    tryCatch({
      force(expression)
      NULL
    }, error = identity),
    class
  )
}

started <- Sys.time()

coordinates <- matrix(
  c(0, 0, 1, 0), ncol = 2L, byrow = TRUE
)
values <- matrix(
  c(1, 2, 2, 4, 4, 8),
  nrow = 2L
)
axis <- ngeo_time_axis(time = c(0, 1, 3), unit = "second")
x <- ngeo_point(coordinates, values = values)
x <- ngeo_set_time_axis(x, axis, "instantaneous")
base_hash <- base_hash(x)
slice <- ngeo_time_slice(x, index = c(1L, 3L))
axis_gate <- identical(axis$time, c(0, 1, 3)) &&
  !axis$regular &&
  identical(base_hash(slice), base_hash) &&
  identical(ngeo_get_time_axis(slice)$time, c(0, 3)) &&
  identical(dim(slice$values), c(2L, 2L))
assert(axis_gate, "Explicit time-axis or slicing gate failed.")

mutated_axis <- axis
mutated_axis$time[[2L]] <- 2
mutation_rejected <- rejected_as(
  ngeo_validate_time_axis(mutated_axis),
  "ngeo_error_time_axis_mutation"
)
reorder_rejected <- rejected_as(
  ngeo_time_slice(x, index = c(2L, 1L)),
  "ngeo_error_index"
)
inference_rejected <- rejected_as(
  ngeo_set_time_axis(
    x, ngeo_time_axis(time = 1:2), "instantaneous"
  ),
  "ngeo_error_alignment"
)
assert(
  mutation_rejected && reorder_rejected && inference_rejected,
  "Time-axis adversarial gate failed."
)

temporal <- ngeo_temporal_weights(axis, style = "B")
spatial <- ngeo_spatial_weights(
  x, method = "distance_band", threshold = 1.1, style = "B"
)
space_time <- ngeo_spatiotemporal_weights(
  spatial, temporal, combination = "sum", spatial_scale = 0.5
)
lag <- ngeo_spatiotemporal_lag(x, space_time)
expected_lag <- matrix(
  c(2, 2.5, 4.5, 6, 5, 4),
  nrow = 2L
)
lag_reference <- isTRUE(all.equal(
  unname(lag), expected_lag, tolerance = 1e-12
))
reference_matrix <- ngeo_materialize_spatiotemporal_weights(
  space_time
)
kronecker_reference <- isTRUE(all.equal(
  as.numeric(lag),
  as.numeric(reference_matrix %*% as.numeric(x$values)),
  tolerance = 1e-12
))
assert(
  lag_reference && kronecker_reference &&
    !space_time$matrix_materialized &&
    !"matrix" %in% names(space_time),
  "Matrix-free space-time lag reference failed."
)

moran <- ngeo_spatiotemporal_moran(
  x, space_time, permutations = 49L, seed = 3301
)
moran_again <- ngeo_spatiotemporal_moran(
  x, space_time, permutations = 49L, seed = 3301
)
flat <- as.numeric(x$values)
centered <- flat - mean(flat)
s0 <- sum(reference_matrix)
expected_moran <- length(flat) / s0 *
  sum(centered * as.numeric(reference_matrix %*% centered)) /
  sum(centered^2)
moran_reference <- isTRUE(all.equal(
  moran$estimate, expected_moran, tolerance = 1e-12
)) && identical(moran$simulated, moran_again$simulated)
assert(moran_reference, "Spatiotemporal Moran reference failed.")

temporal_variogram <- ngeo_temporal_variogram(
  x, breaks = 2L, max_pairs = 6L
)
joint_variogram <- ngeo_spatiotemporal_variogram(
  x,
  spatial_distance = matrix(c(0, 1, 1, 0), nrow = 2L),
  spatial_breaks = 2L,
  temporal_breaks = 2L,
  max_pairs = 15L
)
pair_accounting <- identical(
  attr(temporal_variogram, "pair_count"), 6
) && sum(temporal_variogram$n_pairs) == 6 &&
  identical(attr(joint_variogram, "pair_count"), 15) &&
  sum(joint_variogram$n_pairs) == 15
pair_budget_rejected <- rejected_as(
  ngeo_spatiotemporal_variogram(x, max_pairs = 14L),
  "ngeo_error_resource"
)
assert(
  pair_accounting && pair_budget_rejected,
  "Variogram pair-accounting gate failed."
)

interval_axis <- ngeo_time_axis(
  time = c(0.5, 2),
  interval_start = c(0, 1),
  interval_end = c(1, 3),
  unit = "hour"
)
interval_base <- ngeo_point(
  coordinates,
  values = matrix(c(2, 4, 6, 8), nrow = 2L)
)
rate <- ngeo_set_time_axis(
  interval_base, interval_axis, "rate"
)
total <- ngeo_set_time_axis(
  interval_base, interval_axis, "interval_total"
)
integral <- ngeo_temporal_contrast(rate, operation = "integral")
summed <- ngeo_temporal_contrast(total, operation = "sum")
semantic_reference <- identical(
  as.numeric(integral$values), c(14, 20)
) && identical(as.numeric(summed$values), c(8, 12))
invalid_mean_rejected <- rejected_as(
  ngeo_temporal_contrast(total, operation = "mean"),
  "ngeo_error_temporal_support"
)
assert(
  semantic_reference && invalid_mean_rejected,
  "Temporal support semantics gate failed."
)

trend_time <- c(0, 1, 3, 6)
trend_values <- rbind(
  2 + 3 * trend_time,
  5 - 2 * trend_time
)
trend_data <- ngeo_point(coordinates, values = trend_values)
trend_data <- ngeo_set_time_axis(
  trend_data, ngeo_time_axis(time = trend_time), "instantaneous"
)
trend <- ngeo_temporal_trend(trend_data)
change <- ngeo_longitudinal_change(
  trend_data, from = 1L, to = 4L, scale = "rate"
)
longitudinal_reference <- isTRUE(all.equal(
  as.numeric(trend$values[, "intercept"]), c(2, 5),
  tolerance = 1e-12
)) && isTRUE(all.equal(
  as.numeric(trend$values[, "slope"]), c(3, -2),
  tolerance = 1e-12
)) && identical(as.numeric(change$values), c(3, -2)) &&
  identical(base_hash(trend), base_hash(trend_data))
assert(longitudinal_reference, "Longitudinal helper reference failed.")

large_n_space <- 10000L
large_n_time <- 100L
large_coordinates <- cbind(
  x = seq_len(large_n_space),
  y = rep.int(0, large_n_space)
)
large_values <- outer(
  seq_len(large_n_space),
  seq_len(large_n_time),
  function(i, j) sin(i / 101) + cos(j / 11)
)
large <- ngeo_point(
  large_coordinates, values = large_values
)
large_axis <- ngeo_time_axis(
  start = 0, step = 0.72, n = large_n_time, unit = "second"
)
large <- ngeo_set_time_axis(
  large, large_axis, "instantaneous"
)
large_spatial <- ngeo_spatial_weights(
  large, method = "knn", k = 2L,
  style = "W", symmetry = "union"
)
large_temporal <- ngeo_temporal_weights(
  large_axis, style = "W"
)
large_weights <- ngeo_spatiotemporal_weights(
  large_spatial, large_temporal
)
large_timing <- system.time({
  large_lag <- ngeo_spatiotemporal_lag(
    large,
    large_weights,
    budget = ngeo_resource_budget(
      memory_bytes = 64 * 1024^2,
      materialized_elements = 2e6
    )
  )
})
large_gate <- identical(
  dim(large_lag), c(large_n_space, large_n_time)
) && !large_weights$matrix_materialized &&
  !"matrix" %in% names(large_weights) &&
  nrow(large$base$elements) == large_n_space &&
  nrow(large$layers) == large_n_time &&
  length(large_spatial$matrix@x) < 10 * large_n_space &&
  unname(large_timing[["elapsed"]]) < 30 &&
  rejected_as(
    ngeo_materialize_spatiotemporal_weights(
      large_weights, max_observations = 10000L
    ),
    "ngeo_error_resource"
  )
assert(large_gate, "Large matrix-free spatiotemporal gate failed.")

corpus <- neurogeo:::.ngeo_conformance_manifest(version = "3.3")
manifests <- lapply(
  list(axis, temporal, space_time), ngeo_object_manifest
)
schemas <- vapply(
  manifests, `[[`, character(1), "object_schema"
)
schema_gate <- identical(corpus$corpus_version, "3.3") &&
  identical(
    schemas,
    c(
      "ngcs/time-axis",
      "ngcs/temporal-spatial_weights",
      "ngcs/spatiotemporal-spatial_weights"
    )
  ) &&
  all(vapply(
    manifests,
    function(manifest) identical(
      manifest$specification, "NGCS 3.3"
    ),
    logical(1)
  ))
assert(schema_gate, "NGCS 3.3 corpus or schema gate failed.")

report <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  started_at_utc = format(
    started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(packageVersion("neurogeo")),
  specification = "NGCS 3.3",
  validation = "passed",
  explicit_time_axis = list(
    regular_and_irregular = TRUE,
    instant_and_interval = TRUE,
    mutation_rejected = mutation_rejected,
    deterministic_slice = TRUE,
    domain_hash_preserved = TRUE,
    map_count_inference = FALSE
  ),
  separable_weights = list(
    matrix_free_lag_reference = lag_reference,
    kronecker_reference = kronecker_reference,
    stored_kronecker_matrix = FALSE
  ),
  statistics = list(
    moran_reference = moran_reference,
    deterministic_permutations = TRUE,
    exact_pair_accounting = pair_accounting,
    pair_budget_rejected = pair_budget_rejected
  ),
  temporal_semantics = list(
    interval_integral_and_total_sum = semantic_reference,
    invalid_interval_total_mean_rejected = invalid_mean_rejected,
    longitudinal_reference = longitudinal_reference
  ),
  large_gate = list(
    space_elements = large_n_space,
    time_coordinates = large_n_time,
    observations = large_n_space * large_n_time,
    spatial_nonzero = length(large_spatial$matrix@x),
    temporal_nonzero = length(large_temporal$matrix@x),
    stored_kronecker_matrix = FALSE,
    geometry_expanded_through_time = FALSE,
    lag_elapsed_seconds = unname(large_timing[["elapsed"]]),
    elapsed_limit_seconds = 30
  ),
  conformance_corpus = "3.3",
  schemas = schemas,
  manifest_sha256 = vapply(
    manifests, `[[`, character(1), "canonical_sha256"
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
