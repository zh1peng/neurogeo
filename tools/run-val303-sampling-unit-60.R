args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--smoke" %in% args) "smoke" else "full"
args <- args[args != "--smoke"]
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val303-sampling-unit-60.json")
required <- c("digest", "jsonlite", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-303 requires: ", paste(missing, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
phase3 <- jsonlite::fromJSON(design_path, simplifyVector = FALSE)
validation <- Filter(
  function(x) identical(x$id, "VAL-303"), phase3$validations
)[[1L]]
common <- phase3$common
replicates <- if (identical(run_mode, "full")) {
  common$replicates_per_calibration_cell
} else {
  500L
}
alpha <- common$alpha

wilson_interval <- function(successes, attempted, confidence = 0.95) {
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  estimate <- successes / attempted
  denominator <- 1 + z^2 / attempted
  center <- (estimate + z^2 / (2 * attempted)) / denominator
  half_width <- z * sqrt(
    estimate * (1 - estimate) / attempted + z^2 / (4 * attempted^2)
  ) / denominator
  list(
    estimate = estimate,
    lower = max(0, center - half_width),
    upper = min(1, center + half_width),
    successes = successes,
    attempted = attempted
  )
}

make_features <- function(values, ids) {
  values <- as.matrix(values)
  colnames(values) <- "endpoint"
  structure(list(
    values = values,
    unit = data.frame(unit_id = ids, stringsAsFactors = FALSE),
    endpoints = data.frame(
      endpoint_id = "endpoint", family = "simulation", estimand = "mean",
      layer_x = "x", layer_y = "y", direction = "none",
      component = "component_001", band = "retained",
      scale_type = "rank_matched", bounds = "unbounded",
      recommended_transform = "none", stringsAsFactors = FALSE
    ),
    diagnostics = list(), history = list(source = "VAL-303")
  ), class = "ngeo_subject_features")
}

cell_design <- function(name, id_prefix) {
  ids <- paste0(id_prefix, seq_len(6L))
  block <- factor(rep(c("block_a", "block_b"), each = 3L))
  group <- switch(
    name,
    balanced = c("control", "case", "control", "case", "control", "case"),
    unbalanced = c("control", "control", "case", "control", "case", "control"),
    `site-confounded` = c("control", "control", "case", "control", "case", "case")
  )
  data <- data.frame(
    unit_id = ids, block = block,
    group = factor(group, levels = c("control", "case")),
    stringsAsFactors = FALSE
  )
  site_confounded <- identical(name, "site-confounded")
  model <- if (site_confounded) ~ block + group else ~ group
  full <- stats::model.matrix(model, data)
  reduced <- if (site_confounded) {
    stats::model.matrix(~ block, data)
  } else {
    matrix(1, nrow(data), 1L, dimnames = list(NULL, "(Intercept)"))
  }
  list(
    ids = ids, block = block, data = data, model = model,
    full = full, reduced = reduced,
    coefficient = grep("^group", colnames(full))[[1L]],
    nuisance_mean = if (site_confounded) 0.8 * (block == "block_b") else 0
  )
}

make_exchangeability <- function(spec, schedule_name, unit_kind) {
  if (identical(schedule_name, "free")) {
    return(ngeo_exchangeability(
      spec$ids, scheme = "free", permutations = 99999L,
      seed = 303L, unit_kind = unit_kind
    ))
  }
  base <- ngeo_exchangeability(
    spec$ids, scheme = "within_block", blocks = spec$block,
    permutations = 99999L, seed = 303L, unit_kind = unit_kind
  )
  if (identical(schedule_name, "blocked")) return(base)
  ngeo_exchangeability(
    spec$ids, scheme = "user", blocks = spec$block,
    schedule = base$schedule, unit_kind = unit_kind
  )
}

fit_statistics <- function(values, full, coefficient) {
  full_qr <- qr(full)
  coefficients <- qr.coef(full_qr, values)
  residuals <- qr.resid(full_qr, values)
  residual_df <- nrow(full) - ncol(full)
  variance_factor <- chol2inv(qr.R(full_qr))[coefficient, coefficient]
  standard_error <- sqrt(
    colSums(residuals^2) / residual_df * variance_factor
  )
  list(
    coefficient = coefficients[coefficient, ],
    standard_error = standard_error,
    statistic = coefficients[coefficient, ] / standard_error,
    residual_df = residual_df
  )
}

exact_permutation_p <- function(values, spec, exchangeability) {
  reduced_qr <- qr(spec$reduced)
  fitted <- qr.fitted(reduced_qr, values)
  residuals <- qr.resid(reduced_qr, values)
  observed <- fit_statistics(values, spec$full, spec$coefficient)$statistic
  exceedance <- integer(ncol(values))
  for (row in seq_len(nrow(exchangeability$schedule))) {
    current <- fitted + residuals[exchangeability$schedule[row, ], , drop = FALSE]
    statistic <- fit_statistics(
      current, spec$full, spec$coefficient
    )$statistic
    exceedance <- exceedance + (abs(statistic) >= abs(observed))
  }
  (1 + exceedance) / (nrow(exchangeability$schedule) + 1)
}

condition_is <- function(expression, class) {
  condition <- tryCatch({
    force(expression)
    NULL
  }, error = identity)
  inherits(condition, class)
}

check_wrong_units <- function(spec, exchangeability, unit_kind) {
  values <- make_features(matrix(stats::rnorm(6L), ncol = 1L), spec$ids)
  reversed <- ngeo_exchangeability(
    rev(spec$ids), permutations = 5L, seed = 303L, unit_kind = unit_kind
  )
  duplicate_rejected <- condition_is(
    ngeo_exchangeability(c("duplicate", "duplicate"), unit_kind = unit_kind),
    "ngeo_error_independent_unit"
  )
  alignment_rejected <- condition_is(
    ngeo_group_test(
      values, spec$data, spec$model, "group", reversed,
      transform = "none", adjustment = "none"
    ),
    "ngeo_error_alignment"
  )
  crossing <- exchangeability$schedule[1L, , drop = FALSE]
  crossing[1L, ] <- c(4L, 2L, 3L, 1L, 5L, 6L)
  colnames(crossing) <- spec$ids
  crossing_rejected <- condition_is(
    ngeo_exchangeability(
      spec$ids, scheme = "user", blocks = spec$block,
      schedule = crossing, unit_kind = unit_kind
    ),
    "ngeo_error_exchangeability"
  )
  checks <- c(duplicate_rejected, alignment_rejected, crossing_rejected)
  list(
    duplicate_unit_id = duplicate_rejected,
    group_alignment = alignment_rejected,
    cross_block_schedule = crossing_rejected,
    rejected = sum(checks), attempted = length(checks),
    estimate = mean(checks), pass = all(checks)
  )
}

run_supported_cell <- function(unit_kind, design_name, schedule_name,
                               design_index, schedule_index) {
  prefix <- switch(
    unit_kind, subject = "subject_", site = "site_",
    spatial_block = "spatial_block_"
  )
  spec <- cell_design(design_name, prefix)
  exchangeability <- make_exchangeability(spec, schedule_name, unit_kind)
  signal_seed <- validation$seed_base + design_index
  null_seed <- validation$seed_base + 100L +
    10L * design_index + schedule_index
  set.seed(signal_seed)
  signal <- spec$nuisance_mean + 0.5 * spec$full[, spec$coefficient] +
    matrix(stats::rnorm(6L * replicates), nrow = 6L)
  signal_fit <- fit_statistics(signal, spec$full, spec$coefficient)
  critical <- stats::qt(1 - alpha / 2, signal_fit$residual_df)
  covered <- abs(signal_fit$coefficient - 0.5) <=
    critical * signal_fit$standard_error
  coverage <- wilson_interval(sum(covered), replicates)

  set.seed(null_seed)
  null <- spec$nuisance_mean + matrix(
    stats::rnorm(6L * replicates), nrow = 6L
  )
  p_value <- exact_permutation_p(null, spec, exchangeability)
  type1 <- wilson_interval(sum(p_value <= alpha), replicates)

  public_null <- suppressWarnings(ngeo_group_test(
    make_features(null[, 1L, drop = FALSE], spec$ids),
    spec$data, spec$model, "group", exchangeability,
    transform = "none", adjustment = "none", retain_null = TRUE
  ))
  manual_first <- p_value[[1L]]
  api_p_value_error <- abs(public_null$tests$p_raw[[1L]] - manual_first)
  api_statistic_error <- abs(
    public_null$tests$statistic[[1L]] -
      fit_statistics(null[, 1L, drop = FALSE], spec$full,
                     spec$coefficient)$statistic[[1L]]
  )
  reversed_schedule <- exchangeability$schedule[
    rev(seq_len(nrow(exchangeability$schedule))), , drop = FALSE
  ]
  reversed <- ngeo_exchangeability(
    spec$ids, scheme = "user",
    blocks = if (identical(schedule_name, "free")) NULL else spec$block,
    schedule = reversed_schedule, unit_kind = unit_kind
  )
  reordered <- suppressWarnings(ngeo_group_test(
    make_features(null[, 1L, drop = FALSE], spec$ids),
    spec$data, spec$model, "group", reversed,
    transform = "none", adjustment = "none", retain_null = TRUE
  ))
  order_difference <- max(abs(c(
    public_null$tests$statistic - reordered$tests$statistic,
    public_null$tests$p_raw - reordered$tests$p_raw,
    sort(public_null$null$endpoint[, 1L]) -
      sort(reordered$null$endpoint[, 1L])
  )))
  wrong <- check_wrong_units(spec, exchangeability, unit_kind)
  palm_matrix <- t(rbind(seq_len(6L), exchangeability$schedule))
  palm_compatible <- identical(dim(palm_matrix), c(6L, nrow(
    exchangeability$schedule
  ) + 1L)) && identical(unname(palm_matrix[, 1L]), seq_len(6L))
  coverage_pass <- coverage$lower >= 0.93 && coverage$upper <= 0.97
  type1_pass <- type1$upper <= 0.065
  implementation_pass <- api_p_value_error <= 1e-12 &&
    api_statistic_error <= 1e-10 && order_difference <= 1e-12 &&
    wrong$pass && palm_compatible &&
    identical(public_null$sampling_unit, unit_kind)
  list(
    declared_unit = gsub("_", "-", unit_kind),
    design = design_name, schedule = schedule_name,
    seed_signal = signal_seed, seed_null = null_seed,
    independent_units = 6L,
    transformations = nrow(exchangeability$schedule),
    exact_enumeration = identical(exchangeability$status, "exact") ||
      identical(exchangeability$scheme, "user"),
    underlying_restriction = if (identical(schedule_name, "free"))
      "unrestricted" else "within-block",
    replicates_attempted = replicates, failed_fits = 0L,
    failed_fit_rate = 0,
    coverage = coverage, type1 = type1,
    wrong_unit_rejection = wrong,
    schedule_order_difference = order_difference,
    api_p_value_error = api_p_value_error,
    api_statistic_error = api_statistic_error,
    palm_column_schedule_compatible = palm_compatible,
    primary_gate_applicable = TRUE,
    primary_gate_pass = if (identical(run_mode, "full"))
      coverage_pass && type1_pass else NULL,
    implementation_pass = implementation_pass,
    pass = implementation_pass && (!identical(run_mode, "full") ||
      (coverage_pass && type1_pass))
  )
}

run_restricted_cell <- function(unit_kind, design_name, schedule_name,
                                design_index, schedule_index) {
  prefix <- switch(
    unit_kind, subject = "subject_", site = "site_",
    spatial_block = "spatial_block_"
  )
  spec <- cell_design(design_name, prefix)
  unrestricted_schedule <- .ngeo_exact_permutations(length(spec$ids))
  colnames(unrestricted_schedule) <- spec$ids
  reference <- list(schedule = unrestricted_schedule)
  signal_seed <- validation$seed_base + design_index
  null_seed <- validation$seed_base + 100L +
    10L * design_index + schedule_index
  set.seed(signal_seed)
  signal <- spec$nuisance_mean + 0.5 * spec$full[, spec$coefficient] +
    matrix(stats::rnorm(6L * replicates), nrow = 6L)
  signal_fit <- fit_statistics(signal, spec$full, spec$coefficient)
  critical <- stats::qt(1 - alpha / 2, signal_fit$residual_df)
  covered <- abs(signal_fit$coefficient - 0.5) <=
    critical * signal_fit$standard_error
  coverage <- wilson_interval(sum(covered), replicates)
  set.seed(null_seed)
  null <- spec$nuisance_mean + matrix(
    stats::rnorm(6L * replicates), nrow = 6L
  )
  p_value <- exact_permutation_p(null, spec, reference)
  type1 <- wilson_interval(sum(p_value <= alpha), replicates)
  runtime_rejected <- condition_is(
    ngeo_exchangeability(
      spec$ids, scheme = "free", blocks = spec$block,
      permutations = 99999L, unit_kind = unit_kind
    ),
    "ngeo_error_exchangeability_design"
  )
  wrong <- check_wrong_units(spec, reference, unit_kind)
  palm_matrix <- t(rbind(seq_len(6L), unrestricted_schedule))
  palm_compatible <- identical(dim(palm_matrix), c(
    6L, nrow(unrestricted_schedule) + 1L
  )) && identical(unname(palm_matrix[, 1L]), seq_len(6L))
  original_gate_pass <- coverage$lower >= 0.93 && coverage$upper <= 0.97 &&
    type1$upper <= 0.065
  implementation_pass <- runtime_rejected && wrong$pass && palm_compatible
  list(
    declared_unit = gsub("_", "-", unit_kind),
    design = design_name, schedule = schedule_name,
    seed_signal = signal_seed, seed_null = null_seed,
    independent_units = 6L,
    transformations = nrow(unrestricted_schedule),
    exact_enumeration = TRUE,
    underlying_restriction = "unrestricted-negative-control",
    replicates_attempted = replicates, failed_fits = 0L,
    failed_fit_rate = 0,
    coverage = coverage, type1 = type1,
    wrong_unit_rejection = wrong,
    schedule_order_difference = NULL,
    api_p_value_error = NULL, api_statistic_error = NULL,
    palm_column_schedule_compatible = palm_compatible,
    primary_gate_applicable = FALSE,
    post_primary_restriction = TRUE,
    original_calibration_gate_pass = original_gate_pass,
    runtime_free_with_blocks_rejected = runtime_rejected,
    restriction_reason = paste(
      "The frozen primary run found inflated type-I error for a free",
      "schedule with site-confounded nuisance structure. Stable use now",
      "requires within-block or a block-respecting user schedule."
    ),
    implementation_pass = implementation_pass,
    pass = implementation_pass && (!identical(run_mode, "full") ||
      !original_gate_pass)
  )
}

run_map_null_cell <- function(design_name, schedule_name) {
  rejected <- condition_is(
    ngeo_exchangeability(
      c("map_1", "map_2"), unit_kind = "map_null"
    ),
    "ngeo_error_exchangeability"
  )
  list(
    declared_unit = "map-null", design = design_name,
    schedule = schedule_name, replicates_attempted = 0L,
    failed_fits = 0L, failed_fit_rate = 0,
    coverage = NULL, type1 = NULL,
    wrong_unit_rejection = list(
      map_null_separation = rejected,
      rejected = as.integer(rejected), attempted = 1L,
      estimate = as.numeric(rejected), pass = rejected
    ),
    schedule_order_difference = NULL,
    primary_gate_applicable = FALSE,
    separation_reason = paste(
      "Map nulls preserve a map's spatial dependence and belong to the",
      "spatial-null API, not independent-unit group exchangeability."
    ),
    implementation_pass = rejected, pass = rejected
  )
}

units <- unlist(validation$factors$declared_unit, use.names = FALSE)
designs <- unlist(validation$factors$design, use.names = FALSE)
schedules <- unlist(validation$factors$schedule, use.names = FALSE)
cells <- list()
cell_index <- 0L
for (unit in units) {
  for (design_index in seq_along(designs)) {
    for (schedule_index in seq_along(schedules)) {
      cell_index <- cell_index + 1L
      cells[[cell_index]] <- if (identical(unit, "map-null")) {
        run_map_null_cell(designs[[design_index]], schedules[[schedule_index]])
      } else if (identical(designs[[design_index]], "site-confounded") &&
                 identical(schedules[[schedule_index]], "free")) {
        run_restricted_cell(
          gsub("-", "_", unit), designs[[design_index]],
          schedules[[schedule_index]], design_index, schedule_index
        )
      } else {
        run_supported_cell(
          gsub("-", "_", unit), designs[[design_index]],
          schedules[[schedule_index]], design_index, schedule_index
        )
      }
    }
  }
}

expected_cells <- length(units) * length(designs) * length(schedules)
cell_keys <- vapply(cells, function(cell) paste(
  cell$declared_unit, cell$design, cell$schedule, sep = "|"
), character(1))
coverage_complete <- length(cells) == expected_cells && !anyDuplicated(cell_keys)
implementation_passed <- all(vapply(
  cells, `[[`, logical(1), "implementation_pass"
))
primary_cells <- cells[vapply(
  cells, `[[`, logical(1), "primary_gate_applicable"
)]
restriction_cells <- cells[vapply(
  cells, function(cell) isTRUE(cell$post_primary_restriction), logical(1)
)]
primary_passed <- if (identical(run_mode, "full")) all(vapply(
  primary_cells, `[[`, logical(1), "primary_gate_pass"
)) else TRUE
passed <- coverage_complete && implementation_passed && primary_passed
result <- list(
  schema = "neurogeo/phase3-validation/1",
  validation_id = validation$id,
  simulation_id = validation$simulation_id,
  design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  dependency_versions = as.list(vapply(
    required, function(package) as.character(utils::packageVersion(package)),
    character(1)
  )),
  platform = R.version$platform, r_version = R.version.string,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  run_mode = run_mode, seed_base = validation$seed_base,
  replicates_per_calibration_cell = replicates,
  attempted_replicates_are_denominator = TRUE,
  primary_evidence_eligible = identical(run_mode, "full") && passed,
  validation = if (passed && identical(run_mode, "full") &&
                   length(restriction_cells)) {
    "passed-with-restriction"
  } else if (passed) {
    "debug-passed"
  } else {
    "failed"
  },
  registered_cell_count = expected_cells,
  observed_cell_count = length(cells),
  registered_cell_coverage_complete = coverage_complete,
  primary_cell_count = length(primary_cells),
  restriction_cell_count = length(restriction_cells),
  separation_cell_count = length(cells) - length(primary_cells) -
    length(restriction_cells),
  evidence_boundary = paste(
    "Structural unit declarations and schedules are machine checked;",
    "the package cannot infer whether a user-chosen label matches the",
    "biological sampling process. PALM compatibility here means the",
    "documented column-oriented permutation-matrix convention, not an",
    "external PALM execution."
  ),
  cells = unname(cells)
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
