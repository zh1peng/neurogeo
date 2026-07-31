args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "group-inference-47-validation.json")
required <- c("jsonlite", "permuco")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Group inference 4.7 validation requires: ",
       paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

make_features <- function(values, ids, bounds = "unbounded") {
  values <- as.matrix(values)
  if (is.null(colnames(values))) {
    colnames(values) <- paste0("endpoint_", seq_len(ncol(values)))
  }
  endpoints <- data.frame(
    endpoint_id = colnames(values), family = "simulation",
    estimand = ifelse(bounds == "[-1,1]", "spectral_coupling", "energy"),
    layer_x = "x", layer_y = "y", direction = "none",
    component = "component_001", band = "retained",
    scale_type = "rank_matched", bounds = bounds,
    recommended_transform = ifelse(bounds == "[-1,1]", "fisher_z", "none"),
    stringsAsFactors = FALSE
  )
  structure(list(
    values = values,
    units = data.frame(unit_id = ids, stringsAsFactors = FALSE),
    endpoints = endpoints,
    diagnostics = list(),
    provenance = list(source = "calibrated_simulation")
  ), class = "ngeo_subject_features")
}

correlated_errors <- function(n, p) {
  common <- rnorm(n)
  matrix(rnorm(n * p), n, p) * sqrt(0.6) + common * sqrt(0.4)
}

binomial_upper <- function(replicates, alpha = 0.05) {
  stats::qbinom(0.995, replicates, alpha) / replicates
}

run_null <- function(replicates, permutations, scenario) {
  n <- 40L
  p <- 6L
  ids <- paste0("s", seq_len(n))
  if (identical(scenario, "site_blocks")) {
    site <- factor(rep(paste0("site", 1:4), each = 10L))
    group <- factor(rep(rep(c("a", "b"), each = 5L), 4L))
    age <- seq(-1, 1, length.out = n)
    design <- data.frame(unit_id = ids, age = age, site = site, group = group)
    schedule <- ngeo_exchangeability(
      ids, scheme = "within_block", blocks = site,
      permutations = permutations, seed = 4703L
    )
    model <- ~ age + site + group
  } else {
    group <- factor(c(rep("a", 23L), rep("b", 17L)))
    age <- seq(-1, 1, length.out = n) + ifelse(group == "b", 0.55, 0)
    design <- data.frame(unit_id = ids, age = age, group = group)
    schedule <- ngeo_exchangeability(
      ids, permutations = permutations,
      seed = if (identical(scenario, "nuisance")) 4702L else 4701L
    )
    model <- ~ age + group
  }
  raw_rejections <- 0L
  family_rejections <- logical(replicates)
  for (iteration in seq_len(replicates)) {
    errors <- correlated_errors(n, p)
    nuisance <- if (identical(scenario, "pure_null")) 0 else
      0.9 * design$age
    values <- errors + nuisance
    colnames(values) <- paste0("e", seq_len(p))
    result <- ngeo_group_test(
      make_features(values, ids), design, model, "group", schedule,
      transform = "none", adjustment = "maxT", workers = 1L
    )
    raw_rejections <- raw_rejections + sum(result$tests$p_raw <= 0.05)
    family_rejections[[iteration]] <- any(result$tests$p_maxT <= 0.05)
  }
  fwer <- mean(family_rejections)
  endpoint_rate <- raw_rejections / (replicates * p)
  upper <- binomial_upper(replicates)
  list(
    replicates = replicates, permutations = permutations,
    endpoint_type1 = endpoint_rate, family_wise_error = fwer,
    binomial_99_5_upper_gate = upper,
    pass = fwer <= upper && endpoint_rate <= upper
  )
}

run_sign_null <- function(replicates, permutations) {
  n <- 14L
  ids <- paste0("pair", seq_len(n))
  design <- data.frame(unit_id = ids)
  schedule <- ngeo_exchangeability(
    ids, scheme = "sign_flip", permutations = permutations, seed = 4704L
  )
  rejected <- logical(replicates)
  for (iteration in seq_len(replicates)) {
    result <- ngeo_group_test(
      make_features(cbind(change = rnorm(n)), ids),
      design, ~ 1, "(Intercept)", schedule, transform = "none"
    )
    rejected[[iteration]] <- result$tests$p_raw <= 0.05
  }
  rate <- mean(rejected)
  upper <- binomial_upper(replicates)
  list(
    replicates = replicates, permutations = permutations,
    type1 = rate, binomial_99_5_upper_gate = upper,
    pass = rate <= upper
  )
}

run_signal <- function(replicates, permutations, distributed = FALSE) {
  n <- 40L
  p <- 6L
  ids <- paste0("s", seq_len(n))
  group <- factor(rep(c("a", "b"), each = n / 2L))
  design <- data.frame(unit_id = ids, group = group)
  schedule <- ngeo_exchangeability(
    ids, permutations = permutations,
    seed = if (distributed) 4712L else 4711L
  )
  detected <- logical(replicates)
  for (iteration in seq_len(replicates)) {
    values <- correlated_errors(n, p)
    if (distributed) {
      values[group == "b", ] <- values[group == "b", ] + 0.75
    } else {
      values[group == "b", 1L] <- values[group == "b", 1L] + 1.6
    }
    colnames(values) <- paste0("e", seq_len(p))
    result <- ngeo_group_test(
      make_features(values, ids), design, ~ group, "group", schedule,
      transform = "none"
    )
    detected[[iteration]] <- if (distributed) {
      result$omnibus$p_value[result$omnibus$omnibus == "sum_sq"] <= 0.05
    } else {
      result$tests$p_maxT[[1L]] <= 0.05
    }
  }
  power <- mean(detected)
  list(
    replicates = replicates, permutations = permutations,
    target = if (distributed) "sum_sq_omnibus" else "sparse_endpoint_maxT",
    power = power,
    pass = power >= 0.6
  )
}

full <- identical(
  tolower(Sys.getenv("NEUROGEO_GROUP_FULL_CALIBRATION")), "true"
)
replicates <- if (full) 100L else 20L
permutations <- if (full) 199L else 99L
set.seed(4700L)
checks <- list(
  pure_null = run_null(replicates, permutations, "pure_null"),
  nuisance_null = run_null(replicates, permutations, "nuisance"),
  site_block_null = run_null(replicates, permutations, "site_blocks"),
  sign_flip_null = run_sign_null(replicates, permutations),
  sparse_signal = run_signal(replicates, permutations, FALSE),
  distributed_signal = run_signal(replicates, permutations, TRUE)
)

# Independent fixed-schedule Freedman--Lane reference.
n <- 14L
ids <- paste0("r", seq_len(n))
design <- data.frame(
  unit_id = ids, age = seq_len(n),
  group = factor(rep(c("a", "b"), each = n / 2L))
)
y <- 0.1 * design$age + 0.6 * (design$group == "b") + rnorm(n)
schedule <- ngeo_exchangeability(ids, permutations = 59L, seed = 47L)
observed <- ngeo_group_test(
  make_features(cbind(endpoint = y), ids), design,
  ~ age + group, "group", schedule, transform = "none"
)
permutation_matrix <- t(rbind(seq_len(n), schedule$schedule))
permutation_matrix <- structure(
  permutation_matrix, type = "permutation", counting = "user",
  np = ncol(permutation_matrix), n = nrow(permutation_matrix), class = "Pmat"
)
reference <- suppressWarnings(permuco::lmperm(
  y ~ age + group, data = design, P = permutation_matrix,
  method = "freedman_lane"
))
row <- match("groupb", rownames(reference$table))
checks$permuco_reference <- list(
  statistic_error = abs(observed$tests$statistic[[1L]] -
    reference$table[row, "t value"]),
  p_value_error = abs(observed$tests$p_raw[[1L]] -
    reference$table[row, "resampled Pr(>|t|)"]),
  pass = abs(observed$tests$statistic[[1L]] -
    reference$table[row, "t value"]) <= 1e-10 &&
    abs(observed$tests$p_raw[[1L]] -
      reference$table[row, "resampled Pr(>|t|)"]) <= 1e-12
)

# Deterministic parallel order and bounded endpoint streaming.
performance_n <- if (full) 200L else 80L
performance_p <- if (full) 256L else 128L
performance_b <- if (full) 999L else 199L
ids <- paste0("p", seq_len(performance_n))
group <- factor(rep(c("a", "b"), length.out = performance_n))
age <- seq(-1, 1, length.out = performance_n)
design <- data.frame(unit_id = ids, age = age, group = group)
values <- correlated_errors(performance_n, performance_p)
colnames(values) <- paste0("endpoint_", seq_len(performance_p))
features <- make_features(values, ids)
schedule <- ngeo_exchangeability(
  ids, permutations = performance_b, seed = 4720L
)
timing <- system.time({
  serial <- ngeo_group_test(
    features, design, ~ age + group, "group", schedule,
    transform = "none", workers = 1L
  )
})
parallel_subset <- ngeo_group_test(
  make_features(values[, 1:8, drop = FALSE], ids), design,
  ~ age + group, "group", schedule,
  transform = "none", workers = 2L, retain_null = TRUE
)
serial_subset <- ngeo_group_test(
  make_features(values[, 1:8, drop = FALSE], ids), design,
  ~ age + group, "group", schedule,
  transform = "none", workers = 1L, retain_null = TRUE
)
checks$streaming_performance <- list(
  subjects = performance_n, endpoints = performance_p,
  permutations = performance_b,
  elapsed_seconds = unname(timing[["elapsed"]]),
  result_bytes = as.numeric(object.size(serial)),
  endpoint_null_retained = "endpoint" %in% names(serial$null),
  vertices_permuted = serial$diagnostics$vertices_permuted,
  pass = !"endpoint" %in% names(serial$null) &&
    identical(serial$diagnostics$vertices_permuted, FALSE)
)
checks$parallel_reproducibility <- list(
  maximum_test_error = max(abs(
    parallel_subset$tests$statistic - serial_subset$tests$statistic
  )),
  maximum_null_error = max(abs(
    parallel_subset$null$endpoint - serial_subset$null$endpoint
  )),
  schedule_hash_equal = identical(
    parallel_subset$exchangeability$schedule_hash,
    serial_subset$exchangeability$schedule_hash
  )
)
checks$parallel_reproducibility$pass <-
  checks$parallel_reproducibility$maximum_test_error <= 1e-12 &&
  checks$parallel_reproducibility$maximum_null_error <= 1e-12 &&
  checks$parallel_reproducibility$schedule_hash_equal

pass <- all(vapply(checks, `[[`, logical(1), "pass"))
report <- list(
  schema = "neurogeo/group-inference-47-validation",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  full_calibration = full,
  checks = checks,
  pass = pass,
  claim = paste(
    "Whole-subject Freedman-Lane calibration under declared exchangeability;",
    "no vertex permutation or arbitrary heteroscedasticity claim."
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, auto_unbox = TRUE, pretty = TRUE, digits = 16
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) stop("Group inference 4.7 validation failed.", call. = FALSE)
