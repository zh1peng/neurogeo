args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--smoke" %in% args) "smoke" else "full"
args <- args[args != "--smoke"]
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val305-operator-simplex-60.json")
required <- c("digest", "jsonlite", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-305 requires: ", paste(missing, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
design <- jsonlite::fromJSON(design_path, simplifyVector = FALSE)
validation <- Filter(
  function(x) identical(x$id, "VAL-305"), design$validations
)[[1L]]
common <- design$common
replicates <- if (identical(run_mode, "full")) {
  common$replicates_per_calibration_cell
} else {
  500L
}

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

empirical_support <- function(concentration) {
  scale <- 0.18 * sqrt(2 / concentration)
  direction <- rbind(
    c(1, -0.5, -0.5),
    c(-1, 0.5, 0.5),
    c(-0.5, 1, -0.5),
    c(0.5, -1, 0.5),
    c(-0.5, -0.5, 1),
    c(0.5, 0.5, -1)
  )
  list(
    weights = sweep(scale * direction, 2L, rep(1 / 3, 3L), "+"),
    probability = c(0.25, 0.25, 0.15, 0.15, 0.10, 0.10)
  )
}

draw_weights <- function(distribution, count, concentration) {
  if (identical(distribution, "empirical-weighted")) {
    empirical <- empirical_support(concentration)
    return(empirical$weights[sample.int(
      nrow(empirical$weights), count, replace = TRUE,
      prob = empirical$probability
    ), , drop = FALSE])
  }
  if (identical(distribution, "dirichlet")) {
    value <- matrix(
      stats::rgamma(count * 3L, shape = concentration / 3),
      nrow = count,
      ncol = 3L
    )
    return(value / rowSums(value))
  }
  if (identical(distribution, "logistic-normal")) {
    latent <- matrix(
      stats::rnorm(count * 3L, sd = 1 / sqrt(concentration)),
      nrow = count,
      ncol = 3L
    )
    latent <- latent - apply(latent, 1L, max)
    value <- exp(latent)
    return(value / rowSums(value))
  }
  if (identical(distribution, "independent-gaussian-ablation")) {
    standard_deviation <- sqrt(2 / (9 * (concentration + 1)))
    return(matrix(
      stats::rnorm(
        count * 3L, mean = 1 / 3, sd = standard_deviation
      ),
      nrow = count,
      ncol = 3L
    ))
  }
  stop("Unknown VAL-305 operator distribution: ", distribution)
}

source <- ngeo_point(
  matrix(c(0, 0), nrow = 1L),
  values = cbind(amount = 1),
  measures = ngeo_measure(support_behavior = "extensive")
)
target <- ngeo_parcellation(
  data.frame(region_id = c("A", "B", "C"), stringsAsFactors = FALSE),
  support_size = rep.int(NA_real_, 3L),
  coordinate_space = source$base$coordinate_space
)
support_map <- function(weight) {
  ngeo_support_map(
    source, target, matrix(weight, ncol = 1L),
    type = "probabilistic", source_support = 1
  )
}

api_reference <- function(weights) {
  maps <- lapply(seq_len(nrow(weights)), function(i) {
    support_map(weights[i, ])
  })
  ensemble <- ngeo_support_ensemble(maps)
  sensitivity <- ngeo_support_sensitivity(source, target, ensemble)
  observed_mean <- sensitivity$distribution$ensemble_mean
  observed_variance <- sensitivity$distribution$between_operator_variance
  expected_mean <- colMeans(weights)
  expected_variance <- colMeans(sweep(weights, 2L, expected_mean, "-")^2)
  list(
    mean_error = max(abs(observed_mean - expected_mean)),
    variance_error = max(abs(observed_variance - expected_variance)),
    ensemble_hash_verified = identical(
      sensitivity$ensemble_hash, ensemble$ensemble_hash
    )
  )
}

weighted_api_reference <- function(concentration) {
  empirical <- empirical_support(concentration)
  maps <- lapply(seq_len(nrow(empirical$weights)), function(i) {
    support_map(empirical$weights[i, ])
  })
  ensemble <- ngeo_support_ensemble(
    maps, spatial_weights = empirical$probability
  )
  sensitivity <- ngeo_support_sensitivity(source, target, ensemble)
  expected_mean <- as.numeric(
    t(empirical$weights) %*% empirical$probability
  )
  centered <- sweep(empirical$weights, 2L, expected_mean, "-")
  expected_variance <- as.numeric(
    t(centered^2) %*% empirical$probability
  )
  list(
    mean_error = max(abs(
      sensitivity$distribution$ensemble_mean - expected_mean
    )),
    variance_error = max(abs(
      sensitivity$distribution$between_operator_variance - expected_variance
    ))
  )
}

ablation_rejected <- function() {
  error <- tryCatch(
    support_map(c(0.4, 0.4, 0.4)),
    error = identity
  )
  inherits(error, "ngeo_error")
}

run_cell <- function(distribution, ensemble_size, concentration, seed) {
  set.seed(seed)
  measurement_noise_variance <- 0.05
  draw <- draw_weights(
    distribution, replicates * ensemble_size, concentration
  )
  contrast <- rowSums(draw[, 1:2, drop = FALSE])
  contrast <- matrix(
    contrast, nrow = replicates, ncol = ensemble_size, byrow = TRUE
  )
  estimated_mean <- rowMeans(contrast)
  estimated_variance <- apply(contrast, 1L, stats::var)
  future_weight <- draw_weights(distribution, replicates, concentration)
  future_outcome <- rowSums(future_weight[, 1:2, drop = FALSE]) +
    stats::rnorm(replicates, sd = sqrt(measurement_noise_variance))
  lower <- estimated_mean - stats::qnorm(0.975) *
    sqrt(measurement_noise_variance + estimated_variance)
  upper <- estimated_mean + stats::qnorm(0.975) *
    sqrt(measurement_noise_variance + estimated_variance)
  coverage <- wilson_interval(
    sum(future_outcome >= lower & future_outcome <= upper), replicates
  )
  truth <- 2 / 3
  bias <- mean(estimated_mean) - truth
  rmse <- sqrt(mean((estimated_mean - truth)^2))
  simplex_error <- max(abs(rowSums(draw) - 1))
  simplex_distribution <- !identical(
    distribution, "independent-gaussian-ablation"
  )
  api <- if (simplex_distribution) {
    api_reference(draw[seq_len(ensemble_size), , drop = FALSE])
  } else {
    list(
      mean_error = NA_real_,
      variance_error = NA_real_,
      ensemble_hash_verified = NA
    )
  }
  weighted <- if (identical(distribution, "empirical-weighted")) {
    weighted_api_reference(concentration)
  } else {
    list(mean_error = 0, variance_error = 0)
  }
  implementation_pass <- if (simplex_distribution) {
    simplex_error <= 1e-12 && api$mean_error <= 1e-12 &&
      api$variance_error <= 1e-12 &&
      isTRUE(api$ensemble_hash_verified) &&
      weighted$mean_error <= 1e-12 && weighted$variance_error <= 1e-12
  } else {
    simplex_error > 1e-6 && ablation_rejected()
  }
  coverage_pass <- coverage$lower >= 0.93 && coverage$upper <= 0.97
  list(
    operator_distribution = distribution,
    ensemble_size = ensemble_size,
    concentration = concentration,
    seed = seed,
    replicates_attempted = replicates,
    failed_fits = 0L,
    failed_fit_rate = 0,
    estimand = "future noisy mass allocated to targets A and B",
    measurement_noise_variance = measurement_noise_variance,
    bias = bias,
    rmse = rmse,
    coverage = coverage,
    simplex_error = simplex_error,
    api_mean_error = api$mean_error,
    api_variance_error = api$variance_error,
    weighted_api_mean_error = weighted$mean_error,
    weighted_api_variance_error = weighted$variance_error,
    public_api_rejected_non_simplex_ablation = if (simplex_distribution) {
      NA
    } else {
      ablation_rejected()
    },
    primary_gate_applicable = simplex_distribution,
    coverage_gate_pass = if (simplex_distribution) coverage_pass else NULL,
    implementation_pass = implementation_pass
  )
}

distributions <- unlist(
  validation$factors$operator_distribution, use.names = FALSE
)
ensemble_sizes <- unlist(
  validation$factors$ensemble_size, use.names = FALSE
)
concentrations <- unlist(
  validation$factors$concentration, use.names = FALSE
)
cells <- list()
cell_index <- 0L
for (distribution in distributions) {
  for (ensemble_size in ensemble_sizes) {
    for (concentration in concentrations) {
      cell_index <- cell_index + 1L
      cells[[cell_index]] <- run_cell(
        distribution, ensemble_size, concentration,
        validation$seed_base + cell_index
      )
    }
  }
}

for (i in seq_along(cells)) {
  cell <- cells[[i]]
  matched_ablation <- Filter(function(candidate) {
    identical(
      candidate$operator_distribution,
      "independent-gaussian-ablation"
    ) && identical(candidate$ensemble_size, cell$ensemble_size) &&
      identical(candidate$concentration, cell$concentration)
  }, cells)[[1L]]
  cells[[i]]$independent_gaussian_rmse <- matched_ablation$rmse
  cells[[i]]$rmse_gate_pass <- if (isTRUE(cell$primary_gate_applicable)) {
    cell$rmse <= matched_ablation$rmse
  } else {
    NULL
  }
  cells[[i]]$primary_gate_pass <- if (isTRUE(cell$primary_gate_applicable)) {
    isTRUE(cell$coverage_gate_pass) && abs(cell$bias) <= 0.02 &&
      isTRUE(cells[[i]]$rmse_gate_pass) && cell$simplex_error <= 1e-12
  } else {
    NULL
  }
  cells[[i]]$pass <- isTRUE(cell$implementation_pass) &&
    (identical(run_mode, "smoke") ||
       !isTRUE(cell$primary_gate_applicable) ||
       isTRUE(cells[[i]]$primary_gate_pass))
}

expected_cells <- length(distributions) * length(ensemble_sizes) *
  length(concentrations)
cell_keys <- vapply(cells, function(cell) paste(
  cell$operator_distribution, cell$ensemble_size, cell$concentration,
  sep = "|"
), character(1))
coverage_complete <- length(cells) == expected_cells &&
  !anyDuplicated(cell_keys)
passed <- coverage_complete && all(vapply(cells, `[[`, logical(1), "pass"))
primary_cells <- cells[vapply(
  cells, `[[`, logical(1), "primary_gate_applicable"
)]
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
  platform = R.version$platform,
  r_version = R.version.string,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  run_mode = run_mode,
  seed_base = validation$seed_base,
  replicates_per_cell = replicates,
  primary_evidence_eligible = identical(run_mode, "full") && passed,
  validation = if (passed && identical(run_mode, "full")) {
    "passed"
  } else if (passed) {
    "debug-passed"
  } else {
    "failed"
  },
  registered_cell_count = expected_cells,
  observed_cell_count = length(cells),
  registered_cell_coverage_complete = coverage_complete,
  primary_cell_count = length(primary_cells),
  ablation_cell_count = length(cells) - length(primary_cells),
  cells = unname(cells)
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
