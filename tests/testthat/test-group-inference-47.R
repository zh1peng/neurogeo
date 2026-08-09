group_feature_fixture <- function(values, ids = NULL, bounds = NULL) {
  values <- as.matrix(values)
  if (is.null(ids)) ids <- paste0("s", seq_len(nrow(values)))
  if (is.null(colnames(values))) {
    colnames(values) <- paste0("endpoint_", seq_len(ncol(values)))
  }
  if (is.null(bounds)) bounds <- rep("unbounded", ncol(values))
  endpoints <- data.frame(
    endpoint_id = colnames(values),
    family = "test",
    estimand = ifelse(bounds == "[-1,1]", "spectral_coupling", "energy"),
    layer_x = "x", layer_y = "y", direction = "none",
    component = "component_001", band = "retained",
    scale_type = "rank_matched", bounds = bounds,
    recommended_transform = ifelse(bounds == "[-1,1]", "fisher_z", "none"),
    stringsAsFactors = FALSE
  )
  structure(list(
    values = values,
    unit = data.frame(unit_id = ids, stringsAsFactors = FALSE),
    endpoints = endpoints,
    diagnostics = list(),
    history = list(source = "unit-test")
  ), class = "ngeo_subject_features")
}

manual_t <- function(y, full, reduced, test_column) {
  full_fit <- stats::lm.fit(full, y)
  beta <- full_fit$coefficients[[test_column]]
  mse <- sum(full_fit$residuals^2) / full_fit$df.residual
  covariance <- chol2inv(qr.R(qr(full)))
  column <- match(test_column, colnames(full))
  beta / sqrt(mse * covariance[column, column])
}

test_that("Freedman-Lane matches an explicit fixed-schedule calculation", {
  set.seed(11)
  n <- 18L
  ids <- paste0("s", seq_len(n))
  age <- seq(-1, 1, length.out = n)
  group <- factor(rep(c("control", "case"), each = n / 2))
  y1 <- 0.4 * age + 0.8 * (group == "case") + stats::rnorm(n, sd = 0.4)
  y2 <- -0.2 * age + 0.5 * (group == "case") + stats::rnorm(n, sd = 0.5)
  features <- group_feature_fixture(cbind(energy = y1, roughness = y2), ids)
  design <- data.frame(unit_id = ids, age = age, group = group)
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 79L, seed = 29L
  )
  result <- ngeo_group_test(
    features, design, model = ~ age + group, test = "group",
    exchangeability = exchangeability, transform = "none",
    retain_null = TRUE
  )
  expect_s3_class(result, "ngeo_group_result")
  expect_identical(result$sampling_unit, "subject")
  expect_identical(ngeo_inference_contract(result)$sampling_unit, "subject")
  expect_equal(nrow(result$tests), 2L)
  expect_true(all(result$tests$statistic_type == "t"))
  expect_true(all(result$tests$p_maxT >= result$tests$p_raw))
  expect_true(all(result$tests$partial_r2 >= 0 & result$tests$partial_r2 <= 1))
  expect_true(all(is.finite(result$tests$interval_low)))
  expect_equal(result$omnibus$omnibus, c("max", "sum_sq"))
  expect_true(nrow(result$design$group_summaries) > 0L)
  expect_equal(dim(result$null$endpoint), c(79L, 2L))

  full <- model.matrix(~ age + group, design)
  reduced <- model.matrix(~ age, design)
  reduced_fit <- stats::lm.fit(reduced, y1)
  tested_column <- grep("^group", colnames(full), value = TRUE)
  observed <- manual_t(y1, full, reduced, tested_column)
  simulated <- vapply(seq_len(nrow(exchangeability$schedule)), function(i) {
    yb <- reduced_fit$fitted.values +
      reduced_fit$residuals[exchangeability$schedule[i, ]]
    manual_t(yb, full, reduced, tested_column)
  }, numeric(1))
  expected_p <- (1 + sum(abs(simulated) >= abs(observed))) /
    (length(simulated) + 1)
  expect_equal(result$tests$statistic[[1L]], observed, tolerance = 1e-10)
  expect_equal(result$tests$p_raw[[1L]], expected_p, tolerance = 1e-12)
  expect_equal(result$null$endpoint[, 1L], simulated, tolerance = 1e-10)
})

test_that("group results retain a declared non-subject sampling unit", {
  ids <- paste0("site", 1:8)
  group <- factor(rep(c("control", "case"), each = 4L))
  features <- group_feature_fixture(cbind(endpoint = rnorm(8)), ids)
  design <- data.frame(unit_id = ids, group = group)
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 19L, seed = 47L, unit_kind = "site"
  )
  result <- ngeo_group_test(
    features, design, ~ group, "group", exchangeability,
    transform = "none"
  )
  expect_identical(result$sampling_unit, "site")
  expect_identical(result$history$inference_unit, "independent_site")
  expect_identical(ngeo_inference_contract(result)$sampling_unit, "site")
  expect_match(result$claim, "independent sites")
})

test_that("permuco provides an independent Freedman-Lane reference", {
  skip_if_not_installed("permuco")
  set.seed(21)
  n <- 12L
  ids <- paste0("s", seq_len(n))
  design <- data.frame(
    unit_id = ids,
    age = seq_len(n),
    group = factor(rep(c("a", "b"), each = n / 2))
  )
  y <- 0.1 * design$age + 0.7 * (design$group == "b") + rnorm(n)
  features <- group_feature_fixture(cbind(endpoint = y), ids)
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 39L, seed = 4L
  )
  observed <- ngeo_group_test(
    features, design, ~ age + group, "group", exchangeability,
    transform = "none"
  )
  permutation_matrix <- t(rbind(
    seq_len(n), exchangeability$schedule
  ))
  permutation_matrix <- structure(
    permutation_matrix,
    type = "permutation", counting = "user",
    np = ncol(permutation_matrix), n = nrow(permutation_matrix),
    class = "Pmat"
  )
  reference <- suppressWarnings(permuco::lmperm(
    y ~ age + group, data = design, P = permutation_matrix,
    method = "freedman_lane"
  ))
  row <- match("groupb", rownames(reference$table))
  expect_equal(
    observed$tests$statistic[[1L]],
    reference$table[row, "t value"], tolerance = 1e-10
  )
  expect_equal(
    observed$tests$p_raw[[1L]],
    reference$table[row, "resampled Pr(>|t|)"], tolerance = 1e-12
  )
})

test_that("design alignment is exact and invalid designs fail", {
  ids <- paste0("s", 1:10)
  values <- cbind(endpoint = seq_len(10) + rep(c(0, 1), 5))
  features <- group_feature_fixture(values, ids)
  design <- data.frame(
    unit_id = ids, age = seq_len(10),
    group = factor(rep(c("a", "b"), 5))
  )
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 19L, seed = 2L
  )
  ordered <- ngeo_group_test(
    features, design, ~ age + group, "group", exchangeability,
    transform = "none"
  )
  shuffled <- ngeo_group_test(
    features, design[sample(seq_len(nrow(design))), ],
    ~ age + group, "group", exchangeability, transform = "none"
  )
  expect_equal(shuffled$tests, ordered$tests)

  duplicate <- rbind(design, design[1L, ])
  expect_error(
    ngeo_group_test(
      features, duplicate, ~ age + group, "group", exchangeability
    ),
    class = "ngeo_error_design"
  )
  missing <- design
  missing$age[[1L]] <- NA_real_
  expect_error(
    ngeo_group_test(
      features, missing, ~ age + group, "group", exchangeability
    ),
    class = "ngeo_error_missing"
  )
  repeated <- design
  repeated$subject_id <- rep(paste0("p", 1:5), each = 2L)
  expect_error(
    ngeo_group_test(
      features, repeated, ~ age + group, "group", exchangeability
    ),
    class = "ngeo_error_independent_unit"
  )
  rank_deficient <- design
  rank_deficient$age_copy <- rank_deficient$age
  expect_error(
    ngeo_group_test(
      features, rank_deficient, ~ age + age_copy + group, "group",
      exchangeability
    ),
    class = "ngeo_error_design_rank"
  )
})

test_that("auto Fisher transform is auditable and raw values are retained", {
  n <- 16L
  ids <- paste0("s", 1:n)
  group <- factor(rep(c("a", "b"), each = n / 2))
  coupling <- tanh(seq(-1.2, 1.2, length.out = n) +
    0.3 * (group == "b"))
  features <- group_feature_fixture(
    cbind(coupling = coupling), ids, bounds = "[-1,1]"
  )
  design <- data.frame(unit_id = ids, group = group)
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 39L, seed = 8L
  )
  result <- ngeo_group_test(
    features, design, ~ group, "group", exchangeability,
    transform = "auto"
  )
  expect_identical(result$tests$transform, "fisher_z")
  expect_equal(result$raw_values[, 1L], coupling)
  expect_true(all(is.finite(result$analysis_values)))
  expect_match(result$claim, "subjects")
  expect_false("endpoint" %in% names(result$null))
})

test_that("multi-df terms use partial F and named contrasts remain one-df", {
  n <- 18L
  ids <- paste0("s", 1:n)
  group <- factor(rep(c("a", "b", "c"), each = 6L))
  age <- seq(-1, 1, length.out = n)
  values <- cbind(endpoint = age + as.numeric(group) / 3 + rnorm(n, sd = 0.2))
  features <- group_feature_fixture(values, ids)
  design <- data.frame(unit_id = ids, age = age, group = group)
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 39L, seed = 17L
  )
  omnibus_term <- ngeo_group_test(
    features, design, ~ age + group, "group", exchangeability,
    transform = "none"
  )
  expect_identical(omnibus_term$tests$statistic_type, "F")
  expect_equal(omnibus_term$tests$df_num, 2L)
  expect_true(omnibus_term$tests$statistic >= 0)
  expect_true(is.na(omnibus_term$tests$coefficient))

  columns <- colnames(model.matrix(~ age + group, design))
  contrast <- stats::setNames(rep(0, length(columns)), columns)
  contrast[["groupb"]] <- 1
  contrast_result <- ngeo_group_test(
    features, design, ~ age + group, contrast, exchangeability,
    transform = "none"
  )
  expect_identical(contrast_result$tests$statistic_type, "t")
  expect_equal(contrast_result$tests$df_num, 1L)
  expect_true(is.finite(contrast_result$tests$coefficient))
})

test_that("max-T families can be declared from endpoint metadata", {
  n <- 14L
  ids <- paste0("s", 1:n)
  values <- cbind(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  features <- group_feature_fixture(values, ids)
  features$endpoints$band <- c("low", "high", "high")
  design <- data.frame(
    unit_id = ids, group = factor(rep(c("a", "b"), each = n / 2))
  )
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 31L, seed = 9L
  )
  result <- ngeo_group_test(
    features, design, ~ group, "group", exchangeability,
    family = "band", transform = "none"
  )
  expect_equal(result$tests$maxT_family, c("low", "high", "high"))
  expect_true(result$tests$p_maxT[[1L]] >= result$tests$p_raw[[1L]])
  expect_true(all(result$tests$p_maxT[2:3] >= result$tests$p_raw[2:3]))
  expect_true(nzchar(result$diagnostics$family_hash))
})

test_that("constant endpoints fail instead of producing invalid p-values", {
  ids <- paste0("s", 1:8)
  features <- group_feature_fixture(cbind(endpoint = rep(1, 8)), ids)
  design <- data.frame(
    unit_id = ids, group = factor(rep(c("a", "b"), each = 4L))
  )
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 15L, seed = 2L
  )
  expect_error(
    ngeo_group_test(
      features, design, ~ group, "group", exchangeability,
      transform = "none"
    ),
    class = "ngeo_error_statistic"
  )
})

test_that("sign flips test one contrast per independent pair", {
  differences <- c(1.2, 0.8, 1.1, -0.1, 0.7, 1.4)
  ids <- paste0("pair", seq_along(differences))
  features <- group_feature_fixture(cbind(change = differences), ids)
  design <- data.frame(unit_id = ids)
  exchangeability <- ngeo_exchangeability(
    ids, scheme = "sign_flip", permutations = 99L, seed = 1L
  )
  result <- ngeo_group_test(
    features, design, ~ 1, "(Intercept)", exchangeability,
    transform = "none"
  )
  expect_identical(result$tests$statistic_type, "t")
  expect_equal(result$tests$df_num, 1L)
  expect_true(result$tests$coefficient > 0)
  expect_identical(result$exchangeability$scheme, "sign_flip")
})

test_that("complete-family missingness uses one declared unit set", {
  ids <- paste0("s", 1:8)
  values <- cbind(a = seq_len(8), b = rev(seq_len(8)))
  values[8L, 2L] <- NA_real_
  features <- group_feature_fixture(values, ids)
  design <- data.frame(
    unit_id = ids, group = factor(rep(c("a", "b"), 4L))
  )
  all_schedule <- ngeo_exchangeability(ids, permutations = 19L, seed = 3L)
  expect_error(
    ngeo_group_test(
      features, design, ~ group, "group", all_schedule,
      missing = "complete_family"
    ),
    class = "ngeo_error_alignment"
  )
  kept <- ids[-8L]
  kept_schedule <- ngeo_exchangeability(kept, permutations = 19L, seed = 3L)
  result <- ngeo_group_test(
    features, design, ~ group, "group", kept_schedule,
    missing = "complete_family"
  )
  expect_identical(result$design$unit_id, kept)
  expect_equal(result$diagnostics$dropped_incomplete_units, "s8")
  expect_error(
    ngeo_group_test(
      features, design, ~ group, "group", kept_schedule,
      missing = "error"
    ),
    class = "ngeo_error_missing"
  )
})

test_that("within-block group confounding is rejected", {
  ids <- paste0("s", 1:8)
  blocks <- rep(c("site-a", "site-b"), each = 4L)
  group <- factor(rep(c("control", "case"), each = 4L))
  features <- group_feature_fixture(cbind(endpoint = rnorm(8)), ids)
  design <- data.frame(unit_id = ids, block = blocks, group = group)
  exchangeability <- ngeo_exchangeability(
    ids, scheme = "within_block", blocks = blocks,
    permutations = 19L, seed = 5L
  )
  expect_error(
    ngeo_group_test(
      features, design, ~ group, "group", exchangeability
    ),
    class = "ngeo_error_exchangeability_design"
  )
})

test_that("worker count does not change schedule order or results", {
  n <- 14L
  ids <- paste0("s", 1:n)
  design <- data.frame(
    unit_id = ids, age = seq_len(n),
    group = factor(rep(c("a", "b"), each = n / 2))
  )
  features <- group_feature_fixture(cbind(
    a = rnorm(n), b = rnorm(n), c = rnorm(n)
  ), ids)
  exchangeability <- ngeo_exchangeability(
    ids, permutations = 47L, seed = 12L
  )
  serial <- ngeo_group_test(
    features, design, ~ age + group, "group", exchangeability,
    transform = "none", workers = 1L, retain_null = TRUE
  )
  parallel <- ngeo_group_test(
    features, design, ~ age + group, "group", exchangeability,
    transform = "none", workers = 2L, retain_null = TRUE
  )
  expect_equal(parallel$tests, serial$tests, tolerance = 1e-12)
  expect_equal(parallel$null, serial$null, tolerance = 1e-12)
  expect_identical(parallel$exchangeability$schedule_hash,
                   serial$exchangeability$schedule_hash)
})
