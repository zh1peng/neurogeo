support_feature_fixture <- function(values, ids, support_hash,
                                    band = "low", scale_type = "rank_matched",
                                    boundary = NULL) {
  values <- as.matrix(values)
  if (is.null(colnames(values))) {
    colnames(values) <- paste0("endpoint_", seq_len(ncol(values)))
  }
  endpoints <- data.frame(
    endpoint_id = colnames(values), family = "coupling",
    estimand = rep(c("spectral_coupling", "band_energy_x"),
                   length.out = ncol(values)),
    layer_x = "thickness", layer_y = "myelin", direction = "none",
    component = "cortex", band = band, scale_type = scale_type,
    eigenvalue_min = 0.1, eigenvalue_max = 1,
    mode_count = 8L, bounds = rep(c("[-1,1]", "unbounded"),
                                 length.out = ncol(values)),
    recommended_transform = rep(c("fisher_z", "none"),
                                length.out = ncol(values)),
    support_hash = support_hash,
    stringsAsFactors = FALSE
  )
  structure(list(
    values = values,
    units = data.frame(unit_id = ids, stringsAsFactors = FALSE),
    endpoints = endpoints,
    diagnostics = list(boundary_sensitivity = boundary),
    provenance = list(support_hash = support_hash)
  ), class = "ngeo_subject_features")
}

test_that("one common schedule drives every declared support", {
  set.seed(48)
  n <- 18L
  ids <- paste0("s", 1:n)
  group <- factor(rep(c("a", "b"), each = n / 2L))
  base <- rnorm(n) + 0.8 * (group == "b")
  features <- list(
    vertex = support_feature_fixture(
      cbind(coupling = tanh(base / 2), energy = base^2 + 0.1),
      ids, "hash-vertex", boundary = list(mean = 0.12)
    ),
    atlas100 = support_feature_fixture(
      cbind(coupling = tanh((base + 0.05) / 2), energy = base^2 + 0.2),
      ids, "hash-atlas100", boundary = list(mean = 0.18)
    )
  )
  design <- data.frame(unit_id = ids, group = group)
  schedule <- ngeo_exchangeability(ids, permutations = 59L, seed = 4801L)
  result <- ngeo_group_test(
    features, design, ~ group, "group", schedule
  )
  expect_s3_class(result, "ngeo_group_result")
  expect_equal(unique(result$tests$support_id), c("vertex", "atlas100"))
  expect_equal(nrow(result$tests), 4L)
  expect_true(all(result$tests$p_maxT >= result$tests$p_raw))
  expect_identical(result$exchangeability$schedule_hash,
                   schedule$schedule_hash)
  expect_true(result$diagnostics$common_schedule_all_supports)
  expect_equal(result$support$analysis_order, c("vertex", "atlas100"))
  expect_equal(nrow(result$support$boundary), 2L)
  expect_false("stable" %in% names(result$support$stability))
  expect_match(result$claim, "declared support family")
  expect_match(result$claim, "not parcellation-invariant")
})

test_that("semantic matches yield descriptive support stability", {
  n <- 16L
  ids <- paste0("s", 1:n)
  group <- factor(rep(c("a", "b"), each = n / 2L))
  signal <- seq(-1, 1, length.out = n) + 0.6 * (group == "b")
  first <- support_feature_fixture(
    cbind(coupling = tanh(signal / 2)), ids, "h1"
  )
  second <- support_feature_fixture(
    cbind(coupling = tanh(signal / 2)), ids, "h2"
  )
  schedule <- ngeo_exchangeability(ids, permutations = 39L, seed = 4802L)
  result <- ngeo_group_test(
    list(a = first, b = second),
    data.frame(unit_id = ids, group = group),
    ~ group, "group", schedule
  )
  stability <- result$support$stability
  expect_equal(nrow(stability), 1L)
  expect_true(stability$direction_agreement)
  expect_equal(stability$effect_range, 0, tolerance = 1e-12)
  expect_equal(stability$effect_sd, 0, tolerance = 1e-12)
  expect_equal(stability$max_loso_influence, 0, tolerance = 1e-12)
  expect_identical(stability$scale_comparability, "rank_matched")
})

test_that("support-induced sign reversal and one-driver influence are visible", {
  n <- 20L
  ids <- paste0("s", 1:n)
  group <- factor(rep(c("a", "b"), each = n / 2L))
  group_numeric <- as.numeric(group == "b")
  set.seed(4803)
  noise <- rnorm(n, sd = 0.15)
  features <- list(
    a = support_feature_fixture(
      cbind(coupling = tanh(0.7 * group_numeric + noise)), ids, "ha"
    ),
    b = support_feature_fixture(
      cbind(coupling = tanh(0.6 * group_numeric + noise)), ids, "hb"
    ),
    driver = support_feature_fixture(
      cbind(coupling = tanh(-1.5 * group_numeric + noise)), ids, "hc"
    )
  )
  expect_warning(
    result <- ngeo_group_test(
      features, data.frame(unit_id = ids, group = group),
      ~ group, "group",
      ngeo_exchangeability(ids, permutations = 79L, seed = 4803L)
    ),
    class = "ngeo_warning_heteroscedasticity"
  )
  stability <- result$support$stability
  expect_false(stability$direction_agreement)
  expect_identical(stability$driving_support, "driver")
  expect_true(stability$max_loso_influence > 0)
  expect_true(stability$significance_persistence >= 0 &
    stability$significance_persistence <= 1)
})

test_that("full-family max-T is at least as conservative as separate support", {
  set.seed(4810)
  n <- 18L
  ids <- paste0("s", 1:n)
  group <- factor(rep(c("a", "b"), each = n / 2L))
  first <- support_feature_fixture(
    cbind(coupling = tanh(rnorm(n) + 0.8 * (group == "b"))), ids, "h1"
  )
  second <- support_feature_fixture(
    cbind(coupling = tanh(rnorm(n) + 0.8 * (group == "b"))), ids, "h2"
  )
  design <- data.frame(unit_id = ids, group = group)
  schedule <- ngeo_exchangeability(ids, permutations = 59L, seed = 4804L)
  combined <- ngeo_group_test(
    list(one = first, two = second), design,
    ~ group, "group", schedule
  )
  separate <- ngeo_group_test(
    first, design, ~ group, "group", schedule
  )
  combined_first <- combined$tests$p_maxT[combined$tests$support_id == "one"]
  expect_true(combined_first >= separate$tests$p_maxT)
  expect_true(nzchar(combined$diagnostics$family_hash))
  expect_true(nzchar(combined$provenance$support_family_hash))
})

test_that("support lists require exact units, names, and hashes", {
  ids <- paste0("s", 1:8)
  values <- cbind(coupling = seq(-0.8, 0.8, length.out = 8))
  first <- support_feature_fixture(values, ids, "h1")
  reordered <- support_feature_fixture(values[8:1, , drop = FALSE],
                                       rev(ids), "h2")
  design <- data.frame(
    unit_id = ids, group = factor(rep(c("a", "b"), each = 4L))
  )
  schedule <- ngeo_exchangeability(ids, permutations = 15L, seed = 4805L)
  expect_error(
    ngeo_group_test(
      list(a = first, b = reordered), design,
      ~ group, "group", schedule
    ),
    class = "ngeo_error_alignment"
  )
  missing_hash <- first
  missing_hash$endpoints$support_hash <- NA_character_
  missing_hash$provenance$support_hash <- NULL
  expect_error(
    ngeo_group_test(
      list(a = first, b = missing_hash), design,
      ~ group, "group", schedule
    ),
    class = "ngeo_error_support_family"
  )
  expect_error(
    ngeo_group_test(
      unname(list(first, first)), design,
      ~ group, "group", schedule
    ),
    class = "ngeo_error_support_family"
  )
})

test_that("unmatched semantic scales remain support-specific", {
  n <- 12L
  ids <- paste0("s", 1:n)
  values <- cbind(coupling = seq(-0.7, 0.7, length.out = n))
  first <- support_feature_fixture(values, ids, "h1", scale_type = "unmatched")
  second <- support_feature_fixture(values, ids, "h2", scale_type = "unmatched")
  group <- factor(rep(c("a", "b"), each = n / 2L))
  result <- ngeo_group_test(
    list(a = first, b = second),
    data.frame(unit_id = ids, group = group),
    ~ group, "group",
    ngeo_exchangeability(ids, permutations = 23L, seed = 4806L)
  )
  expect_equal(nrow(result$support$stability), 0L)
  expect_equal(result$support$diagnostics$unmatched_endpoints, 2L)
})

test_that("complete-family deletion spans all supports", {
  ids <- paste0("s", 1:9)
  values <- cbind(coupling = seq(-0.8, 0.8, length.out = 9))
  first <- support_feature_fixture(values, ids, "h1")
  second <- support_feature_fixture(values, ids, "h2")
  second$values[9L, 1L] <- NA_real_
  group <- factor(rep(c("a", "b", "a"), 3L))
  design <- data.frame(unit_id = ids, group = group)
  kept <- ids[-9L]
  result <- ngeo_group_test(
    list(a = first, b = second), design,
    ~ group, "group",
    ngeo_exchangeability(kept, permutations = 15L, seed = 4807L),
    missing = "complete_family"
  )
  expect_identical(result$design$unit_id, kept)
  expect_equal(result$diagnostics$dropped_incomplete_units, "s9")
  expect_true(result$diagnostics$complete_family_across_supports)
})
