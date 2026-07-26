test_that("support-family adjustment matches independent references", {
  p_value <- c(0.01, 0.04, 0.2)
  expect_equal(
    ngeo_support_adjust(p_value, method = "BH")$adjusted,
    stats::p.adjust(p_value, method = "BH")
  )

  observed <- c(2, -1)
  simulated <- rbind(
    c(0, 0),
    c(1, -2),
    c(3, 0.5)
  )
  adjusted <- ngeo_support_adjust(
    method = "maxT",
    observed = observed,
    simulated = simulated
  )
  expect_equal(adjusted$raw, c(0.5, 0.5))
  expect_equal(adjusted$adjusted, c(0.75, 0.75))
  expect_true(all(adjusted$adjusted >= adjusted$raw))
})

test_that("cross-atlas consensus matches inverse-variance formulas", {
  estimate <- c(1, 2, 4)
  standard_error <- c(1, 2, 1)
  fixed <- ngeo_cross_atlas_consensus(
    estimate,
    standard_error,
    method = "fixed",
    labels = c("a", "b", "c")
  )
  weight <- 1 / standard_error^2

  expect_equal(
    fixed$estimate,
    sum(weight * estimate) / sum(weight)
  )
  expect_equal(fixed$standard_error, sqrt(1 / sum(weight)))
  expect_equal(nrow(fixed$leave_one_out), 3L)
  expect_identical(fixed$claim, "cross-atlas consensus; not parcellation invariance")

  pair <- ngeo_cross_atlas_consensus(
    c(1, 3),
    c(0.5, 0.5),
    labels = c("left", "right")
  )
  expect_equal(pair$leave_one_out$estimate, c(3, 1))
  expect_true(all(is.na(c(
    neurogeo:::.ngeo_meta_estimate(1, 0.5, "random")$q_p_value
  ))))
})

test_that("common-source support tests are reproducible and family-aware", {
  fixture <- inference_fixture()
  first <- ngeo_common_support_test(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    nsim = 39,
    seed = 2023
  )
  second <- ngeo_common_support_test(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    nsim = 39,
    seed = 2023
  )

  expect_s3_class(first, "ngeo_common_support_test")
  expect_identical(first$simulated, second$simulated)
  expect_identical(first$estimates, second$estimates)
  expect_true(all(
    first$estimates$adjusted_p_value >= first$estimates$p_value
  ))
  expect_false(first$preserves_spatial_autocorrelation)
  expect_identical(first$null, "permutation")
})

test_that("Moran common-source null is explicitly spatial", {
  fixture <- inference_fixture()
  weights <- ngeo_weights(
    fixture$source,
    method = "mesh_contiguity",
    style = "W"
  )
  result <- ngeo_common_support_test(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    null = "moran",
    weights = weights,
    nsim = 7,
    seed = 17
  )

  expect_true(result$preserves_spatial_autocorrelation)
  expect_identical(result$null, "moran")
  expect_equal(dim(result$simulated), c(7L, 2L))
})

test_that("multiscale inference preserves caller-declared hierarchy", {
  fixture <- inference_fixture()
  result <- ngeo_multiscale_inference(
    fixture$source,
    fixture$maps,
    fixture$targets,
    scales = c("coarse", "fine"),
    outcome = "outcome",
    predictor = "predictor",
    nsim = 19,
    seed = 8
  )

  expect_s3_class(result, "ngeo_multiscale_inference")
  expect_identical(result$estimates$scale, c("coarse", "fine"))
  expect_equal(
    result$adjacent_change$change,
    diff(result$estimates$statistic)
  )
  expect_equal(
    result$stability[["range"]],
    diff(range(result$estimates$statistic))
  )
  expect_error(
    ngeo_multiscale_inference(
      fixture$source,
      fixture$maps,
      fixture$targets,
      scales = c("same", "same"),
      outcome = "outcome",
      predictor = "predictor",
      nsim = 3
    ),
    class = "ngeo_error_alignment"
  )
})

test_that("boundary tests use a declared segmentation ensemble", {
  fixture <- inference_fixture()
  ensemble <- ngeo_segmentation_ensemble(fixture$maps)
  first <- ngeo_boundary_test(
    fixture$source,
    ensemble,
    fixture$targets[[1L]],
    outcome = "outcome",
    predictor = "predictor",
    nsim = 19,
    seed = 9
  )
  second <- ngeo_boundary_test(
    fixture$source,
    ensemble,
    fixture$targets[[1L]],
    outcome = "outcome",
    predictor = "predictor",
    nsim = 19,
    seed = 9
  )

  expect_s3_class(first, "ngeo_boundary_test")
  expect_identical(first$simulated_dispersion, second$simulated_dispersion)
  expect_equal(nrow(first$effects), 2L)
  expect_equal(nrow(first$boundary_sensitivity), 2L)
  expect_true(first$p_value >= 0 && first$p_value <= 1)
})

test_that("atlas effects expose intervals, consensus, and bootstrap audit", {
  fixture <- inference_fixture()
  first <- ngeo_atlas_robust_effect(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    bootstrap = 3,
    seed = 44
  )
  second <- ngeo_atlas_robust_effect(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    bootstrap = 3,
    seed = 44
  )

  expect_true(all(
    first$estimates$confidence_lower <= first$estimates$estimate
  ))
  expect_true(all(
    first$estimates$confidence_upper >= first$estimates$estimate
  ))
  expect_s3_class(first$meta_analysis, "ngeo_cross_atlas_consensus")
  expect_equal(nrow(first$leave_one_out), 2L)
  expect_identical(
    first$bootstrap$estimates,
    second$bootstrap$estimates
  )
  expect_identical(first$seed, 44L)
})

test_that("2.3 inference rejects malformed statistical families", {
  expect_error(
    ngeo_support_adjust(
      method = "maxT",
      observed = 1:2,
      simulated = matrix(1, nrow = 3, ncol = 1)
    ),
    class = "ngeo_error_inference"
  )
  expect_error(
    ngeo_cross_atlas_consensus(c(1, 2), c(1, 0)),
    class = "ngeo_error_inference"
  )
})
