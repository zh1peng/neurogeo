test_that("atlas-robust effects report a bounded coefficient family", {
  fixture <- inference_fixture()
  result <- ngeo_atlas_robust_effect(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor"
  )

  expect_s3_class(result, "ngeo_atlas_robust_effect")
  expect_equal(nrow(result$estimates), 2L)
  expect_true(all(is.finite(result$estimates$estimate)))
  expect_match(result$claim, "not local parcellation invariance")
})

test_that("common-source permutation inference is reproducible", {
  fixture <- inference_fixture()
  first <- ngeo_support_test(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    nsim = 19,
    seed = 2026
  )
  second <- ngeo_support_test(
    fixture$source,
    fixture$maps,
    fixture$targets,
    outcome = "outcome",
    predictor = "predictor",
    nsim = 19,
    seed = 2026
  )

  expect_s3_class(first, "ngeo_support_test")
  expect_equal(first$simulated, second$simulated)
  expect_identical(first$permutation_domain, "common_source")
  expect_true(all(first$estimates$p_value > 0))
})

test_that("boundary sensitivity compares target identities", {
  fixture <- inference_fixture()
  result <- ngeo_boundary_sensitivity(fixture$maps)

  expect_equal(nrow(result), 2L)
  expect_equal(result$assignment_disagreement_fraction[[1L]], 0)
  expect_gt(result$assignment_disagreement_fraction[[2L]], 0)
  expect_equal(length(unique(result$support_map_hash)), 2L)
})
