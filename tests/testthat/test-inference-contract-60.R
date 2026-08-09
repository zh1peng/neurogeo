test_that("stable statistical results expose explicit inference semantics", {
  point <- ngeo_point(
    cbind(x = 1:5, y = 0),
    values = cbind(signal = c(1, 3, 2, 5, 4)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  spatial_weights <- ngeo_spatial_weights(point, method = "knn", k = 2L)
  global <- ngeo_moran(
    point, spatial_weights, "signal",
    permutations = 19L, seed = 42
  )
  contract <- ngeo_inference_contract(global)

  expect_s3_class(contract, "ngeo_inference_contract")
  expect_identical(contract$result_class, "ngeo_global_stat")
  expect_identical(contract$metric, global$weights_method)
  expect_match(contract$estimand, "autocorrelation")
  expect_match(contract$sampling_unit, "base elements")
  expect_match(contract$null_model, "permutation")
  expect_match(contract$support, "base elements")
  expect_match(contract$uncertainty_target, "global statistic")
  expect_true(all(c("base_hash", "layer_id") %in% names(contract$identifiers)))
})

test_that("runtime null and metric fields override declarative defaults", {
  point <- ngeo_point(
    cbind(x = 1:5, y = 0),
    values = cbind(signal = c(1, 3, 2, 5, 4)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  spatial_weights <- ngeo_spatial_weights(point, method = "knn", k = 2L)
  lisa <- ngeo_local_moran(
    point, spatial_weights, "signal",
    permutations = 9L, seed = 7, null_model = "conditional"
  )
  contract <- ngeo_inference_contract(lisa)

  expect_identical(contract$null_model, "conditional")
  expect_identical(contract$metric, attr(lisa, "weights_method"))
})

test_that("print summary and manifest share one stored contract", {
  fixture <- diagnostic_fixture()
  covariance <- ngeo_support_covariance(
    fixture$source,
    variance = rep(0.1, nrow(fixture$source$base$elements))
  )
  uncertainty <- ngeo_support_uncertainty(
    fixture$source,
    fixture$target,
    fixture$hard,
    covariance,
    layers = "outcome"
  )
  contract <- ngeo_inference_contract(uncertainty)
  summary_value <- summary(contract)
  manifest <- ngeo_object_manifest(contract)
  fields <- c(
    "estimand", "sampling_unit", "null_model", "metric", "support",
    "uncertainty_target"
  )

  expect_output(print(contract), "uncertainty target:")
  expect_identical(summary_value[fields], unclass(contract)[fields])
  expect_identical(manifest$metadata[fields], unclass(contract)[fields])
  expect_true(ngeo_validate_manifest(manifest, contract)$valid)
})

test_that("unregistered and experimental results do not acquire stable claims", {
  expect_error(
    ngeo_inference_contract(structure(list(), class = "unregistered_result")),
    class = "ngeo_error_inference_contract"
  )
  expect_error(
    ngeo_inference_contract(structure(list(), class = "ngeo_mgwr")),
    class = "ngeo_error_inference_contract"
  )
})
