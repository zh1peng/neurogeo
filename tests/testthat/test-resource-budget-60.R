test_that("each resource-budget dimension has a positive and negative gate", {
  unlimited <- ngeo_resource_budget()
  for (field in c(
    "memory_bytes", "elapsed_seconds", "blocks", "materialized_elements"
  )) {
    expect_identical(unlimited[[field]], Inf)
  }

  budget <- ngeo_resource_budget(
    memory_bytes = 16,
    elapsed_seconds = 1,
    blocks = 2,
    materialized_elements = 4
  )
  context <- neurogeo:::.ngeo_budget_context(budget)
  inherited <- neurogeo:::.ngeo_budget_context(context)
  expect_identical(inherited$started_elapsed, context$started_elapsed)
  expect_identical(inherited$budget, context$budget)
  expect_invisible(neurogeo:::.ngeo_budget_assert(context, "memory_bytes", 16))
  expect_invisible(neurogeo:::.ngeo_budget_assert(context, "blocks", 2))
  expect_invisible(neurogeo:::.ngeo_budget_assert(
    context, "materialized_elements", 4
  ))
  expect_error(
    neurogeo:::.ngeo_budget_assert(context, "memory_bytes", 17),
    class = "ngeo_error_resource"
  )
  expect_error(
    neurogeo:::.ngeo_budget_assert(context, "blocks", 3),
    class = "ngeo_error_resource"
  )
  expect_error(
    neurogeo:::.ngeo_budget_assert(context, "materialized_elements", 5),
    class = "ngeo_error_resource"
  )

  expired <- neurogeo:::.ngeo_budget_context(ngeo_resource_budget(
    elapsed_seconds = 0.001
  ))
  expired$started_elapsed <- expired$started_elapsed - 1
  condition <- tryCatch(
    neurogeo:::.ngeo_budget_checkpoint(expired),
    error = identity
  )
  expect_s3_class(condition, "ngeo_error_resource_deadline")
  expect_identical(condition$code, "NGEO_ERROR_RESOURCE_DEADLINE")
  expect_identical(condition$field, "elapsed_seconds")

  inherited_expired <- neurogeo:::.ngeo_budget_context(expired)
  expect_error(
    neurogeo:::.ngeo_budget_checkpoint(inherited_expired),
    class = "ngeo_error_resource_deadline"
  )
})

test_that("change of support enforces its scheduled-block budget", {
  source <- ngeo_point(
    matrix(c(0, 0, 1, 0), ncol = 2L, byrow = TRUE),
    values = cbind(a = 1:2, b = 3:4),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  atlas <- ngeo_atlas_map(
    source, c("A", "A"), source_support = rep(1, 2)
  )
  expect_error(
    aggregate_to(
      source, atlas$target, atlas,
      budget = ngeo_resource_budget(blocks = 1)
    ),
    class = "ngeo_error_resource"
  )
})
