test_that("support diagnostics remain sparse and report exact invariants", {
  fixture <- diagnostic_fixture()
  hard <- ngeo_support_diagnostics(fixture$hard)
  soft <- ngeo_support_diagnostics(fixture$soft)

  expect_s3_class(hard, "ngeo_support_diagnostics")
  expect_true(hard$conservative)
  expect_true(hard$complete)
  expect_equal(hard$source$entropy, rep(0, 4))
  expect_equal(soft$source$entropy, c(0, 1, 1, 0))
  expect_equal(
    hard$summary$value[hard$summary$metric == "nonzero"],
    4
  )
  expect_equal(
    soft$summary$value[soft$summary$metric == "nonzero"],
    6
  )
})

test_that("uncertain support draws are reproducible and normalized", {
  fixture <- diagnostic_fixture()
  uncertain <- fixture$soft
  uncertain$weight_variance <- methods::as(
    uncertain$operator * 0.01,
    "dgCMatrix"
  )

  first <- ngeo_support_monte_carlo(
    uncertain,
    nsim = 5,
    seed = 42
  )
  second <- ngeo_support_monte_carlo(
    uncertain,
    nsim = 5,
    seed = 42
  )

  expect_s3_class(first, "ngeo_support_ensemble")
  expect_equal(
    first$samples[[1L]]$operator,
    second$samples[[1L]]$operator
  )
  expect_true(all(vapply(first$samples, function(map) {
    all(abs(Matrix::colSums(map$operator) - 1) < 1e-10)
  }, logical(1))))
})

test_that("support sensitivity binds alternative operator hashes", {
  fixture <- diagnostic_fixture()
  result <- ngeo_support_sensitivity(
    fixture$source,
    fixture$target,
    list(hard = fixture$hard, soft = fixture$soft)
  )

  expect_s3_class(result, "ngeo_support_sensitivity")
  expect_equal(length(result$support_map_hashes), 2L)
  expect_equal(
    result$summary$maximum_absolute_difference[
      result$summary$support_map == 1L
    ],
    c(0, 0)
  )
  expect_true(any(
    result$summary$maximum_absolute_difference[
      result$summary$support_map == 2L
    ] > 0
  ))
})

test_that("support map plots return their input invisibly", {
  fixture <- diagnostic_fixture()
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  expect_identical(
    plot(fixture$soft, type = "entropy"),
    fixture$soft
  )
})
