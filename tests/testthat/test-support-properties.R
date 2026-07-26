test_that("random complete probabilistic maps satisfy sparse properties", {
  set.seed(20210726)
  for (iteration in seq_len(25L)) {
    n_source <- sample(5:30, 1L)
    n_target <- sample(2:6, 1L)
    source <- ngeo_points(
      cbind(seq_len(n_source), 0),
      space = ngeo_space("property", kind = "unknown")
    )
    probability <- matrix(
      stats::rexp(n_source * n_target),
      nrow = n_source
    )
    probability <- probability / rowSums(probability)
    colnames(probability) <- paste0("R", seq_len(n_target))
    map <- ngeo_probabilistic_atlas_map(
      source,
      probability,
      source_support = rep(1, n_source)
    )

    expect_silent(ngeo_validate_support_map(map))
    expect_identical(map$coverage, "complete")
    expect_equal(Matrix::colSums(map$operator), rep(1, n_source))
    expect_true(inherits(map$operator, "dgCMatrix"))
    expect_true(all(ngeo_support_entropy(map) >= 0))
    expect_true(all(ngeo_support_entropy(map) <= 1))
  }
})

test_that("support validator rejects corrupted identity and support fields", {
  fixture <- diagnostic_fixture()
  map <- fixture$hard

  wrong_direction <- map
  wrong_direction$direction <- "source_by_target"
  expect_error(
    ngeo_validate_support_map(wrong_direction),
    class = "ngeo_error_support_map"
  )

  wrong_support <- map
  wrong_support$target_support[[1L]] <-
    wrong_support$target_support[[1L]] + 1
  expect_error(
    ngeo_validate_support_map(wrong_support),
    class = "ngeo_error_support"
  )

  expect_error(
    ngeo_validate_support_map(map, tolerance = NA_real_),
    class = "ngeo_error_argument"
  )
})
