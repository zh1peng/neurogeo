test_that("free schedules are unique, reproducible, and identity-free", {
  ids <- paste0("s", 1:7)
  first <- ngeo_exchangeability(
    ids, scheme = "free", permutations = 40L, seed = 42L
  )
  second <- ngeo_exchangeability(
    ids, scheme = "free", permutations = 40L, seed = 42L
  )
  expect_s3_class(first, "ngeo_exchangeability")
  expect_identical(first$schedule, second$schedule)
  expect_identical(first$schedule_hash, second$schedule_hash)
  expect_identical(colnames(first$schedule), ids)
  expect_equal(nrow(unique(first$schedule)), nrow(first$schedule))
  expect_false(any(apply(
    first$schedule, 1L, identical, y = seq_along(ids)
  )))
  expect_true(all(apply(first$schedule, 1L, function(row) {
    identical(unname(sort(row)), seq_along(ids))
  })))
})

test_that("within-block schedules never cross blocks", {
  ids <- paste0("s", 1:8)
  blocks <- rep(c("site-a", "site-b"), each = 4L)
  schedule <- ngeo_exchangeability(
    ids, scheme = "within_block", blocks = blocks,
    permutations = 50L, seed = 7L
  )
  for (row in seq_len(nrow(schedule$schedule))) {
    expect_identical(blocks[schedule$schedule[row, ]], blocks)
  }
  expect_identical(schedule$blocks, stats::setNames(blocks, ids))
  expect_error(
    ngeo_exchangeability(
      ids, scheme = "within_block", blocks = rep("one", length(ids)),
      permutations = 10L
    ),
    class = "ngeo_error_exchangeability"
  )
})

test_that("small sign-flip families are enumerated exactly", {
  ids <- paste0("pair", 1:3)
  schedule <- ngeo_exchangeability(
    ids, scheme = "sign_flip", permutations = 99L, seed = 1L
  )
  expect_equal(dim(schedule$schedule), c(7L, 3L))
  expect_true(all(schedule$schedule %in% c(-1L, 1L)))
  expect_false(any(apply(schedule$schedule, 1L, function(row) all(row == 1L))))
  expect_identical(schedule$status, "exact")
  expect_equal(schedule$unique_transformations, 7)
})

test_that("user schedules validate alignment, identity, duplicates, and blocks", {
  ids <- paste0("s", 1:4)
  supplied <- rbind(c(2, 1, 3, 4), c(1, 2, 4, 3))
  colnames(supplied) <- ids
  schedule <- ngeo_exchangeability(
    ids, scheme = "user", schedule = supplied
  )
  expect_equal(schedule$schedule, supplied)
  expect_identical(schedule$transformation, "permutation")

  bad_names <- supplied
  colnames(bad_names) <- rev(ids)
  expect_error(
    ngeo_exchangeability(ids, scheme = "user", schedule = bad_names),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_exchangeability(
      ids, scheme = "user",
      schedule = rbind(supplied[1L, ], supplied[1L, ])
    ),
    class = "ngeo_error_exchangeability"
  )
  identity <- rbind(seq_along(ids), supplied[1L, ])
  colnames(identity) <- ids
  expect_error(
    ngeo_exchangeability(ids, scheme = "user", schedule = identity),
    class = "ngeo_error_exchangeability"
  )
  expect_error(
    ngeo_exchangeability(
      ids, scheme = "user", schedule = supplied,
      blocks = c("a", "a", "b", "b")
    ),
    NA
  )
  crossing <- supplied
  crossing[1L, ] <- c(3, 2, 1, 4)
  expect_error(
    ngeo_exchangeability(
      ids, scheme = "user", schedule = crossing,
      blocks = c("a", "a", "b", "b")
    ),
    class = "ngeo_error_exchangeability"
  )
})

test_that("schedule materialization obeys resource budgets", {
  expect_error(
    ngeo_exchangeability(
      paste0("s", 1:10), permutations = 100L, seed = 1L,
      budget = ngeo_resource_budget(materialized_elements = 50)
    ),
    class = "ngeo_error_resource"
  )
})
