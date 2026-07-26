test_that("space is explicit and defaults to unknown", {
  space <- ngeo_space()

  expect_s3_class(space, "ngeo_space")
  expect_identical(space$space_id, "unknown")
  expect_identical(space$kind, "unknown")
  expect_output(print(space), "<ngeo_space> unknown")
})

test_that("space rejects malformed scalar fields", {
  expect_error(ngeo_space(space_id = character()), class = "ngeo_error_argument")
  expect_error(ngeo_space(units = NA_character_), class = "ngeo_error_argument")
})
