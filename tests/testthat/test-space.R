test_that("coordinate_space is explicit and defaults to unknown", {
  coordinate_space <- ngeo_coordinate_space()

  expect_s3_class(coordinate_space, "ngeo_coordinate_space")
  expect_identical(coordinate_space$space_id, "unknown")
  expect_identical(coordinate_space$kind, "unknown")
  expect_output(print(coordinate_space), "<ngeo_coordinate_space> unknown")
})

test_that("coordinate_space rejects malformed scalar fields", {
  expect_error(ngeo_coordinate_space(space_id = character()), class = "ngeo_error_argument")
  expect_error(ngeo_coordinate_space(unit = NA_character_), class = "ngeo_error_argument")
})
