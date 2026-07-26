test_that("support map exchange round-trips operator and uncertainty", {
  skip_if_not_installed("jsonlite")
  fixture <- diagnostic_fixture()
  map <- fixture$soft
  map$weight_variance <- methods::as(map$operator * 0.01, "dgCMatrix")
  prefix <- tempfile("support-map-")
  on.exit(unlink(paste0(prefix, c(
    ".mtx", ".json", ".variance.mtx"
  ))), add = TRUE)

  paths <- write_ngeo_support_map(map, prefix)
  restored <- read_ngeo_support_map(prefix)

  expect_true(all(file.exists(paths)))
  expect_s3_class(restored, "ngeo_support_map")
  expect_equal(restored$operator, map$operator)
  expect_equal(restored$weight_variance, map$weight_variance)
  expect_equal(restored$source_support, map$source_support)
  expect_identical(
    ngeo_support_map_hash(restored),
    ngeo_support_map_hash(map)
  )
})

test_that("support map exchange verifies missing and altered inputs", {
  skip_if_not_installed("jsonlite")
  fixture <- diagnostic_fixture()
  prefix <- tempfile("support-map-")
  on.exit(unlink(paste0(prefix, c(".mtx", ".json"))), add = TRUE)

  write_ngeo_support_map(fixture$hard, prefix)
  unlink(paste0(prefix, ".mtx"))
  expect_error(
    read_ngeo_support_map(prefix),
    class = "ngeo_error_io"
  )
})

test_that("support map exchange rejects sidecar path traversal", {
  skip_if_not_installed("jsonlite")
  fixture <- diagnostic_fixture()
  prefix <- tempfile("support-map-")
  on.exit(unlink(paste0(prefix, c(".mtx", ".json"))), add = TRUE)
  write_ngeo_support_map(fixture$hard, prefix)
  metadata <- jsonlite::fromJSON(
    paste0(prefix, ".json"),
    simplifyVector = FALSE
  )
  metadata$operator_file <- "../outside.mtx"
  jsonlite::write_json(
    metadata,
    paste0(prefix, ".json"),
    auto_unbox = TRUE
  )

  expect_error(
    read_ngeo_support_map(prefix),
    class = "ngeo_error_io"
  )
})
