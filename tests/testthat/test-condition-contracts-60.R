capture_ngeo_error <- function(expression) {
  tryCatch(expression, error = identity)
}

expect_ngeo_condition_fields <- function(condition) {
  expect_s3_class(condition, "ngeo_error")
  expect_type(condition$code, "character")
  expect_length(condition$code, 1L)
  expect_match(condition$code, "^NGEO_ERROR(?:_[A-Z0-9]+)*$")
  expect_type(condition$field, "character")
  expect_length(condition$field, 1L)
  expect_true(nzchar(condition$field))
  expect_type(condition$hint, "character")
  expect_length(condition$hint, 1L)
  expect_true(nzchar(condition$hint))
}

test_that("all aborts receive structured condition fields", {
  condition <- capture_ngeo_error(ngeo_measure(unit = NA_character_))
  expect_ngeo_condition_fields(condition)
  expect_identical(condition$code, "NGEO_ERROR_ARGUMENT_SCALAR_CHARACTER")
  expect_identical(condition$field, "unit")
})

test_that("backend failures identify the package and next step", {
  condition <- capture_ngeo_error(neurogeo:::.ngeo_require(
    "definitelyNotANeurogeoPackage",
    "condition contract audit"
  ))
  expect_ngeo_condition_fields(condition)
  expect_s3_class(condition, "ngeo_error_backend")
  expect_identical(condition$code, "NGEO_ERROR_BACKEND_MISSING")
  expect_identical(condition$field, "definitelyNotANeurogeoPackage")
  expect_match(condition$hint, "install.packages", fixed = TRUE)
})

test_that("layer and measure failures give task-specific recovery", {
  x <- ngeo_point(
    matrix(c(0, 0, 1, 0), ncol = 2L, byrow = TRUE),
    values = cbind(signal = c(1, 2))
  )
  layer_condition <- capture_ngeo_error(ngeo_layer_index(
    x,
    unit = "subject_id",
    feature = "feature"
  ))
  expect_ngeo_condition_fields(layer_condition)
  expect_identical(layer_condition$code, "NGEO_ERROR_LAYER_METADATA_MISSING")
  expect_identical(layer_condition$field, "layers")

  measure_condition <- capture_ngeo_error(ngeo_update_measure(
    x,
    "absent",
    unit = "mm"
  ))
  expect_ngeo_condition_fields(measure_condition)
  expect_identical(measure_condition$code, "NGEO_ERROR_MEASURE_UNKNOWN")
  expect_identical(measure_condition$field, "measure_id")
})

test_that("condition message snapshots remain user-facing", {
  snapshot_path <- system.file(
    "spec", "condition-message-snapshots-6.0.csv",
    package = "neurogeo", mustWork = TRUE
  )
  snapshot <- utils::read.csv(
    snapshot_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_false(any(grepl(
    "agent|milestone|phase[ -]?[0-9]|[45]\\.[0-9]",
    paste(snapshot$message, snapshot$hint),
    ignore.case = TRUE
  )))
})

test_that("reader failures identify the path recovery contract", {
  condition <- capture_ngeo_error(neurogeo:::.ngeo_backend_read(
    "NIfTI",
    "missing.nii",
    function() stop("synthetic reader failure")
  ))
  expect_ngeo_condition_fields(condition)
  expect_identical(condition$code, "NGEO_ERROR_IO_READ")
  expect_identical(condition$field, "path")
  expect_match(condition$hint, "exists", fixed = TRUE)
})
