test_that("portable manifests cover spaces, transforms, support, and time", {
  source_space <- ngeo_coordinate_space("source", kind = "unknown")
  target_space <- ngeo_coordinate_space("target", kind = "unknown")
  transform <- ngeo_transform(
    source_space,
    target_space,
    type = "affine",
    parameters = list(matrix = diag(4))
  )
  transform_manifest <- ngeo_object_manifest(transform)
  expect_identical(transform_manifest$object_schema, "ngcs/transform")
  expect_identical(
    transform_manifest$metadata$source_space_sha256,
    ngeo_coordinate_space_hash(source_space)
  )
  expect_identical(
    transform_manifest$metadata$target_space_sha256,
    ngeo_coordinate_space_hash(target_space)
  )

  space_manifest <- ngeo_object_manifest(source_space)
  expect_identical(space_manifest$metadata$space_id, "source")

  fixture <- diagnostic_fixture()
  support_manifest <- ngeo_object_manifest(fixture$soft)
  expect_identical(support_manifest$metadata$direction, "target_by_source")
  expect_equal(support_manifest$metadata$dimensions, c(2L, 4L))

  axis <- ngeo_time_axis(time = c(0, 1, 2), unit = "second")
  temporal <- ngeo_temporal_weights(axis, method = "adjacent")
  point <- ngeo_point(
    matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)
  )
  spatial <- ngeo_spatial_weights(point, method = "knn", k = 1)
  joint <- ngeo_spatiotemporal_weights(spatial, temporal)

  expect_identical(ngeo_object_manifest(axis)$metadata$n_time, 3L)
  expect_equal(
    ngeo_object_manifest(temporal)$metadata$dimensions,
    c(3L, 3L)
  )
  expect_false(
    ngeo_object_manifest(joint)$metadata$matrix_materialized
  )
})

test_that("manifest validation reports structure, schema, hash, and object mismatch", {
  first <- ngeo_point(
    matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
    values = cbind(signal = c(1, 2))
  )
  second <- ngeo_point(
    matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE),
    values = cbind(signal = c(1, 2))
  )
  manifest <- ngeo_object_manifest(first)

  incomplete <- ngeo_validate_manifest(list(schema = "bad"))
  expect_false(incomplete$valid)
  expect_identical(incomplete$issues$code, "MANIFEST_STRUCTURE")

  unsupported <- manifest
  unsupported$object_schema_version <- "999"
  unsupported$specification <- "NGCS 999"
  unsupported$canonical_sha256 <- neurogeo:::.ngeo_manifest_sha256(unsupported)
  schema_report <- ngeo_validate_manifest(unsupported)
  expect_false(schema_report$valid)
  expect_true("MANIFEST_SCHEMA" %in% schema_report$issues$code)

  changed_hash <- manifest
  changed_hash$metadata$element_count <- 99L
  hash_report <- ngeo_validate_manifest(changed_hash)
  expect_true("MANIFEST_HASH" %in% hash_report$issues$code)

  mismatch <- ngeo_validate_manifest(manifest, second)
  expect_true("MANIFEST_OBJECT_MISMATCH" %in% mismatch$issues$code)

  expect_error(
    ngeo_validate_manifest(changed_hash, mode = "error"),
    class = "ngeo_error_manifest"
  )
})

test_that("manifest JSON reader fails before exposing invalid content", {
  testthat::skip_if_not_installed("jsonlite")
  missing <- tempfile(fileext = ".json")
  expect_error(read_ngeo_manifest(missing), class = "ngeo_error_io")

  malformed <- tempfile(fileext = ".json")
  writeLines("{not-json", malformed)
  on.exit(unlink(malformed), add = TRUE)
  expect_error(read_ngeo_manifest(malformed), class = "ngeo_error_io")
})
