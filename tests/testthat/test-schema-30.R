test_that("retained schema definitions cover core object families", {
  definitions <- neurogeo:::.ngeo_schema_definitions()
  corpus <- neurogeo:::.ngeo_conformance_manifest(version = "3.0")
  schema_ids <- definitions$schema_id

  expect_true(all(c(
    "ngcs/ngeo-surface",
    "ngcs/ngeo-volume",
    "ngcs/ngeo-point",
    "ngcs/ngeo-grayordinate",
    "ngcs/ngeo-parcellation",
    "ngcs/coordinate_space",
    "ngcs/transform",
    "ngcs/spatial_weights",
    "ngcs/partition",
    "ngcs/support-map",
    "ngcs/support-covariance",
    "ngcs/file-values",
    "ngcs/resampling-plan",
    "ngcs/resampling-result",
    "ngcs/time-axis",
    "ngcs/temporal-spatial_weights",
    "ngcs/spatiotemporal-spatial_weights",
    "ngcs/solver-control",
    "ngcs/iterative-solution"
  ) %in% schema_ids))
  expect_false(any(c(
    "ngcs/block-support-map",
    "ngcs/execution-plan",
    "ngcs/delayed-values"
  ) %in% schema_ids))
  expect_true(all(lengths(definitions$invariants) > 0L))
  expect_identical(corpus$corpus_version, "3.0")
})

test_that("generic validation delegates to authoritative validators", {
  source <- builder_surface(
    values = cbind(signal = c(1, 2, 3, 4)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  fixture <- diagnostic_fixture()
  objects <- list(
    source,
    source$base$coordinate_space,
    ngeo_spatial_weights(source, method = "mesh_contiguity"),
    ngeo_partition(source, c("A", "A", "B", "B")),
    ngeo_support_covariance(source, variance = rep(0.1, 4L)),
    fixture$soft
  )

  expect_true(all(vapply(
    objects,
    function(object) {
      ngeo_validate(object)
      TRUE
    },
    logical(1)
  )))
})

test_that("generic validation preserves classed invariant errors", {
  x <- builder_surface(values = cbind(signal = 1:4))
  x$values <- x$values[-1L, , drop = FALSE]

  expect_error(ngeo_validate(x), class = "ngeo_error_alignment")
})

test_that("portable object manifests are deterministic and atomic", {
  skip_if_not_installed("jsonlite")
  x <- builder_surface(
    values = cbind(signal = c(1, 2, 3, 4)),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  first <- ngeo_object_manifest(x)
  second <- ngeo_object_manifest(x)
  path <- tempfile(fileext = ".json")
  output <- write_ngeo_manifest(first, path)
  restored <- read_ngeo_manifest(path)

  expect_s3_class(first, "ngeo_object_manifest")
  expect_identical(first$canonical_sha256, second$canonical_sha256)
  expect_s3_class(output, "ngeo_atomic_output")
  expect_true(ngeo_validate_manifest(restored, x)$valid)

  corrupted <- restored
  corrupted$metadata$element_count <- 999L
  report <- ngeo_validate_manifest(corrupted)
  expect_false(report$valid)
  expect_identical(report$issues$code, "MANIFEST_HASH")
})

test_that("canonical manifest hashes ignore object property order", {
  first <- list(
    z = list(beta = 2, alpha = 1),
    a = list(value = 3)
  )
  second <- list(
    a = list(value = 3),
    z = list(alpha = 1, beta = 2)
  )
  expect_identical(
    neurogeo:::.ngeo_manifest_sha256(first),
    neurogeo:::.ngeo_manifest_sha256(second)
  )
})

test_that("4.0 removes implementation-only public APIs", {
  exports <- getNamespaceExports("neurogeo")
  removed <- c(
    "ngeo_metric",
    "ngeo_delayed_values",
    "ngeo_block_support_map",
    "ngeo_execution_plan",
    "ngeo_execute",
    "ngeo_cache",
    "ngeo_atomic_write",
    "ngeo_gwr_batched",
    "ngeo_kriging_batched",
    "ngeo_schema_registry",
    "ngeo_schema",
    "ngeo_validate_schema",
    "ngeo_migrate_schema",
    "ngeo_api_inventory",
    "ngeo_api_lifecycle",
    "ngeo_compatibility_matrix",
    "ngeo_conformance_manifest"
  )

  expect_false(any(removed %in% exports))
})
