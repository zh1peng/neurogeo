test_that("NGCS 3.0 registry covers core object families", {
  registry <- ngeo_schema_registry()
  corpus <- ngeo_conformance_manifest(version = "3.0")

  expect_s3_class(registry, "ngeo_schema_registry_30")
  expect_identical(registry$version, "3.5")
  expect_gte(nrow(registry$schemas), 33L)
  expect_true(all(c(
    "ngcs/ngeo-surface",
    "ngcs/ngeo-volume",
    "ngcs/ngeo-points",
    "ngcs/ngeo-grayordinates",
    "ngcs/ngeo-regions",
    "ngcs/space",
    "ngcs/transform",
    "ngcs/weights",
    "ngcs/partition",
    "ngcs/support-map",
    "ngcs/block-support-map",
    "ngcs/support-covariance",
    "ngcs/file-values",
    "ngcs/resampling-plan",
    "ngcs/resampling-result",
    "ngcs/time-axis",
    "ngcs/temporal-weights",
    "ngcs/spatiotemporal-weights",
    "ngcs/solver-control",
    "ngcs/iterative-solution",
    "ngcs/logdet-estimate",
    "ngcs/iterative-spatial-regression",
    "ngcs/iterative-car",
    "ngcs/delayed-values",
    "ngcs/space-registry",
    "ngcs/transform-graph",
    "ngcs/execution-plan"
  ) %in% registry$schemas$schema_id))
  expect_true(all(lengths(registry$schemas$invariants) > 0L))
  expect_identical(corpus$corpus_version, "3.0")
  expect_length(corpus$specifications, 16L)
})

test_that("schema validation delegates to authoritative validators", {
  source <- builder_surface(
    values = cbind(signal = c(1, 2, 3, 4)),
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  weights <- ngeo_weights(source, method = "mesh_contiguity")
  partition <- ngeo_partition(source, c("A", "A", "B", "B"))
  covariance <- ngeo_support_covariance(
    source, variance = rep(0.1, 4L)
  )
  fixture <- diagnostic_fixture()
  block <- ngeo_block_support_map(
    fixture$soft, row_block_size = 1L, source_block_size = 2L
  )

  objects <- list(
    source,
    source$domain$space,
    weights,
    partition,
    covariance,
    fixture$soft,
    block
  )
  reports <- lapply(objects, ngeo_validate_schema)

  expect_true(all(vapply(reports, `[[`, logical(1), "valid")))
  expect_true(all(vapply(
    reports, inherits, logical(1), what = "ngeo_validation_report"
  )))
})

test_that("schema validation returns deterministic classed issues", {
  x <- builder_surface(values = cbind(signal = 1:4))
  x$values <- x$values[-1L, , drop = FALSE]

  first <- ngeo_validate_schema(x)
  second <- ngeo_validate_schema(x)

  expect_false(first$valid)
  expect_identical(first$issues, second$issues)
  expect_identical(first$issues$severity, "error")
  expect_identical(
    first$issues$condition_class, "ngeo_error_alignment"
  )
  condition <- tryCatch(
    {
      ngeo_validate_schema(x, mode = "error")
      NULL
    },
    error = identity
  )
  expect_s3_class(condition, "ngeo_error_schema_validation")
  expect_s3_class(condition$report, "ngeo_validation_report")
})

test_that("portable object manifests are deterministic and atomic", {
  skip_if_not_installed("jsonlite")
  x <- builder_surface(
    values = cbind(signal = c(1, 2, 3, 4)),
    measures = ngeo_measure(spatial_semantics = "intensive")
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

test_that("schema migration and API lifecycle retain earlier APIs", {
  x <- builder_surface(values = cbind(signal = 1:4))
  migrated <- ngeo_migrate_schema(x)
  migration <- attr(migrated, "ngeo_schema_migration")
  lifecycle <- ngeo_api_lifecycle()
  old_inventory <- ngeo_api_inventory()

  expect_identical(migration$target_version, "3.0")
  expect_true(migration$valid)
  expect_true(all(lifecycle$lifecycle == "stable"))
  expect_true(all(lifecycle$planned_action == "retain"))
  expect_true(all(old_inventory$status_2_9[
    old_inventory$api %in% c("ngeo_surface", "ngeo_support_map")
  ] == "stable"))
  expect_true(all(old_inventory$status_2_9[
    old_inventory$api %in% c(
      "ngeo_schema_registry", "ngeo_object_manifest"
    )
  ] == "not_exported"))
})
