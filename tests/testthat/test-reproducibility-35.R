replay_fixture_35 <- function() {
  data <- builder_surface(
    values = cbind(
      t0 = c(1, 2, 3, 4),
      t1 = c(2, 4, 6, 8),
      t2 = c(4, 8, 12, 16)
    ),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive"),
      ngeo_measure(spatial_semantics = "intensive"),
      ngeo_measure(spatial_semantics = "intensive")
    )
  )
  ngeo_set_time_axis(
    data,
    ngeo_time_axis(start = 0, step = 1, n = 3, unit = "day"),
    temporal_semantics = "instantaneous"
  )
}

test_that("logical hashes cover scientific values but not timestamps", {
  x <- replay_fixture_35()
  copy <- x
  copy$provenance$operations[[1L]]$timestamp_utc <-
    "2099-01-01T00:00:00Z"

  expect_identical(ngeo_logical_hash(x), ngeo_logical_hash(copy))
  copy$values[1L, 1L] <- copy$values[1L, 1L] + 1
  expect_false(identical(ngeo_logical_hash(x), ngeo_logical_hash(copy)))
  expect_match(ngeo_logical_hash(x), "^[0-9a-f]{64}$")
  expect_error(
    ngeo_logical_hash(
      x,
      ngeo_resource_budget(materialized_elements = 1)
    ),
    class = "ngeo_error_resource"
  )
})

test_that("provenance DAGs reject cycles, missing parents, and mutation", {
  hash <- paste(rep("a", 64), collapse = "")
  dag <- ngeo_provenance_dag(
    list(
      list(id = "source", type = "input", logical_hash = hash),
      list(id = "result", type = "operation:test", logical_hash = hash)
    ),
    list(list(from = "source", to = "result", role = "x"))
  )

  expect_s3_class(dag, "ngeo_provenance_dag")
  expect_invisible(ngeo_validate_provenance_dag(dag))
  expect_identical(ngeo_schema(dag)$version, "3.5")
  expect_error(
    ngeo_provenance_dag(
      dag$nodes,
      c(
        dag$edges,
        list(list(from = "result", to = "source", role = "x"))
      )
    ),
    class = "ngeo_error_provenance_cycle"
  )
  expect_error(
    ngeo_provenance_dag(
      dag$nodes,
      list(list(from = "missing", to = "result", role = "x"))
    ),
    class = "ngeo_error_provenance_parent"
  )
  changed <- dag
  changed$nodes[[1L]]$type <- "changed"
  expect_error(
    ngeo_validate_provenance_dag(changed),
    class = "ngeo_error_provenance_hash"
  )
})

test_that("recorded workflows replay to identical logical output", {
  x <- replay_fixture_35()
  steps <- list(
    ngeo_replay_step(
      "window",
      "ngeo_time_slice",
      c(x = "source"),
      list(index = 1:2)
    ),
    ngeo_replay_step(
      "trend",
      "ngeo_temporal_trend",
      c(x = "window")
    )
  )
  manifest <- ngeo_record_replay(
    list(source = x), steps, outputs = "trend"
  )
  replayed <- ngeo_replay(manifest, list(source = x))

  expect_s3_class(manifest, "ngeo_replay_manifest")
  expect_true(ngeo_validate_replay_manifest(manifest)$valid)
  expect_true(replayed$verified)
  expect_identical(
    ngeo_logical_hash(replayed$outputs$trend),
    manifest$output_hashes$trend
  )
  expect_length(manifest$dag$nodes, 3L)
  expect_length(manifest$dag$edges, 2L)
  expect_identical(ngeo_schema(manifest)$version, "3.5")
})

test_that("replay detects input mutation and environment drift", {
  x <- replay_fixture_35()
  manifest <- ngeo_record_replay(
    list(source = x),
    list(ngeo_replay_step(
      "trend", "ngeo_temporal_trend", c(x = "source")
    ))
  )
  changed <- x
  changed$values[1L, 1L] <- 100
  expect_error(
    ngeo_replay(manifest, list(source = changed)),
    class = "ngeo_error_replay_mutation"
  )

  drifted <- manifest
  drifted$environment$platform <- "different-platform"
  drifted$canonical_sha256 <-
    neurogeo:::.ngeo_manifest_sha256(drifted)
  expect_error(
    ngeo_replay(drifted, list(source = x), "exact"),
    class = "ngeo_error_replay_environment"
  )
  expect_error(
    ngeo_replay(drifted, list(source = x), "compatible"),
    class = "ngeo_error_replay_environment"
  )
})

test_that("replay manifests round trip without executable code", {
  x <- replay_fixture_35()
  manifest <- ngeo_record_replay(
    list(source = x),
    list(ngeo_replay_step(
      "contrast",
      "ngeo_temporal_contrast",
      c(x = "source"),
      list(
        operation = "linear",
        coefficients = c(-1, 0, 1),
        name = "change"
      )
    ))
  )
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  output <- write_ngeo_replay_manifest(manifest, path)
  restored <- read_ngeo_replay_manifest(path)

  expect_true(file.exists(output$path))
  expect_identical(
    restored$canonical_sha256,
    manifest$canonical_sha256
  )
  expect_true(ngeo_replay(restored, list(source = x))$verified)
  expect_error(
    ngeo_replay_step(
      "bad", "system", c(x = "source"), list(command = "echo")
    ),
    class = "ngeo_error_replay_step"
  )
})

test_that("artifact manifests fail before corrupt or incomplete use", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "result.txt")
  writeLines("complete", path)
  manifest <- ngeo_artifact_manifest(path, root, "result")

  expect_s3_class(manifest, "ngeo_artifact_manifest")
  expect_true(ngeo_validate_artifact_manifest(manifest, root)$valid)
  expect_identical(manifest$entries[[1L]]$path, "result.txt")
  expect_identical(ngeo_schema(manifest)$version, "3.5")

  writeLines("corrupt", path)
  report <- ngeo_validate_artifact_manifest(manifest, root)
  expect_false(report$valid)
  expect_true("ARTIFACT_CORRUPT" %in% report$issues$code)
  expect_error(
    ngeo_validate_artifact_manifest(manifest, root, "error"),
    class = "ngeo_error_artifact"
  )

  incomplete <- manifest
  incomplete$complete <- FALSE
  incomplete$canonical_sha256 <-
    neurogeo:::.ngeo_manifest_sha256(incomplete)
  expect_false(ngeo_validate_artifact_manifest(incomplete)$valid)
})

test_that("artifact manifests are portable and root confined", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "metric.tsv")
  writeLines(c("id\tvalue", "1\t2"), path)
  manifest <- ngeo_artifact_manifest(path, root, "tabular_metric")
  manifest_path <- file.path(root, "artifacts.json")
  write_ngeo_artifact_manifest(
    manifest, manifest_path, root = root
  )
  restored <- read_ngeo_artifact_manifest(manifest_path, root)

  expect_true(ngeo_validate_artifact_manifest(restored, root)$valid)
  expect_identical(
    restored$canonical_sha256,
    manifest$canonical_sha256
  )
  outside <- tempfile()
  writeLines("outside", outside)
  on.exit(unlink(outside), add = TRUE)
  expect_error(
    ngeo_artifact_manifest(outside, root),
    class = "ngeo_error_artifact_scope"
  )
})

test_that("artifact batches publish atomically with derivative-only scope", {
  directory <- tempfile()
  batch <- ngeo_write_artifact_batch(
    directory,
    c("sub-01_metric.tsv", "sub-01_metric.json"),
    list(
      function(path) writeLines("1\t2", path),
      function(path) writeLines('{"Units":"z"}', path)
    ),
    roles = c("metric", "sidecar"),
    entities = list(subject = "01", datatype = "anat")
  )
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  restored <- ngeo_read_artifact_batch(directory)

  expect_s3_class(batch, "ngeo_batch_manifest")
  expect_identical(batch$scope, "derivative_only")
  expect_true(restored$complete)
  expect_true(ngeo_validate_artifact_batch(restored, directory)$valid)
  expect_true(file.exists(file.path(directory, "manifest.json")))
  expect_true(file.exists(file.path(directory, "artifacts.json")))
  expect_identical(ngeo_schema(batch)$version, "3.5")

  unlink(file.path(directory, "artifacts.json"))
  expect_error(
    ngeo_read_artifact_batch(directory),
    class = "ngeo_error_artifact_batch"
  )
})

test_that("failed artifact batches leave no visible partial result", {
  directory <- tempfile()
  failure <- tryCatch(
    ngeo_write_artifact_batch(
      directory,
      c("first.txt", "second.txt"),
      list(
        function(path) writeLines("complete", path),
        function(path) stop("writer failure")
      )
    ),
    error = identity
  )

  expect_s3_class(failure, "error")
  expect_false(dir.exists(directory))
  expect_false(file.exists(file.path(directory, "manifest.json")))
})

test_that("3.5 schemas, environment, and API lifecycle are explicit", {
  registry <- ngeo_schema_registry()
  snapshot <- ngeo_environment_snapshot()
  lifecycle <- ngeo_api_lifecycle()
  api_35 <- lifecycle$api[lifecycle$introduced == "3.5"]

  expect_identical(registry$version, "3.5")
  expect_identical(registry$specification, "NGCS 3.5")
  expect_true(all(c(
    "ngcs/provenance-dag", "ngcs/replay-manifest",
    "ngcs/artifact-manifest", "ngcs/batch-manifest"
  ) %in% registry$schemas$schema_id))
  expect_match(snapshot$environment_sha256, "^[0-9a-f]{64}$")
  expect_identical(snapshot$specification, "NGCS 3.5")
  expect_length(api_35, 17L)
  expect_true(all(lifecycle$lifecycle[lifecycle$introduced == "3.5"] ==
                    "stable"))
})
