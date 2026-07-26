resampling_volume <- function(
    space,
    affine = diag(4),
    values = NULL,
    semantics = "intensive") {
  measures <- if (is.null(values)) {
    NULL
  } else {
    n_map <- if (is.null(dim(values)) ||
      length(dim(values)) == 3L) 1L else dim(values)[[4L]]
    do.call(rbind, rep(
      list(ngeo_measure(spatial_semantics = semantics)),
      n_map
    ))
  }
  ngeo_volume(
    values = values,
    dim = c(2, 2, 2),
    affine = affine,
    measures = measures,
    space = space,
    index_base = "zero"
  )
}

resampling_surface <- function(space, translation = 0, values = NULL) {
  coordinates <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      1, 1, 0,
      0, 1, 0
    ),
    ncol = 3L,
    byrow = TRUE
  )
  coordinates[, 1L] <- coordinates[, 1L] + translation
  ngeo_surface(
    coordinates,
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE),
    values = values,
    measures = if (is.null(values)) NULL else
      ngeo_measure(spatial_semantics = "intensive"),
    space = space
  )
}

resampling_path <- function(
    source_space,
    target_space = source_space,
    matrix = diag(4),
    lossy = FALSE,
    type = "affine") {
  registry <- ngeo_space_registry(
    if (identical(
      ngeo_space_hash(source_space),
      ngeo_space_hash(target_space)
    )) {
      list(source_space)
    } else {
      list(source_space, target_space)
    }
  )
  if (identical(
    ngeo_space_hash(source_space),
    ngeo_space_hash(target_space)
  )) {
    graph <- ngeo_transform_graph(registry)
  } else {
    transform <- ngeo_transform(
      source_space,
      target_space,
      type,
      method = "supplied test transform",
      interpolation = if (identical(type, "affine")) "none" else "linear",
      parameters = if (identical(type, "affine")) {
        list(matrix = matrix)
      } else {
        list(reference = "supplied-warp.nii.gz")
      }
    )
    graph <- ngeo_transform_graph(
      registry,
      transform,
      edge_ids = "supplied",
      lossy = lossy
    )
  }
  ngeo_transform_path(
    graph,
    ngeo_space_hash(source_space),
    ngeo_space_hash(target_space)
  )
}

test_that("identity volume resampling agrees for every 3.2 method", {
  space <- ngeo_space("grid", kind = "volume")
  source <- resampling_volume(
    space,
    values = array(1:8, dim = c(2, 2, 2))
  )
  target <- resampling_volume(space)
  path <- resampling_path(space)

  for (method in c("nearest", "linear", "overlap")) {
    plan <- ngeo_resampling_plan(
      source, target, path, method = method
    )
    map <- ngeo_build_resampling_map(plan, authorize = TRUE)
    result <- ngeo_resample(plan, authorize = TRUE)

    expect_equal(as.matrix(map$operator), diag(8))
    expect_equal(result$data$values, source$values)
    expect_true(result$diagnostics$conservative)
    expect_false(result$diagnostics$registration_estimated)
    expect_identical(
      result$provenance$path$path_hash, path$path_hash
    )
    expect_identical(
      result$data$provenance$resampling$joint_hash,
      result$diagnostics$joint_hash
    )
  }
})

test_that("supplied affine paths bridge volume grids exactly", {
  source_space <- ngeo_space("native", kind = "volume")
  target_space <- ngeo_space("standard", kind = "volume")
  translated <- diag(4)
  translated[1L, 4L] <- 1
  source <- resampling_volume(
    source_space,
    values = array(seq_len(8), dim = c(2, 2, 2))
  )
  target <- resampling_volume(target_space, affine = translated)
  path <- resampling_path(
    source_space, target_space, translated
  )

  for (method in c("nearest", "linear", "overlap")) {
    plan <- ngeo_resampling_plan(
      source, target, path, method = method
    )
    result <- ngeo_resample(plan, authorize = TRUE)
    expect_equal(
      as.matrix(result$support_map$operator), diag(8)
    )
    expect_equal(result$data$values, source$values)
    expect_identical(
      result$support_map$provenance$resampling$path$tokens,
      "supplied"
    )
  }
})

test_that("surface nearest and barycentric consume an affine path", {
  source_space <- ngeo_space(
    "native-surface", kind = "surface",
    structure = "CORTEX_LEFT"
  )
  target_space <- ngeo_space(
    "standard-surface", kind = "surface",
    structure = "CORTEX_LEFT"
  )
  transform <- diag(4)
  transform[1L, 4L] <- 2
  source <- resampling_surface(
    source_space, values = cbind(signal = 1:4)
  )
  target <- resampling_surface(target_space, translation = 2)
  path <- resampling_path(
    source_space, target_space, transform
  )

  for (method in c("nearest", "barycentric")) {
    result <- ngeo_resample(
      ngeo_resampling_plan(
        source, target, path, method = method
      ),
      authorize = TRUE
    )
    expect_equal(
      as.matrix(result$support_map$operator), diag(4)
    )
    expect_equal(result$data$values, source$values)
  }
})

test_that("authorization and path boundaries are explicit", {
  space <- ngeo_space("grid", kind = "volume")
  source <- resampling_volume(
    space,
    values = array(seq_len(8), dim = c(2, 2, 2))
  )
  target <- resampling_volume(space)
  plan <- ngeo_resampling_plan(
    source, target, resampling_path(space), method = "nearest"
  )

  expect_error(
    ngeo_build_resampling_map(plan),
    class = "ngeo_error_authorization"
  )
  expect_error(
    ngeo_resample(plan),
    class = "ngeo_error_authorization"
  )

  point_space <- ngeo_space("points")
  points <- ngeo_points(
    cbind(x = 1:2, y = 1:2),
    values = 1:2,
    space = point_space
  )
  expect_error(
    ngeo_resampling_plan(
      points, points, resampling_path(point_space)
    ),
    class = "ngeo_error_resampling_method"
  )

  target_space <- ngeo_space("other", kind = "volume")
  lossy <- resampling_path(
    space, target_space, lossy = TRUE
  )
  expect_error(
    ngeo_resampling_plan(
      source, resampling_volume(target_space), lossy
    ),
    class = "ngeo_error_resampling_path"
  )
  warp <- resampling_path(
    space, target_space, type = "warp"
  )
  expect_error(
    ngeo_resampling_plan(
      source, resampling_volume(target_space), warp
    ),
    class = "ngeo_error_resampling_path"
  )
  changed_path <- resampling_path(space, target_space)
  changed_path$composed$parameters$matrix[1L, 4L] <- 99
  expect_error(
    ngeo_resampling_plan(
      source, resampling_volume(target_space), changed_path
    ),
    class = "ngeo_error_transform_path_mutation"
  )
})

test_that("coverage, missing support, and conservation policies differ", {
  space <- ngeo_space("grid", kind = "volume")
  source <- resampling_volume(
    space,
    values = array(rep(1, 8), dim = c(2, 2, 2)),
    semantics = "extensive"
  )
  shifted <- diag(4)
  shifted[1L, 4L] <- 0.25
  target <- resampling_volume(space, affine = shifted)
  path <- resampling_path(space)

  complete <- ngeo_resampling_plan(
    source, target, path, method = "linear",
    coverage = "error"
  )
  expect_error(
    ngeo_build_resampling_map(complete, authorize = TRUE),
    class = "ngeo_error_coverage"
  )

  strict <- ngeo_resampling_plan(
    source, target, path, method = "linear",
    coverage = "drop", missing = "drop",
    conservation = "strict"
  )
  expect_error(
    ngeo_resample(strict, authorize = TRUE),
    class = "ngeo_error_conservation"
  )

  normalized <- ngeo_resampling_plan(
    source, target, path, method = "linear",
    coverage = "drop", missing = "drop",
    conservation = "normalize"
  )
  result <- ngeo_resample(normalized, authorize = TRUE)
  expect_equal(sum(result$data$values), 8)
  expect_gt(nrow(result$diagnostics$issues), 0L)
})

test_that("uncertainty policy requires exactly its declared inputs", {
  space <- ngeo_space("grid", kind = "volume")
  source <- resampling_volume(
    space,
    values = array(seq_len(8), dim = c(2, 2, 2))
  )
  target <- resampling_volume(space)
  path <- resampling_path(space)
  value_plan <- ngeo_resampling_plan(
    source, target, path, method = "nearest",
    uncertainty = "value"
  )

  expect_error(
    ngeo_resample(value_plan, authorize = TRUE),
    class = "ngeo_error_uncertainty"
  )
  value_result <- ngeo_resample(
    value_plan,
    value_variance = rep(0.04, 8),
    authorize = TRUE
  )
  expect_equal(
    as.numeric(value_result$variance), rep(0.04, 8)
  )

  weight_variance <- Matrix::sparseMatrix(
    i = seq_len(8), j = seq_len(8),
    x = rep(0.01, 8), dims = c(8, 8)
  )
  joint_plan <- ngeo_resampling_plan(
    source, target, path, method = "nearest",
    uncertainty = "value_and_mapping",
    weight_variance = weight_variance
  )
  joint <- ngeo_resample(
    joint_plan,
    value_variance = rep(0.04, 8),
    authorize = TRUE
  )
  expect_true(joint$diagnostics$mapping_variance_declared)
  expect_equal(as.numeric(joint$variance), rep(0.04, 8))

  expect_error(
    ngeo_resampling_plan(
      source, target, path,
      uncertainty = "none",
      weight_variance = weight_variance
    ),
    class = "ngeo_error_uncertainty"
  )
})

test_that("plans reject mutation and resource overruns", {
  space <- ngeo_space("grid", kind = "volume")
  source <- resampling_volume(
    space,
    values = array(seq_len(8), dim = c(2, 2, 2))
  )
  target <- resampling_volume(space)
  path <- resampling_path(space)
  plan <- ngeo_resampling_plan(
    source, target, path, method = "nearest"
  )
  changed <- plan
  changed$method <- "linear"
  expect_error(
    ngeo_validate_resampling_plan(changed),
    class = "ngeo_error_resampling_plan_mutation"
  )

  constrained <- ngeo_resampling_plan(
    source, target, path, method = "linear",
    budget = ngeo_resource_budget(
      memory_bytes = 100,
      materialized_elements = 10
    )
  )
  expect_error(
    ngeo_build_resampling_map(constrained, authorize = TRUE),
    class = "ngeo_error_resource"
  )
})

test_that("result provenance, schema manifests, and atomic output verify", {
  skip_if_not_installed("jsonlite")
  space <- ngeo_space("grid", kind = "volume")
  source <- resampling_volume(
    space,
    values = array(seq_len(8), dim = c(2, 2, 2))
  )
  target <- resampling_volume(space)
  plan <- ngeo_resampling_plan(
    source, target, resampling_path(space), method = "nearest"
  )
  output <- tempfile(fileext = ".txt")
  result <- ngeo_resample(
    plan,
    authorize = TRUE,
    output_path = output,
    writer = function(data, path) {
      writeLines(as.character(data$values[, 1L]), path)
    }
  )

  expect_s3_class(result, "ngeo_resampling_result")
  expect_s3_class(result$output, "ngeo_atomic_output")
  expect_true(file.exists(output))
  plan_manifest <- ngeo_object_manifest(plan)
  result_manifest <- ngeo_object_manifest(result)
  expect_identical(plan_manifest$object_schema, "ngcs/resampling-plan")
  expect_identical(result_manifest$object_schema, "ngcs/resampling-result")
  expect_invisible(ngeo_validate(plan))
  expect_invisible(ngeo_validate(result))
  expect_identical(plan_manifest$specification, "NGCS 3.2")
  expect_true(ngeo_validate_manifest(plan_manifest, plan)$valid)
  expect_true(ngeo_validate_manifest(result_manifest, result)$valid)

  another <- ngeo_resampling_plan(
    source, target, resampling_path(space), method = "linear"
  )
  expect_error(
    ngeo_resampling_diagnostics(
      another, result$support_map
    ),
    class = "ngeo_error_resampling_plan"
  )
})
