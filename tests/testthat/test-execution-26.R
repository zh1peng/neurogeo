execution_fixture <- function(delayed = FALSE) {
  coordinates <- cbind(x = 0:11, y = 0)
  predictor <- coordinates[, 1L]
  values <- cbind(
    intensive = 1 + 2 * predictor,
    extensive = seq_len(12),
    count = rep(c(1, 2), 6),
    category = rep(c(1, 1, 2), 4),
    predictor = predictor
  )
  colnames(values) <- c(
    "intensive", "extensive", "count", "category", "predictor"
  )
  values_block <- if (delayed) {
    ngeo_delayed_values(
      function(rows, columns) values[rows, columns, drop = FALSE],
      dim(values),
      map_names = colnames(values),
      source = "test callback"
    )
  } else {
    values
  }
  semantics <- c(
    "intensive", "extensive", "count", "categorical", "intensive"
  )
  x <- ngeo_points(
    coordinates,
    values = values_block,
    measures = do.call(rbind, lapply(semantics, function(value) {
      ngeo_measure(
        value_type = if (value == "categorical") "label" else "continuous",
        spatial_semantics = value
      )
    }))
  )
  target <- ngeo_regions(
    data.frame(region_id = c("A", "B", "C")),
    support_size = rep(NA_real_, 3),
    space = x$domain$space
  )
  operator <- Matrix::sparseMatrix(
    i = rep(1:3, each = 4),
    j = 1:12,
    x = 1,
    dims = c(3, 12)
  )
  map <- ngeo_support_map(
    x, target, operator,
    source_support = rep.int(1, 12)
  )
  list(
    x = x,
    target = target,
    map = map,
    block = ngeo_block_support_map(map, 1L, 3L)
  )
}

test_that("true block execution preserves every measurement semantic", {
  fixture <- execution_fixture(delayed = TRUE)
  direct_source <- fixture$x
  direct_source$values <- as.matrix(direct_source$values)
  direct <- ngeo_change_support(
    direct_source,
    fixture$target,
    fixture$map
  )
  block <- ngeo_change_support_block(
    fixture$x,
    fixture$target,
    fixture$block
  )
  diagnostics <- ngeo_block_diagnostics(fixture$block)

  expect_equal(block$values, direct$values)
  expect_false(
    block$provenance$operations[[1L]]$parameters$materialized_operator
  )
  expect_false(diagnostics$materialized_operator)
  expect_equal(diagnostics$nonzero, length(fixture$map$operator@x))
  expect_length(diagnostics$source_unmapped, 0L)
})

test_that("block variance matches monolithic propagation", {
  fixture <- execution_fixture()
  selected <- c("intensive", "extensive", "count")
  variance <- matrix(
    seq(0.1, 0.3, length.out = 12 * 3),
    nrow = 12,
    ncol = 3
  )
  direct <- ngeo_support_variance(
    fixture$x,
    fixture$target,
    fixture$map,
    variance,
    maps = selected
  )
  block <- ngeo_block_variance(
    fixture$x,
    fixture$target,
    fixture$block,
    variance,
    maps = selected
  )

  expect_equal(block, direct, tolerance = 1e-12)

  uncertain <- ngeo_support_map(
    fixture$x,
    fixture$target,
    fixture$map$operator,
    source_support = fixture$map$source_support,
    weight_variance = fixture$map$operator * 0.01
  )
  expect_error(
    ngeo_block_variance(
      fixture$x,
      fixture$target,
      ngeo_block_support_map(uncertain, 1L, 3L),
      variance,
      maps = selected
    ),
    class = "ngeo_error_uncertainty"
  )
})

test_that("block composition matches monolithic sparse composition", {
  fixture <- execution_fixture()
  final <- ngeo_regions(
    data.frame(region_id = c("X", "Y")),
    support_size = c(NA_real_, NA_real_),
    space = fixture$target$domain$space
  )
  second <- ngeo_support_map(
    fixture$target,
    final,
    Matrix::sparseMatrix(
      i = c(1, 1, 2),
      j = 1:3,
      x = 1,
      dims = c(2, 3)
    ),
    source_support = fixture$map$target_support
  )
  first_block <- ngeo_block_support_map(fixture$map, 1L, 3L)
  second_block <- ngeo_block_support_map(second, 1L, 1L)
  composed <- ngeo_compose_block_support_map(first_block, second_block)
  direct <- ngeo_compose_support_map(fixture$map, second)

  expect_equal(
    ngeo_materialize_support_map(composed)$operator,
    direct$operator
  )
})

test_that("streaming summaries covariance and regression match memory", {
  fixture <- execution_fixture(delayed = TRUE)
  memory <- as.matrix(fixture$x$values)
  summary <- ngeo_stream_summary(fixture$x, chunk_size = 5L)
  covariance <- ngeo_stream_covariance(fixture$x, chunk_size = 4L)
  fit <- ngeo_stream_lm(
    fixture$x,
    "intensive",
    "predictor",
    chunk_size = 3L
  )

  expect_equal(summary$mean, unname(colMeans(memory)))
  expect_equal(summary$variance, unname(apply(memory, 2L, stats::var)))
  expect_equal(covariance, stats::cov(memory))
  expect_equal(
    fit$coefficients,
    c(`(Intercept)` = 1, predictor = 2),
    tolerance = 1e-10
  )
})

test_that("streaming Moran matches the in-memory statistic", {
  fixture <- execution_fixture(delayed = TRUE)
  weights <- ngeo_weights(
    fixture$x,
    method = "distance_band",
    threshold = 1.01,
    style = "W"
  )
  streamed <- ngeo_stream_moran(
    fixture$x, weights, "intensive", chunk_size = 4L
  )
  memory <- fixture$x
  memory$values <- as.matrix(memory$values)
  reference <- ngeo_moran(memory, weights, "intensive")

  expect_equal(streamed$estimate, reference$estimate, tolerance = 1e-12)
  expect_false(streamed$materialized_values)
})

test_that("resource budgets reject work before execution", {
  fixture <- execution_fixture()
  expect_error(
    ngeo_change_support_block(
      fixture$x,
      fixture$target,
      fixture$block,
      budget = ngeo_resource_budget(blocks = 1)
    ),
    class = "ngeo_error_resource"
  )
  expect_error(
    ngeo_materialize_support_map(
      fixture$block,
      budget = ngeo_resource_budget(materialized_elements = 10)
    ),
    class = "ngeo_error_resource"
  )
  expect_error(
    ngeo_execution_plan(
      "too-many",
      as.list(1:3),
      function(task, index) task,
      budget = ngeo_resource_budget(blocks = 2)
    ),
    class = "ngeo_error_resource"
  )
})

test_that("execution plans resume deterministically and reject mutation", {
  skip_if_not_installed("jsonlite")
  checkpoint <- tempfile(fileext = ".json")
  tasks <- as.list(1:4)
  plan <- ngeo_execution_plan(
    "square",
    tasks,
    function(task, index) task^2,
    identity = list(domain = "fixed"),
    checkpoint = checkpoint
  )
  partial <- ngeo_execute(plan, stop_after = 2L)
  resumed <- ngeo_execute(plan)

  expect_false(partial$complete)
  expect_true(resumed$complete)
  expect_equal(unlist(resumed$results), (1:4)^2)

  changed <- ngeo_execution_plan(
    "square",
    tasks,
    function(task, index) task^2,
    identity = list(domain = "changed"),
    checkpoint = checkpoint
  )
  expect_error(
    ngeo_execute(changed),
    class = "ngeo_error_cache_mismatch"
  )

  changed_executor <- ngeo_execution_plan(
    "square",
    tasks,
    function(task, index) task^3,
    identity = list(domain = "fixed"),
    checkpoint = checkpoint
  )
  expect_false(identical(plan$plan_hash, changed_executor$plan_hash))
  expect_error(
    ngeo_execute(changed_executor),
    class = "ngeo_error_cache_mismatch"
  )
})

test_that("content cache and atomic writes are auditable", {
  cache <- ngeo_cache(tempfile("ngeo-cache-"))
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    42
  }
  first <- ngeo_cache_compute(
    cache,
    list(domain = "a", semantics = "intensive"),
    compute
  )
  second <- ngeo_cache_compute(
    cache,
    list(domain = "a", semantics = "intensive"),
    compute
  )
  changed_compute <- ngeo_cache_compute(
    cache,
    list(domain = "a", semantics = "intensive"),
    function() {
      calls <<- calls + 1L
      99
    }
  )
  third <- ngeo_cache_compute(
    cache,
    list(domain = "a", semantics = "extensive"),
    function() {
      calls <<- calls + 1L
      7
    }
  )
  path <- tempfile(fileext = ".txt")
  written <- ngeo_atomic_write(
    path,
    function(temporary) writeLines("complete", temporary)
  )

  expect_false(first$hit)
  expect_true(second$hit)
  expect_false(changed_compute$hit)
  expect_false(third$hit)
  expect_identical(second$value, 42)
  expect_identical(changed_compute$value, 99)
  expect_identical(calls, 3L)
  expect_true(file.exists(written$path))
  expect_length(written$sha256, 1L)

  failed <- tempfile(fileext = ".txt")
  expect_error(
    ngeo_atomic_write(failed, function(temporary) stop("interrupted"))
  )
  expect_false(file.exists(failed))
})

test_that("randomized block results equal monolithic results", {
  set.seed(2601)
  for (iteration in seq_len(20L)) {
    n <- sample(12:30, 1L)
    target_n <- sample(3:6, 1L)
    values <- stats::rnorm(n)
    x <- ngeo_points(
      cbind(x = seq_len(n), y = 0),
      values = cbind(signal = values),
      measures = ngeo_measure(spatial_semantics = "intensive")
    )
    target <- ngeo_regions(
      data.frame(region_id = paste0("r", seq_len(target_n))),
      support_size = rep(NA_real_, target_n),
      space = x$domain$space
    )
    membership <- sample(seq_len(target_n), n, replace = TRUE)
    map <- ngeo_support_map(
      x,
      target,
      Matrix::sparseMatrix(
        i = membership,
        j = seq_len(n),
        x = 1,
        dims = c(target_n, n)
      ),
      source_support = stats::runif(n, 0.5, 2)
    )
    block <- ngeo_block_support_map(
      map,
      row_block_size = sample(1:3, 1L),
      source_block_size = sample(2:7, 1L)
    )
    expect_equal(
      ngeo_change_support_block(x, target, block)$values,
      ngeo_change_support(x, target, map)$values,
      tolerance = 1e-12
    )
  }
})
