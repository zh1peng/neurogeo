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
    neurogeo:::.ngeo_delayed_values(
      function(rows, columns) values[rows, columns, drop = FALSE],
      dim(values),
      layer_names = colnames(values),
      source = "test callback"
    )
  } else {
    values
  }
  semantics <- c(
    "intensive", "extensive", "count", "categorical", "intensive"
  )
  x <- ngeo_point(
    coordinates,
    values = values_block,
    measures = do.call(rbind, lapply(semantics, function(value) {
      ngeo_measure(
        value_type = if (value == "categorical") "label" else "continuous",
        support_behavior = value
      )
    }))
  )
  target <- ngeo_parcellation(
    data.frame(region_id = c("A", "B", "C")),
    support_size = rep(NA_real_, 3),
    coordinate_space = x$base$coordinate_space
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
    map = map
  )
}

test_that("sparse change of support preserves every measurement semantic", {
  fixture <- execution_fixture(delayed = TRUE)
  direct_source <- fixture$x
  direct_source$values <- as.matrix(direct_source$values)
  reference <- aggregate_to(
    direct_source,
    fixture$target,
    fixture$map
  )
  changed <- aggregate_to(
    fixture$x,
    fixture$target,
    fixture$map
  )

  expect_equal(changed$values, reference$values)
  expect_s4_class(fixture$map$operator, "dgCMatrix")
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
  spatial_weights <- ngeo_spatial_weights(
    fixture$x,
    method = "distance_band",
    threshold = 1.01,
    style = "W"
  )
  streamed <- ngeo_stream_moran(
    fixture$x, spatial_weights, "intensive", chunk_size = 4L
  )
  memory <- fixture$x
  memory$values <- as.matrix(memory$values)
  reference <- ngeo_moran(memory, spatial_weights, "intensive")

  expect_equal(streamed$estimate, reference$estimate, tolerance = 1e-12)
  expect_false(streamed$materialized_values)
})

test_that("resource budgets reject work before execution", {
  fixture <- execution_fixture()
  expect_error(
    aggregate_to(
      fixture$x,
      fixture$target,
      fixture$map,
      budget = ngeo_resource_budget(materialized_elements = 1)
    ),
    class = "ngeo_error_resource"
  )
})

test_that("randomized sparse support results preserve intensive values", {
  set.seed(2601)
  for (iteration in seq_len(20L)) {
    n <- sample(12:30, 1L)
    target_n <- sample(3:6, 1L)
    values <- stats::rnorm(n)
    x <- ngeo_point(
      cbind(x = seq_len(n), y = 0),
      values = cbind(signal = values),
      measures = ngeo_measure(support_behavior = "intensive")
    )
    target <- ngeo_parcellation(
      data.frame(region_id = paste0("r", seq_len(target_n))),
      support_size = rep(NA_real_, target_n),
      coordinate_space = x$base$coordinate_space
    )
    membership <- sample(seq_len(target_n), n, replace = TRUE)
    source_support <- stats::runif(n, 0.5, 2)
    map <- ngeo_support_map(
      x,
      target,
      Matrix::sparseMatrix(
        i = membership,
        j = seq_len(n),
        x = 1,
        dims = c(target_n, n)
      ),
      source_support = source_support
    )
    result <- aggregate_to(x, target, map)
    expected <- vapply(
      seq_len(target_n),
      function(region) {
        selected <- membership == region
        if (!any(selected)) {
          NA_real_
        } else {
          stats::weighted.mean(
            values[selected], source_support[selected]
          )
        }
      },
      numeric(1)
    )
    expect_equal(result$values[, 1L], expected, tolerance = 1e-12)
  }
})
