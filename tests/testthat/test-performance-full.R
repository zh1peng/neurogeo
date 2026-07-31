full_grid_surface <- function(side, values = FALSE) {
  coordinates <- as.matrix(expand.grid(
    x = seq_len(side),
    y = seq_len(side)
  ))
  coordinates <- cbind(coordinates, 0)
  lower_left <- as.vector(outer(
    seq_len(side - 1L),
    (seq_len(side - 1L) - 1L) * side,
    "+"
  ))
  faces <- rbind(
    cbind(lower_left, lower_left + 1L, lower_left + side),
    cbind(
      lower_left + 1L,
      lower_left + side + 1L,
      lower_left + side
    )
  )
  map <- if (isTRUE(values)) {
    sin(coordinates[, 1L] / 12) +
      cos(coordinates[, 2L] / 15)
  } else {
    NULL
  }
  ngeo_surface(
    coordinates,
    faces,
    values = map,
    measures = if (isTRUE(values)) {
      ngeo_measure(spatial_semantics = "intensive")
    } else {
      NULL
    }
  )
}

test_that("164k surface topology and statistics remain sparse", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROGEO_FULL_PERF") != "true")

  surface <- full_grid_surface(405L, values = TRUE)
  timing <- system.time(
    weights <- ngeo_weights(
      surface,
      method = "mesh_contiguity",
      style = "W"
    )
  )
  statistic_timing <- system.time(
    result <- ngeo_moran(surface, weights)
  )

  expect_equal(nrow(weights$matrix), 164025L)
  expect_lt(length(weights$matrix@x), 7L * nrow(weights$matrix))
  expect_lt(as.numeric(object.size(weights$matrix)), 100 * 1024^2)
  expect_lt(unname(timing[["elapsed"]]), 90)
  expect_lt(unname(statistic_timing[["elapsed"]]), 10)
  expect_true(is.finite(result$estimate))
})

test_that("91k grayordinate topology stays block diagonal", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROGEO_FULL_PERF") != "true")

  left <- full_grid_surface(214L)
  right <- full_grid_surface(214L)
  n_vertex <- nrow(left$domain$elements)
  grayordinates <- ngeo_grayordinates(list(
    list(
      component_id = "left",
      kind = "surface",
      structure = "CORTEX_LEFT",
      vertex_index = seq_len(n_vertex) - 1L,
      surface_vertex_count = n_vertex,
      geometry = left
    ),
    list(
      component_id = "right",
      kind = "surface",
      structure = "CORTEX_RIGHT",
      vertex_index = seq_len(n_vertex) - 1L,
      surface_vertex_count = n_vertex,
      geometry = right
    )
  ))
  timing <- system.time(adjacency <- ngeo_adjacency(grayordinates))
  split <- n_vertex

  expect_equal(nrow(adjacency), 91592L)
  expect_equal(
    length(adjacency[seq_len(split), seq.int(split + 1L, 2L * split)]@x),
    0L
  )
  expect_lt(length(adjacency@x), 7L * nrow(adjacency))
  expect_lt(as.numeric(object.size(adjacency)), 60 * 1024^2)
  expect_lt(unname(timing[["elapsed"]]), 90)
})

test_that("32k surface diagnostics stay bounded", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROGEO_FULL_PERF") != "true")

  surface <- full_grid_surface(180L, values = TRUE)
  surface <- ngeo_set_chart(
    surface,
    surface$domain$coordinates$active[, 1:2, drop = FALSE],
    name = "grid"
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(unlink(path), add = TRUE)
  timing <- system.time(result <- plot(surface, show_edges = FALSE))
  grDevices::dev.off()

  expect_identical(result, surface)
  expect_equal(nrow(surface$domain$elements), 32400L)
  expect_lt(file.info(path)$size, 10 * 1024^2)
  expect_lt(unname(timing[["elapsed"]]), 30)
})

test_that("100k coordinate KNN remains sparse", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROGEO_FULL_PERF") != "true")
  skip_if_not_installed("dbscan")

  points <- ngeo_points(cbind(
    x = seq_len(100000L),
    y = sin(seq_len(100000L) / 100)
  ))
  timing <- system.time(
    weights <- ngeo_weights(
      points,
      method = "knn",
      k = 4L,
      symmetry = "directed",
      style = "B"
    )
  )

  expect_equal(length(weights$matrix@x), 400000L)
  expect_lt(as.numeric(object.size(weights$matrix)), 15 * 1024^2)
  expect_lt(unname(timing[["elapsed"]]), 30)
})

test_that("100k-by-1k support change stays sparse and conservative", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROGEO_FULL_PERF") != "true")

  n_source <- 100000L
  n_target <- 1000L
  source <- ngeo_points(
    cbind(x = seq_len(n_source), y = 0),
    values = cbind(
      intensity = seq_len(n_source) / n_source,
      mass = rep.int(1, n_source)
    ),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive"),
      ngeo_measure(spatial_semantics = "extensive")
    )
  )
  target <- ngeo_regions(
    data.frame(region_id = as.character(seq_len(n_target))),
    support_size = rep.int(n_source / n_target, n_target)
  )
  membership <- as.character(
    rep(seq_len(n_target), each = n_source / n_target)
  )
  timing <- system.time({
    support_map <- ngeo_support_map(
      source,
      target,
      membership,
      source_support = rep.int(1, n_source)
    )
    changed <- ngeo_change_support(source, target, support_map)
  })

  expect_equal(length(support_map$operator@x), n_source)
  expect_lt(
    as.numeric(object.size(support_map$operator)),
    10 * 1024^2
  )
  expect_equal(sum(changed$values[, "mass"]), n_source)
  expect_lt(unname(timing[["elapsed"]]), 30)
})

test_that("100k affine-grid support construction and diagnostics stay sparse", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROGEO_FULL_PERF") != "true")

  source <- ngeo_volume(
    dim = c(100, 100, 10),
    affine = diag(4),
    space = ngeo_space("performance-grid", kind = "volume"),
    index_base = "zero"
  )
  target_affine <- diag(c(2, 2, 1, 1))
  target <- ngeo_volume(
    dim = c(51, 51, 10),
    affine = target_affine,
    space = ngeo_space("performance-grid", kind = "volume"),
    index_base = "zero"
  )
  timing <- system.time({
    support_map <- ngeo_affine_grid_map(
      source,
      target,
      method = "nearest"
    )
    diagnostics <- ngeo_support_diagnostics(support_map)
  })

  expect_equal(ncol(support_map$operator), 100000L)
  expect_equal(length(support_map$operator@x), 100000L)
  expect_lt(as.numeric(object.size(support_map$operator)), 10 * 1024^2)
  expect_true(diagnostics$conservative)
  expect_true(diagnostics$complete)
  expect_lt(unname(timing[["elapsed"]]), 30)
})

test_that("100k uncertain support propagation remains sparse and bounded", {
  skip_on_cran()
  skip_if(Sys.getenv("NEUROGEO_FULL_PERF") != "true")

  n_source <- 100000L
  n_target <- 1000L
  source <- ngeo_points(
    cbind(x = seq_len(n_source), y = 0),
    values = seq_len(n_source) / n_source,
    measures = ngeo_measure(spatial_semantics = "intensive")
  )
  target <- ngeo_regions(
    data.frame(region_id = as.character(seq_len(n_target))),
    support_size = rep.int(n_source / n_target, n_target)
  )
  membership <- as.character(
    rep(seq_len(n_target), each = n_source / n_target)
  )
  map <- ngeo_support_map(
    source,
    target,
    membership,
    source_support = rep.int(1, n_source)
  )
  map$weight_variance <- methods::as(
    map$operator * 1e-4,
    "dgCMatrix"
  )
  covariance <- ngeo_support_covariance(
    source,
    variance = rep.int(0.01, n_source)
  )
  timing <- system.time({
    diagnostics <- ngeo_support_diagnostics(map)
    uncertainty <- ngeo_support_uncertainty(
      source,
      target,
      map,
      covariance
    )
  })

  expect_equal(dim(uncertainty$variance), c(n_target, 1L))
  expect_true(all(is.finite(uncertainty$variance)))
  expect_equal(
    diagnostics$summary$value[
      diagnostics$summary$metric == "uncertain_nonzero"
    ],
    n_source
  )
  expect_lt(as.numeric(object.size(map$operator)), 10 * 1024^2)
  expect_lt(unname(timing[["elapsed"]]), 30)
})
