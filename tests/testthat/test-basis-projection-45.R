projection_fixture <- function(delayed = FALSE) {
  n <- 6L
  subjects <- rep(sprintf("s%02d", 1:3), each = 2L)
  features <- rep(c("thickness", "myelin"), 3L)
  values <- outer(seq_len(n), seq_along(subjects), function(i, j) {
    sin(i * pi / 7) + j / 5 + (j %% 2) * cos(i * pi / 4)
  })
  colnames(values) <- paste(subjects, features, sep = "_")
  stored <- values
  if (isTRUE(delayed)) {
    stored <- neurogeo:::.ngeo_delayed_values(
      function(rows, columns) values[rows, columns, drop = FALSE],
      dim(values),
      map_names = colnames(values),
      source = "projection-test-callback"
    )
  }
  x <- ngeo_points(
    cbind(x = seq_len(n), y = 0),
    values = stored,
    maps = data.frame(
      map_id = paste0("map_", seq_along(subjects)),
      name = colnames(values),
      subject_id = subjects,
      feature = features,
      stringsAsFactors = FALSE
    ),
    measures = do.call(rbind, lapply(features, function(feature) {
      ngeo_measure(
        spatial_semantics = "intensive",
        units = if (feature == "thickness") "mm" else "ratio"
      )
    }))
  )
  weights <- ngeo_weights(
    x, method = "distance_band", threshold = 1.01, style = "B"
  )
  list(x = x, weights = weights, values = values)
}

test_that("full nonconstant basis recovers centered maps", {
  skip_if_not_installed("RSpectra")
  fixture <- projection_fixture()
  index <- ngeo_validate_layers(fixture$x, complete = "error")
  basis <- ngeo_spatial_basis(
    fixture$x, fixture$weights, support = "identity", n_modes = 5L
  )
  projected <- ngeo_basis_project(
    fixture$x,
    basis,
    index = index,
    bands = list(all = 1:5),
    summaries = c(
      "absolute_energy", "relative_energy", "roughness",
      "retained_variance", "residual_energy"
    ),
    chunk_rows = 2L,
    chunk_maps = 2L
  )

  expect_s3_class(projected, "ngeo_subject_features")
  retained <- projected$values[, projected$endpoints$estimand ==
    "retained_variance", drop = FALSE]
  residual <- projected$values[, projected$endpoints$estimand ==
    "residual_energy", drop = FALSE]
  expect_equal(retained, matrix(1, nrow(retained), ncol(retained)),
               tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(residual, matrix(0, nrow(residual), ncol(residual)),
               tolerance = 1e-8, ignore_attr = TRUE)
})

test_that("delayed and in-memory basis projection are identical", {
  skip_if_not_installed("RSpectra")
  dense <- projection_fixture(FALSE)
  delayed <- projection_fixture(TRUE)
  dense_index <- ngeo_validate_layers(dense$x, complete = "error")
  delayed_index <- ngeo_validate_layers(delayed$x, complete = "error")
  basis <- ngeo_spatial_basis(
    dense$x, dense$weights, support = "identity", n_modes = 4L
  )
  first <- ngeo_basis_project(
    dense$x, basis, dense_index,
    bands = list(low = 1:2, high = 3:4),
    summaries = c("coefficients", "absolute_energy", "roughness"),
    chunk_rows = 2L, chunk_maps = 2L
  )
  second <- ngeo_basis_project(
    delayed$x, basis, delayed_index,
    bands = list(low = 1:2, high = 3:4),
    summaries = c("coefficients", "absolute_energy", "roughness"),
    chunk_rows = 3L, chunk_maps = 1L
  )
  expect_identical(first$endpoints, second$endpoints)
  expect_equal(first$values, second$values, tolerance = 1e-10)
})

test_that("projection energy is invariant to basis-vector signs", {
  skip_if_not_installed("RSpectra")
  fixture <- projection_fixture()
  index <- ngeo_validate_layers(fixture$x, complete = "error")
  basis <- ngeo_spatial_basis(
    fixture$x, fixture$weights, support = "identity", n_modes = 4L
  )
  flipped <- basis
  flipped$components[[1L]]$vectors[, c(1L, 3L)] <-
    -flipped$components[[1L]]$vectors[, c(1L, 3L)]
  first <- ngeo_basis_project(
    fixture$x, basis, index,
    bands = list(all = 1:4), summaries = "absolute_energy"
  )
  second <- ngeo_basis_project(
    fixture$x, flipped, index,
    bands = list(all = 1:4), summaries = "absolute_energy"
  )
  expect_equal(first$values, second$values, tolerance = 1e-12)
})

test_that("projection rejects domain changes and split eigenspaces", {
  skip_if_not_installed("RSpectra")
  fixture <- projection_fixture()
  index <- ngeo_validate_layers(fixture$x, complete = "error")
  basis <- ngeo_spatial_basis(
    fixture$x, fixture$weights, support = "identity", n_modes = 4L
  )
  changed <- fixture$x
  changed$domain$space <- ngeo_space("changed", kind = "surface")
  expect_error(
    ngeo_basis_project(changed, basis, index),
    class = "ngeo_error_domain_mismatch"
  )

  split <- basis
  split$components[[1L]]$degenerate_cluster[1:2] <- 1L
  expect_error(
    ngeo_basis_project(
      fixture$x, split, index,
      bands = list(first = 1L, second = 2:4),
      summaries = "absolute_energy"
    ),
    class = "ngeo_error_band"
  )
})
