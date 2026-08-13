path_basis_fixture <- function(n = 6L) {
  x <- ngeo_point(
    cbind(x = seq_len(n), y = 0),
    values = cbind(signal = seq_len(n)),
    layers = data.frame(
      layer_id = "signal",
      name = "signal",
      subject_id = "s01",
      feature = "signal"
    ),
    measures = ngeo_measure(support_behavior = "intensive")
  )
  spatial_weights <- ngeo_spatial_weights(
    x,
    method = "distance_band",
    threshold = 1.01,
    style = "B"
  )
  list(x = x, spatial_weights = spatial_weights)
}

test_that("graph basis matches path analytic eigenvalues", {
  skip_if_not_installed("RSpectra")
  fixture <- path_basis_fixture(6L)
  basis <- ngeo_spatial_basis(
    fixture$x,
    fixture$spatial_weights,
    support = "identity",
    n_modes = 5L
  )
  expected <- 2 - 2 * cos(pi * (1:5) / 6)

  expect_s3_class(basis, "ngeo_spatial_basis")
  expect_equal(
    basis$components[[1L]]$eigenvalues,
    expected,
    tolerance = 1e-8
  )
  expect_lt(basis$diagnostics$max_residual, 1e-7)
  expect_lt(basis$diagnostics$max_orthogonality_error, 1e-7)
  expect_identical(basis$diagnostics$observed_zero_modes, 1L)
})

test_that("graph basis matches cycle and Cartesian-grid spectra", {
  skip_if_not_installed("RSpectra")
  cycle_n <- 6L
  cycle_adjacency <- Matrix::sparseMatrix(
    i = c(seq_len(cycle_n), seq_len(cycle_n)),
    j = c(c(2:cycle_n, 1L), c(cycle_n, seq_len(cycle_n - 1L))),
    x = 1,
    dims = c(cycle_n, cycle_n)
  )
  cycle <- ngeo_parcellation(
    data.frame(region_id = seq_len(cycle_n)),
    support_size = rep(1, cycle_n),
    adjacency = cycle_adjacency
  )
  cycle_weights <- ngeo_spatial_weights(
    cycle, method = "region_contiguity", style = "B"
  )
  cycle_basis <- ngeo_spatial_basis(
    cycle, cycle_weights, n_modes = cycle_n - 1L
  )
  cycle_expected <- sort(2 - 2 * cos(2 * pi * (1:(cycle_n - 1L)) / cycle_n))
  expect_equal(
    cycle_basis$components[[1L]]$eigenvalues,
    cycle_expected,
    tolerance = 1e-8
  )

  coordinates <- as.matrix(expand.grid(x = 1:3, y = 1:4))
  grid <- ngeo_point(coordinates)
  grid_weights <- ngeo_spatial_weights(
    grid, method = "distance_band", threshold = 1.01, style = "B"
  )
  grid_basis <- ngeo_spatial_basis(
    grid, grid_weights, support = "identity", n_modes = 11L
  )
  path3 <- 2 - 2 * cos(pi * (0:2) / 3)
  path4 <- 2 - 2 * cos(pi * (0:3) / 4)
  grid_expected <- sort(as.vector(outer(path3, path4, "+")))[-1L]
  expect_equal(
    grid_basis$components[[1L]]$eigenvalues,
    grid_expected,
    tolerance = 1e-8
  )
})

test_that("graph basis keeps disconnected components separate", {
  skip_if_not_installed("RSpectra")
  x <- ngeo_point(
    matrix(c(0, 0, 1, 0, 10, 0, 11, 0), ncol = 2L, byrow = TRUE)
  )
  spatial_weights <- ngeo_spatial_weights(
    x,
    method = "distance_band",
    threshold = 1.01,
    style = "B"
  )
  basis <- ngeo_spatial_basis(
    x, spatial_weights, support = "identity", n_modes = 1L
  )

  expect_length(basis$components, 2L)
  expect_identical(basis$diagnostics$expected_zero_modes, 2L)
  expect_identical(basis$diagnostics$observed_zero_modes, 2L)
  expect_error(
    ngeo_spatial_basis(
      x, spatial_weights, support = "identity", n_modes = 1L,
      components = "error"
    ),
    class = "ngeo_error_topology"
  )
})

test_that("surface graph basis uses one support-weighted inner product", {
  skip_if_not_installed("RSpectra")
  x <- builder_surface(values = cbind(signal = 1:4))
  spatial_weights <- ngeo_spatial_weights(x, method = "mesh_contiguity", style = "B")
  basis <- ngeo_spatial_basis(x, spatial_weights, n_modes = 2L)
  current <- basis$components[[1L]]
  gram <- crossprod(
    current$vectors,
    current$support * current$vectors
  )
  expect_equal(gram, diag(2L), tolerance = 1e-8)
  expect_identical(basis$support$type, "base")
})

test_that("graph basis rejects directed, negative, or mismatched operators", {
  skip_if_not_installed("RSpectra")
  fixture <- path_basis_fixture(6L)
  directed <- fixture$spatial_weights
  directed$raw_matrix[2L, 1L] <- 0
  directed$matrix <- directed$raw_matrix
  expect_error(
    ngeo_spatial_basis(fixture$x, directed, support = "identity"),
    class = "ngeo_error_operator"
  )

  negative <- fixture$spatial_weights
  negative$raw_matrix@x[[1L]] <- -1
  negative$matrix <- negative$raw_matrix
  expect_error(
    ngeo_spatial_basis(fixture$x, negative, support = "identity"),
    class = "ngeo_error_operator"
  )

  other <- ngeo_point(cbind(x = 1:6, y = 1))
  expect_error(
    ngeo_spatial_basis(other, fixture$spatial_weights, support = "identity"),
    class = "ngeo_error_base_mismatch"
  )
})

test_that("basis budget rejects dense output before eigensolving", {
  skip_if_not_installed("RSpectra")
  fixture <- path_basis_fixture(6L)
  expect_error(
    ngeo_spatial_basis(
      fixture$x,
      fixture$spatial_weights,
      support = "identity",
      n_modes = 5L,
      budget = ngeo_resource_budget(memory_bytes = 1)
    ),
    class = "ngeo_error_resource"
  )
})

test_that("graph symmetry detection is invariant to global weight scale", {
  fixture <- path_basis_fixture(3L)
  cycle <- Matrix::sparseMatrix(
    i = c(1L, 2L, 3L), j = c(2L, 3L, 1L), x = 1,
    dims = c(3L, 3L)
  )
  for (scale in c(1e-12, 1, 1e12)) {
    directed <- fixture$spatial_weights
    directed$raw_matrix <- neurogeo:::.ngeo_as_dgCMatrix(cycle * scale)
    directed$matrix <- directed$raw_matrix
    expect_error(
      ngeo_spatial_basis(
        fixture$x, directed, support = "identity", symmetrize = "error"
      ),
      class = "ngeo_error_operator"
    )
    basis <- ngeo_spatial_basis(
      fixture$x, directed, support = "identity", symmetrize = "mean",
      n_modes = 2L
    )
    expect_true(basis$symmetrized)
  }
})

test_that("weighted standardization preserves representable offset contrasts", {
  values <- 2 * (0:7)
  support <- seq_along(values)
  reference <- neurogeo:::.ngeo_weighted_standardize(values, support)$values
  shifted <- neurogeo:::.ngeo_weighted_standardize(
    values + 1e16, support
  )$values
  expect_equal(shifted, reference, tolerance = 1e-12)

  tiny <- neurogeo:::.ngeo_weighted_standardize(values * 1e-200, support)$values
  huge <- neurogeo:::.ngeo_weighted_standardize(values * 1e200, support)$values
  expect_equal(tiny, reference, tolerance = 1e-12)
  expect_equal(huge, reference, tolerance = 1e-12)
})
