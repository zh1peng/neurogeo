test_that("surface topology matches the tetrahedron conformance fixture", {
  fixture <- read_fixture("surface-tetrahedron.json")
  x <- ngeo_surface(
    rows_to_matrix(fixture$coordinates),
    rows_to_matrix(fixture$faces, "integer"),
    index_base = "zero"
  )
  expect_no_warning(adjacency <- ngeo_adjacency(x))

  expect_s4_class(adjacency, "dgCMatrix")
  expect_equal(length(adjacency@x), 12L)
  expect_equal(as.numeric(Matrix::rowSums(adjacency)), rep(3, 4))
  expect_equal(ngeo_components(x), rep(1L, 4))
  expect_equal(
    ngeo_support_size(x),
    unlist(fixture$expected$barycentric_vertex_area),
    tolerance = fixture$tolerance$absolute
  )
})

test_that("volume adjacency is active-mask limited", {
  fixture <- read_fixture("volume-3x3x3.json")
  lattice_dim <- as.integer(unlist(fixture$dim))
  mask <- array(
    as.logical(unlist(fixture$mask_linear_r_order)),
    dim = lattice_dim
  )
  x <- ngeo_volume(
    dim = lattice_dim,
    affine = rows_to_matrix(fixture$affine),
    mask = mask,
    index_base = "zero"
  )
  adjacency <- ngeo_adjacency(x, connectivity = 6L)

  expect_equal(length(adjacency@x), 6L)
  expect_equal(as.numeric(Matrix::rowSums(adjacency)), c(1, 2, 2, 1))
  expect_equal(ngeo_components(adjacency), rep(1L, 4))
  expect_equal(ngeo_support_size(x), rep(8, 4))
})

test_that("grayordinate topology remains block diagonal", {
  geometry <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3), nrow = 1)
  )
  x <- ngeo_grayordinate(
    list(
      list(
        component_id = "left",
        kind = "surface",
        structure = "CORTEX_LEFT",
        vertex_index = c(0L, 1L),
        surface_vertex_count = 3L,
        geometry = geometry
      ),
      list(
        component_id = "right",
        kind = "surface",
        structure = "CORTEX_RIGHT",
        vertex_index = c(1L, 2L),
        surface_vertex_count = 3L,
        geometry = geometry
      )
    )
  )
  expect_no_warning(adjacency <- ngeo_adjacency(x))

  expect_equal(length(adjacency@x), 4L)
  expect_equal(as.matrix(adjacency)[1:2, 3:4], matrix(0, 2, 2))
  expect_equal(ngeo_components(adjacency), c(1L, 1L, 2L, 2L))
})

test_that("explicit Euclidean and graph distances are correct", {
  point <- ngeo_point(
    matrix(c(0, 0, 3, 4, 0, 4), ncol = 2, byrow = TRUE)
  )
  distance <- ngeo_distance(point, from = 1, to = c(2, 3), distance_method = "euclidean")
  expect_equal(as.vector(distance), c(5, 4))

  surface <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3), nrow = 1)
  )
  expect_equal(
    as.vector(ngeo_distance(
      surface,
      from = 2,
      to = 3,
      distance_method = "edge_geodesic"
    )),
    sqrt(2)
  )
  expect_equal(
    as.vector(ngeo_distance(surface, from = 2, to = 3, distance_method = "hops")),
    1
  )
})

test_that("distance requests have an explicit size guard", {
  point <- ngeo_point(matrix(seq_len(20), ncol = 2))
  old <- options(neurogeo.max_distance_pairs = 20)
  on.exit(options(old), add = TRUE)

  expect_error(
    ngeo_distance(point, from = 1:3),
    class = "ngeo_error_dense_distance"
  )
})

test_that("contiguity spatial_weights retain raw sparse spatial_weights and normalization", {
  surface <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3), nrow = 1)
  )
  spatial_weights <- ngeo_spatial_weights(
    surface,
    method = "mesh_contiguity",
    style = "W"
  )

  expect_s3_class(spatial_weights, "ngeo_spatial_weights")
  expect_s4_class(spatial_weights$matrix, "dgCMatrix")
  expect_equal(as.numeric(Matrix::rowSums(spatial_weights$matrix)), rep(1, 3))
  expect_equal(length(spatial_weights$raw_matrix@x), 6L)
  expect_identical(spatial_weights$base_hash, base_hash(surface))
  expect_identical(spatial_weights$distance_method, "hops")
  expect_equal(spatial_weights$diagnostics$n_component, 1L)
})

test_that("coordinate KNN, distance-band, and kernels stay sparse", {
  point <- ngeo_point(
    matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE)
  )
  knn <- ngeo_spatial_weights(point, method = "knn", k = 1, style = "B")
  band <- ngeo_spatial_weights(
    point,
    method = "distance_band",
    threshold = 1.1,
    style = "B"
  )
  gaussian <- ngeo_spatial_weights(
    point,
    method = "gaussian",
    threshold = 1.1,
    bandwidth = 1,
    style = "none"
  )

  expect_s4_class(knn$matrix, "dgCMatrix")
  expect_true(length(knn$matrix@x) < 16L)
  expect_equal(length(band$matrix@x), 6L)
  expect_true(all(gaussian$matrix@x > 0 & gaussian$matrix@x < 1))
})

test_that("KD-tree queries scale past the exact-neighbor guard", {
  skip_if_not_installed("dbscan")
  point <- ngeo_point(cbind(x = seq_len(6000), y = 0))
  old <- options(neurogeo.max_exact_neighbors = 100L)
  on.exit(options(old), add = TRUE)

  knn <- ngeo_spatial_weights(
    point,
    method = "knn",
    k = 2L,
    style = "B",
    symmetry = "directed"
  )
  radius <- ngeo_spatial_weights(
    point,
    method = "radius",
    threshold = 1.01,
    style = "B"
  )

  expect_equal(length(knn$matrix@x), 12000L)
  expect_lt(length(radius$matrix@x), 2.1 * nrow(radius$matrix))
  expect_identical(radius$method, "radius")
})

test_that("spatial_weights convert to igraph and spdep without dense intermediates", {
  surface <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3), nrow = 1)
  )
  spatial_weights <- ngeo_spatial_weights(surface, style = "W")

  if (requireNamespace("igraph", quietly = TRUE)) {
    graph <- as_igraph(spatial_weights)
    expect_equal(igraph::vcount(graph), 3L)
    expect_equal(igraph::ecount(graph), 3L)
  }
  if (requireNamespace("spdep", quietly = TRUE)) {
    nb <- as_spdep_nb(spatial_weights)
    listw <- as_spdep_listw(spatial_weights)
    expect_s3_class(nb, "nb")
    expect_s3_class(listw, "listw")
    expect_equal(length(nb), 3L)
  }
})

test_that("distance-based spatial_weights execute the declared surface distance_method", {
  coordinates <- rbind(
    c(0, 0, 0),
    c(10, 0, 0),
    c(10, 1, 0),
    c(0.1, 0, 0.1)
  )
  surface <- ngeo_surface(
    coordinates,
    rbind(c(1, 2, 3), c(2, 3, 4)),
    values = cbind(signal = 1:4)
  )
  euclidean <- ngeo_spatial_weights(
    surface,
    method = "distance_band",
    threshold = 1,
    style = "B",
    distance_method = "euclidean"
  )
  geodesic <- ngeo_spatial_weights(
    surface,
    method = "distance_band",
    threshold = 1,
    style = "B",
    distance_method = "edge_geodesic"
  )

  expect_equal(as.numeric(ngeo_distance(
    surface, 1, 4, distance_method = "euclidean"
  )), sqrt(0.02))
  expect_gt(as.numeric(ngeo_distance(
    surface, 1, 4, distance_method = "edge_geodesic"
  )), 19)
  expect_equal(as.matrix(euclidean$matrix)[1, 4], 1)
  expect_equal(as.matrix(geodesic$matrix)[1, 4], 0)
  expect_error(
    ngeo_spatial_weights(
      ngeo_point(matrix(1:6, ncol = 2)),
      method = "distance_band",
      threshold = 2,
      distance_method = "edge_geodesic"
    ),
    class = "ngeo_error_capability"
  )
})

test_that("metrics are controlled names without parameter objects", {
  point <- ngeo_point(cbind(x = 0:1, y = 0))
  expect_equal(
    as.numeric(ngeo_distance(point, 1, 2, "euclidean")),
    1
  )
  expect_error(
    ngeo_distance(point, 1, 2, list(name = "euclidean")),
    class = "ngeo_error_metric"
  )
})
