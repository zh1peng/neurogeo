test_that("surface subset synchronizes values, IDs, coordinates, and faces", {
  coordinates <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ),
    ncol = 3,
    byrow = TRUE
  )
  x <- ngeo_surface(
    coordinates,
    matrix(
      c(1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4),
      ncol = 3,
      byrow = TRUE
    ),
    values = cbind(first = 11:14, second = 21:24)
  )
  old_ids <- ngeo_elements(x)$element_id
  y <- ngeo_subset(x, elements = c(4, 1, 2), maps = "second")

  expect_equal(as.vector(ngeo_values(y)), c(24, 21, 22))
  expect_identical(ngeo_elements(y)$element_id, old_ids[c(4, 1, 2)])
  expect_equal(y$domain$coordinates$active, coordinates[c(4, 1, 2), ])
  expect_equal(y$domain$faces, matrix(c(2, 3, 1), nrow = 1))
  expect_identical(ngeo_maps(y)$name, "second")
  expect_silent(ngeo_validate(y, "strict"))
  expect_false(identical(ngeo_domain_hash(x), ngeo_domain_hash(y)))
  expect_identical(
    tail(ngeo_provenance(y)$operations, 1)[[1]]$operation,
    "ngeo_subset"
  )
})

test_that("volume subset rebuilds the active lattice mask", {
  x <- ngeo_volume(
    values = array(1:8, dim = c(2, 2, 2)),
    dim = c(2, 2, 2),
    affine = diag(4)
  )
  ids <- ngeo_elements(x)$element_id
  y <- ngeo_subset(x, elements = ids[c(8, 1, 4)])

  expect_equal(sum(y$domain$mask), 3L)
  expect_equal(nrow(y$domain$voxel_index), 3L)
  expect_equal(as.vector(y$values), c(8, 1, 4))
  expect_silent(ngeo_validate(y, "strict"))
})

test_that("grayordinate subset rebuilds component global rows", {
  x <- ngeo_grayordinates(
    list(
      list(
        component_id = "left",
        kind = "surface",
        structure = "CORTEX_LEFT",
        vertex_index = c(0L, 1L),
        surface_vertex_count = 2L
      ),
      list(
        component_id = "volume",
        kind = "volume",
        structure = "THALAMUS_LEFT",
        voxel_index = matrix(c(0, 0, 0, 1, 1, 1), ncol = 3, byrow = TRUE),
        affine = diag(4)
      )
    ),
    values = 1:4
  )
  y <- ngeo_subset(x, elements = c(4, 1, 3))

  expect_equal(as.vector(y$values), c(4, 1, 3))
  expect_equal(y$domain$components$left$global_rows, 2L)
  expect_equal(y$domain$components$volume$global_rows, c(1L, 3L))
  expect_silent(ngeo_validate(y, "strict"))
})

test_that("region subset trims adjacency and drops excluded memberships", {
  x <- ngeo_regions(
    data.frame(region_id = c("A", "B", "C")),
    values = c(1, 2, 3),
    membership = c("A", "B", "C", "A"),
    support_size = c(2, 1, 1),
    adjacency = matrix(
      c(0, 1, 0, 1, 0, 1, 0, 1, 0),
      nrow = 3
    )
  )
  y <- ngeo_subset(x, elements = c(3, 1))

  expect_equal(dim(y$domain$adjacency), c(2L, 2L))
  expect_identical(y$domain$membership, c("A", NA, "C", "A"))
  expect_equal(as.vector(y$values), c(3, 1))
  expect_silent(ngeo_validate(y, "strict"))
})

test_that("subset rejects unknown or duplicated selections", {
  x <- ngeo_points(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))

  expect_error(
    ngeo_subset(x, elements = c(1, 1)),
    class = "ngeo_error_index"
  )
  expect_error(
    ngeo_subset(x, elements = "missing"),
    class = "ngeo_error_index"
  )
})

