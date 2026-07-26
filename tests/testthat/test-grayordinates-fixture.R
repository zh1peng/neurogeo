test_that("grayordinate fixture fixes ordered block semantics", {
  fixture <- read_fixture("grayordinates-hybrid.json")

  expect_identical(fixture$spec_version, "1.0")
  expect_equal(length(fixture$components), 3L)
  expect_equal(
    unlist(lapply(fixture$components, function(x) length(x$global_rows))),
    c(2L, 2L, 2L)
  )
  expect_equal(nrow(rows_to_matrix(fixture$values)), fixture$n_element)
  expect_identical(
    fixture$expected$default_topology,
    "block_diagonal"
  )
  expect_length(fixture$expected$cross_component_edges, 0L)
  expect_false(
    fixture$expected$capabilities_without_surface_geometry$geodesic
  )
  expect_true(
    fixture$expected$capabilities_without_surface_geometry$voxel_affine
  )
})
