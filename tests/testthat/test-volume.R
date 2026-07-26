test_that("volume fixture preserves mask, source indices, and affine volume", {
  fixture <- read_fixture("volume-3x3x3.json")
  lattice_dim <- as.integer(unlist(fixture$dim))
  affine <- rows_to_matrix(fixture$affine)
  mask <- array(
    as.logical(unlist(fixture$mask_linear_r_order)),
    dim = lattice_dim
  )
  values <- array(
    as.numeric(unlist(fixture$values_linear_r_order)),
    dim = lattice_dim
  )

  x <- ngeo_volume(
    values = values,
    dim = lattice_dim,
    affine = affine,
    mask = mask,
    space = ngeo_space("fixture_volume", kind = "volume"),
    index_base = "zero"
  )

  expect_s3_class(x, "ngeo_volume")
  expect_silent(ngeo_validate(x, "strict"))
  expect_identical(ngeo_domain_type(x), "volume")
  expect_equal(
    x$domain$source_voxel_index,
    rows_to_matrix(
      fixture$expected$active_voxel_ijk_zero_based,
      "integer"
    )
  )
  expect_equal(
    as.vector(x$values),
    unlist(fixture$expected$active_values)
  )
  expect_equal(
    ngeo_voxel_volume(x),
    fixture$expected$voxel_volume,
    tolerance = fixture$tolerance$absolute
  )
  expect_true(all(ngeo_capabilities(x)[c(
    "voxel_affine", "voxel_volume", "adjacency"
  )]))
})

test_that("volume does not infer a mask from zero values", {
  x <- ngeo_volume(
    values = array(0, dim = c(2, 2, 2)),
    dim = c(2, 2, 2),
    affine = diag(4)
  )

  expect_equal(nrow(x$domain$elements), 8L)
  expect_equal(nrow(x$values), 8L)
})

test_that("volume rejects invalid geometry and alignment", {
  expect_error(
    ngeo_volume(dim = c(2, 2, 2), affine = matrix(0, 4, 4)),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_volume(
      values = 1:3,
      dim = c(2, 2, 2),
      affine = diag(4)
    ),
    class = "ngeo_error_alignment"
  )
})

