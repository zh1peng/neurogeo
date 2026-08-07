test_that("surface nearest and barycentric identity layers are sparse", {
  source <- builder_surface(values = 1:4)
  target <- builder_surface()

  nearest <- ngeo_surface_nearest_map(source, target)
  barycentric <- ngeo_surface_barycentric_map(source, target)

  expect_s3_class(nearest, "ngeo_support_map")
  expect_s3_class(barycentric, "ngeo_support_map")
  expect_equal(as.matrix(nearest$operator), diag(4))
  expect_equal(as.matrix(barycentric$operator), diag(4))
  expect_identical(nearest$type, "crisp")
  expect_identical(barycentric$type, "probabilistic")
  expect_identical(nearest$coverage, "complete")
  expect_equal(Matrix::colSums(barycentric$operator), rep(1, 4))
})

test_that("surface builders require declared registration and respect masks", {
  source <- builder_surface()
  target <- builder_surface()
  target$base$coordinate_space$space_id <- "another-coordinate_space"

  expect_error(
    ngeo_surface_nearest_map(source, target),
    class = "ngeo_error_coordinate_space"
  )
  registered <- ngeo_surface_nearest_map(
    source,
    target,
    registration = "known-sphere"
  )
  expect_identical(registered$coverage, "complete")

  target$base$geometry$mask[[1L]] <- FALSE
  partial <- ngeo_surface_nearest_map(
    source,
    target,
    registration = "known-sphere",
    max_distance = 0
  )
  expect_identical(partial$coverage, "partial")
  expect_equal(Matrix::colSums(partial$operator)[[1L]], 0)
})

test_that("surface builders exercise bounded search engines", {
  skip_if_not_installed("dbscan")
  source <- builder_surface()
  target <- builder_surface()
  old <- options(neurogeo.max_exact_mapping_pairs = 1)
  on.exit(options(old), add = TRUE)

  nearest <- ngeo_surface_nearest_map(source, target)
  barycentric <- ngeo_surface_barycentric_map(
    source,
    target,
    candidate_faces = 2
  )

  expect_equal(as.matrix(nearest$operator), diag(4))
  expect_equal(as.matrix(barycentric$operator), diag(4))
  expect_identical(
    nearest$history$operations[[1L]]$parameters$search_engine,
    "dbscan"
  )
})

builder_volume <- function(affine = diag(4), index_base = "zero") {
  ngeo_volume(
    dim = c(2, 2, 2),
    affine = affine,
    coordinate_space = ngeo_coordinate_space("voxel-grid", kind = "volume"),
    index_base = index_base
  )
}

test_that("known affine identity grids reproduce exact operators", {
  source <- builder_volume()
  target <- builder_volume()

  nearest <- ngeo_affine_grid_map(source, target, "nearest")
  trilinear <- ngeo_affine_grid_map(source, target, "trilinear")
  overlap <- ngeo_voxel_overlap_map(source, target)

  expect_equal(as.matrix(nearest$operator), diag(8))
  expect_equal(as.matrix(trilinear$operator), diag(8))
  expect_equal(as.matrix(overlap$operator), diag(8))
  expect_true(all(vapply(
    list(nearest, trilinear, overlap),
    function(map) identical(map$coverage, "complete"),
    logical(1)
  )))
})

test_that("affine builders make outside coverage explicit", {
  source <- builder_volume()
  shifted_affine <- diag(4)
  shifted_affine[1L, 4L] <- 1
  target <- builder_volume(shifted_affine)

  expect_error(
    ngeo_affine_grid_map(source, target, outside = "error"),
    class = "ngeo_error_coverage"
  )
  partial <- ngeo_affine_grid_map(
    source,
    target,
    outside = "drop"
  )
  expect_identical(partial$coverage, "partial")
  expect_lt(sum(Matrix::colSums(partial$operator) > 0), 8)

  rotated <- builder_volume()
  angle <- pi / 4
  rotated$base$geometry$affine[1:2, 1:2] <- matrix(
    c(cos(angle), sin(angle), -sin(angle), cos(angle)),
    2L
  )
  expect_error(
    ngeo_voxel_overlap_map(source, rotated),
    class = "ngeo_error_geometry"
  )
})

test_that("hard and probabilistic atlases construct bound region targets", {
  source <- builder_surface(values = c(10, 20, 30, 40))
  hard <- ngeo_atlas_map(source, c("A", "A", "B", "B"))
  probability <- matrix(
    c(
      1, 0,
      0.75, 0.25,
      0.25, 0.75,
      0, 1
    ),
    ncol = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B"))
  )
  soft <- ngeo_probabilistic_atlas_map(source, probability)

  expect_s3_class(hard$target, "ngeo_parcellation")
  expect_s3_class(soft$target, "ngeo_parcellation")
  expect_identical(hard$type, "crisp")
  expect_identical(soft$type, "probabilistic")
  expect_identical(hard$coverage, "complete")
  expect_equal(Matrix::colSums(soft$operator), rep(1, 4))
  expect_identical(
    hard$target_base_hash,
    base_hash(hard$target)
  )
  expect_silent(ngeo_validate_support_map(hard))
  expect_silent(ngeo_validate_support_map(soft))
})

test_that("aligned label builders expose partial coverage", {
  source <- builder_surface()
  map <- ngeo_label_overlap_map(
    source,
    c("A", "A", NA, "B")
  )

  expect_identical(map$coverage, "partial")
  expect_equal(Matrix::colSums(map$operator), c(1, 1, 0, 1))
})
