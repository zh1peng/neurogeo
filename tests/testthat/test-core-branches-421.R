test_that("core domain and label accessors expose validated fields", {
  x <- ngeo_points(
    cbind(x = 1:3, y = 0),
    values = cbind(signal = 1:3)
  )

  expect_identical(ngeo_domain(x), x$domain)
  expect_identical(ngeo_labels(x), x$labels)
})

test_that("surface constructor rejects ambiguous geometry metadata", {
  coordinates <- matrix(
    c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(c(1, 2, 3, 1, 3, 4), ncol = 3, byrow = TRUE)

  expect_error(ngeo_surface(list(), faces), class = "ngeo_error_geometry")
  expect_error(
    ngeo_surface(
      structure(list(coordinates, coordinates), names = c("same", "same")),
      faces
    ),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_surface(list(a = coordinates, b = coordinates[-1, ]), faces),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_surface(matrix(1:16, ncol = 4), faces),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_surface(coordinates, matrix(1:4, nrow = 1)),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_surface(coordinates, matrix(c(1, 2, 5), nrow = 1)),
    class = "ngeo_error_index"
  )
  expect_error(
    ngeo_surface(coordinates, faces, space = "surface"),
    class = "ngeo_error_space"
  )
  expect_error(
    ngeo_surface(coordinates, faces, space = ngeo_space(kind = "volume")),
    class = "ngeo_error_space"
  )
  expect_error(
    ngeo_surface(
      list(anatomical = coordinates, inflated = coordinates),
      faces,
      active_coordinates = "missing"
    ),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_surface(
      list(anatomical = coordinates, inflated = coordinates),
      faces,
      coordinate_roles = "anatomical"
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_surface(
      coordinates,
      faces,
      coordinate_roles = "flattened"
    ),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_surface(coordinates, faces, mask = c(TRUE, FALSE)),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_surface(coordinates, faces, source_index_base = 2L),
    class = "ngeo_error_index"
  )

  zero_based <- ngeo_surface(
    coordinates,
    faces - 1L,
    index_base = "zero",
    mask = c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(zero_based$domain$faces, faces)
  expect_identical(zero_based$domain$face_source_index_base, 0L)
  expect_identical(zero_based$domain$elements$source_index, 0:3)
})

test_that("volume constructor enforces lattice, affine, and mask alignment", {
  expect_error(
    ngeo_volume(dim = c(2, 2), affine = diag(4)),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_volume(dim = c(2, 2, 2), affine = diag(3)),
    class = "ngeo_error_geometry"
  )
  singular <- diag(4)
  singular[1, 1] <- 0
  expect_error(
    ngeo_volume(dim = c(2, 2, 2), affine = singular),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_volume(dim = c(2, 2, 2), affine = diag(4), space = "volume"),
    class = "ngeo_error_space"
  )
  expect_error(
    ngeo_volume(
      dim = c(2, 2, 2),
      affine = diag(4),
      space = ngeo_space(kind = "surface")
    ),
    class = "ngeo_error_space"
  )
  expect_error(
    ngeo_volume(
      dim = c(2, 2, 2),
      affine = diag(4),
      mask = array(TRUE, c(2, 2, 1))
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_volume(
      dim = c(2, 2, 2),
      affine = diag(4),
      mask = c(TRUE, FALSE)
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_volume(
      values = array(1:6, c(3, 2, 1)),
      dim = c(2, 2, 2),
      affine = diag(4)
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_volume(
      values = list(1, 2),
      dim = c(2, 2, 2),
      affine = diag(4)
    ),
    class = "ngeo_error_values"
  )
  expect_error(
    ngeo_volume(
      values = matrix(1:6, nrow = 3),
      dim = c(2, 2, 2),
      affine = diag(4)
    ),
    class = "ngeo_error_alignment"
  )

  mask <- array(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), c(2, 2, 2))
  from_data_frame <- ngeo_volume(
    values = data.frame(a = 1:4, b = 5:8),
    dim = c(2, 2, 2),
    affine = diag(4),
    mask = mask,
    index_base = "zero"
  )
  expect_equal(dim(ngeo_values(from_data_frame)), c(4L, 2L))
  expect_identical(from_data_frame$domain$source_index_base, 0L)

  from_lattice_vector <- ngeo_volume(
    values = seq_len(8),
    dim = c(2, 2, 2),
    affine = diag(4),
    mask = mask
  )
  expect_identical(as.numeric(ngeo_values(from_lattice_vector)), c(1, 3, 5, 7))
})

test_that("region constructor validates optional spatial contracts", {
  regions <- data.frame(region_id = c("A", "B"))
  expect_error(
    ngeo_regions(data.frame(name = "A")),
    class = "ngeo_error_domain"
  )
  expect_error(
    ngeo_regions(data.frame(region_id = c("A", "A"))),
    class = "ngeo_error_domain"
  )
  expect_error(
    ngeo_regions(regions, centroid = matrix(1:6, nrow = 3)),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_regions(regions, support_size = -1),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_regions(regions, adjacency = diag(3)),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_regions(regions, membership = list(A = 1)),
    class = "ngeo_error_domain"
  )
  expect_error(
    ngeo_regions(regions, base_domain = c("a", "b")),
    class = "ngeo_error_argument"
  )

  points <- ngeo_points(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  x <- ngeo_regions(
    regions,
    base_domain = points,
    membership = c("A", "B"),
    centroid = matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
    support_size = c(1, 1),
    adjacency = matrix(c(0, 1, 1, 0), nrow = 2)
  )
  expect_identical(x$domain$base_domain_hash, ngeo_domain_hash(points))
})

test_that("public selection paths reject missing, empty, and duplicate indices", {
  x <- ngeo_points(
    matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE),
    values = cbind(a = 1:3, b = 4:6)
  )
  ids <- ngeo_elements(x)$element_id

  expect_equal(
    ngeo_values(ngeo_subset(x, elements = ids[c(1, 3)])),
    matrix(c(1, 3, 4, 6), 2),
    ignore_attr = TRUE
  )
  expect_equal(
    ngeo_values(ngeo_subset(x, elements = c(TRUE, FALSE, TRUE))),
    matrix(c(1, 3, 4, 6), 2),
    ignore_attr = TRUE
  )
  expect_equal(
    ngeo_values(ngeo_subset(x, maps = c(TRUE, FALSE))),
    matrix(1:3, ncol = 1),
    ignore_attr = TRUE
  )
  expect_equal(
    ngeo_values(ngeo_subset(x, maps = "b")),
    matrix(4:6, ncol = 1),
    ignore_attr = TRUE
  )

  expect_error(ngeo_subset(x, elements = "missing"), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, elements = logical(3)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, elements = c(1, 1)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, elements = 4), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, maps = "missing"), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, maps = c(TRUE, NA)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, maps = c(1, 1)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, maps = 3), class = "ngeo_error_index")

  geometry <- ngeo_points(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  expect_error(ngeo_subset(geometry, maps = 1), class = "ngeo_error_index")
})

test_that("neighbors dispatches contiguity by domain and rejects unsupported points", {
  surface <- builder_surface()
  volume <- ngeo_volume(dim = c(2, 1, 1), affine = diag(4))
  regions <- ngeo_regions(
    data.frame(region_id = c("A", "B")),
    adjacency = matrix(c(0, 1, 1, 0), 2)
  )
  component <- list(
    component_id = "left",
    kind = "surface",
    structure = "CORTEX_LEFT",
    vertex_index = 0:3,
    surface_vertex_count = 4L,
    geometry = surface
  )
  grayordinates <- ngeo_grayordinates(list(component))

  expect_identical(ngeo_neighbors(surface, "contiguity")$method, "mesh_contiguity")
  expect_identical(ngeo_neighbors(volume, "contiguity")$method, "voxel_contiguity")
  expect_identical(ngeo_neighbors(regions, "contiguity")$method, "region_contiguity")
  expect_identical(
    ngeo_neighbors(grayordinates, "contiguity")$method,
    "component_contiguity"
  )

  points <- ngeo_points(matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE))
  expect_error(
    ngeo_neighbors(points, "contiguity"),
    class = "ngeo_error_capability"
  )
  expect_identical(ngeo_neighbors(points, "knn", k = 1)$method, "knn")
  expect_identical(
    ngeo_neighbors(points, "distance_band", threshold = 1.1)$method,
    "distance_band"
  )
})
