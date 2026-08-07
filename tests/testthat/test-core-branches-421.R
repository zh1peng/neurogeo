test_that("core base and label accessors expose validated fields", {
  x <- ngeo_point(
    cbind(x = 1:3, y = 0),
    values = cbind(signal = 1:3)
  )

  expect_identical(spatial_base(x), x$base)
  expect_identical(ngeo_labels(x), x$base$labels)
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
    ngeo_surface(coordinates, faces, coordinate_space = "surface"),
    class = "ngeo_error_coordinate_space"
  )
  expect_error(
    ngeo_surface(coordinates, faces, coordinate_space = ngeo_coordinate_space(kind = "volume")),
    class = "ngeo_error_coordinate_space"
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
  expect_equal(zero_based$base$geometry$faces, faces)
  expect_identical(zero_based$base$geometry$face_source_index_base, 0L)
  expect_identical(zero_based$base$elements$source_index, 0:3)
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
    ngeo_volume(dim = c(2, 2, 2), affine = diag(4), coordinate_space = "volume"),
    class = "ngeo_error_coordinate_space"
  )
  expect_error(
    ngeo_volume(
      dim = c(2, 2, 2),
      affine = diag(4),
      coordinate_space = ngeo_coordinate_space(kind = "surface")
    ),
    class = "ngeo_error_coordinate_space"
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
  expect_equal(dim(values(from_data_frame)), c(4L, 2L))
  expect_identical(from_data_frame$base$geometry$source_index_base, 0L)

  from_lattice_vector <- ngeo_volume(
    values = seq_len(8),
    dim = c(2, 2, 2),
    affine = diag(4),
    mask = mask
  )
  expect_identical(as.numeric(values(from_lattice_vector)), c(1, 3, 5, 7))
})

test_that("region constructor validates optional spatial contracts", {
  parcellation <- data.frame(region_id = c("A", "B"))
  expect_error(
    ngeo_parcellation(data.frame(name = "A")),
    class = "ngeo_error_base"
  )
  expect_error(
    ngeo_parcellation(data.frame(region_id = c("A", "A"))),
    class = "ngeo_error_base"
  )
  expect_error(
    ngeo_parcellation(parcellation, centroid = matrix(1:6, nrow = 3)),
    class = "ngeo_error_geometry"
  )
  expect_error(
    ngeo_parcellation(parcellation, support_size = -1),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_parcellation(parcellation, adjacency = diag(3)),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_parcellation(parcellation, membership = list(A = 1)),
    class = "ngeo_error_base"
  )
  expect_error(
    ngeo_parcellation(parcellation, source_base = c("a", "b")),
    class = "ngeo_error_argument"
  )

  point <- ngeo_point(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  x <- ngeo_parcellation(
    parcellation,
    source_base = point,
    membership = c("A", "B"),
    centroid = matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
    support_size = c(1, 1),
    adjacency = matrix(c(0, 1, 1, 0), nrow = 2)
  )
  expect_identical(x$base$geometry$source_base_hash, base_hash(point))
})

test_that("public selection paths reject missing, empty, and duplicate indices", {
  x <- ngeo_point(
    matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE),
    values = cbind(a = 1:3, b = 4:6)
  )
  ids <- base_elements(x)$element_id

  expect_equal(
    values(ngeo_subset(x, elements = ids[c(1, 3)])),
    matrix(c(1, 3, 4, 6), 2),
    ignore_attr = TRUE
  )
  expect_equal(
    values(ngeo_subset(x, elements = c(TRUE, FALSE, TRUE))),
    matrix(c(1, 3, 4, 6), 2),
    ignore_attr = TRUE
  )
  expect_equal(
    values(ngeo_subset(x, layers = c(TRUE, FALSE))),
    matrix(1:3, ncol = 1),
    ignore_attr = TRUE
  )
  expect_equal(
    values(ngeo_subset(x, layers = "b")),
    matrix(4:6, ncol = 1),
    ignore_attr = TRUE
  )

  expect_error(ngeo_subset(x, elements = "missing"), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, elements = logical(3)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, elements = c(1, 1)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, elements = 4), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, layers = "missing"), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, layers = c(TRUE, NA)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, layers = c(1, 1)), class = "ngeo_error_index")
  expect_error(ngeo_subset(x, layers = 3), class = "ngeo_error_index")

  geometry <- ngeo_point(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  expect_error(ngeo_subset(geometry, layers = 1), class = "ngeo_error_index")
})

test_that("neighbors dispatches contiguity by base and rejects unsupported point", {
  surface <- builder_surface()
  volume <- ngeo_volume(dim = c(2, 1, 1), affine = diag(4))
  parcellation <- ngeo_parcellation(
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
  grayordinate <- ngeo_grayordinate(list(component))

  expect_identical(ngeo_neighbors(surface, "contiguity")$method, "mesh_contiguity")
  expect_identical(ngeo_neighbors(volume, "contiguity")$method, "voxel_contiguity")
  expect_identical(ngeo_neighbors(parcellation, "contiguity")$method, "region_contiguity")
  expect_identical(
    ngeo_neighbors(grayordinate, "contiguity")$method,
    "component_contiguity"
  )

  point <- ngeo_point(matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE))
  expect_error(
    ngeo_neighbors(point, "contiguity"),
    class = "ngeo_error_capability"
  )
  expect_identical(ngeo_neighbors(point, "knn", k = 1)$method, "knn")
  expect_identical(
    ngeo_neighbors(point, "distance_band", threshold = 1.1)$method,
    "distance_band"
  )
})
