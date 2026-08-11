cartography_disk <- function(values = cbind(signal = c(1, 2, 4, 3, 2.5))) {
  ngeo_surface(
    coordinates = matrix(
      c(
        0, 0, 0,
        1, 0, 0,
        1, 1, 0,
        0, 1, 0,
        0.5, 0.5, 0.2
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    faces = matrix(
      c(
        1, 2, 5,
        2, 3, 5,
        3, 4, 5,
        4, 1, 5
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    values = values,
    measures = ngeo_measure(support_behavior = "intensive")
  )
}

test_that("harmonic cartography enforces disk topology and boundary order", {
  x <- cartography_disk()
  flat <- ngeo_flatten_surface(
    x,
    method = "harmonic",
    boundary = 1:4,
    name = "disk"
  )
  chart <- flat$base$geometry$coordinates$disk
  metadata <- flat$base$charts$disk

  expect_equal(chart[5L, ], c(0, 0), tolerance = 1e-12)
  expect_identical(metadata$kind, "parameterization")
  expect_true(metadata$is_metric_flattening)
  expect_identical(metadata$boundary, 1:4)
  expect_identical(metadata$invariants$euler_characteristic, 1L)
  expect_identical(metadata$invariants$connected_components, 1L)
  expect_true(metadata$invariants$disk)
  expect_equal(nrow(metadata$distortion), nrow(x$base$geometry$faces))
  expect_identical(
    metadata$source_vertex_id,
    x$base$elements$element_id
  )
  expect_identical(metadata$source_face, seq_len(nrow(x$base$geometry$faces)))
  expect_silent(ngeo_validate(flat, "strict"))

  expect_error(
    ngeo_flatten_surface(x, "harmonic", boundary = c(1, 3, 2, 4)),
    class = "ngeo_error_topology"
  )
  expect_error(
    ngeo_flatten_surface(x, "harmonic", boundary = c(1, 2, 3)),
    class = "ngeo_error_topology"
  )
})

test_that("closed and non-manifold meshes are not silently cut", {
  tetrahedron <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
      ncol = 3L,
      byrow = TRUE
    ),
    matrix(
      c(1, 2, 3, 1, 4, 2, 2, 4, 3, 1, 3, 4),
      ncol = 3L,
      byrow = TRUE
    )
  )
  expect_error(
    ngeo_flatten_surface(
      tetrahedron,
      "harmonic",
      boundary = c(1, 2, 3)
    ),
    class = "ngeo_error_topology"
  )
})

test_that("imported charts preserve coordinates and report folds", {
  x <- cartography_disk()
  imported <- matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0.5, 0.5),
    ncol = 2L,
    byrow = TRUE
  )
  result <- ngeo_flatten_surface(
    x,
    method = "imported",
    coordinates = imported,
    name = "caller"
  )

  expect_identical(result$base$geometry$coordinates$caller, imported)
  expect_identical(result$base$charts$caller$method, "imported")
  expect_false(result$base$charts$caller$invariants$topology_assumed)
  expect_s3_class(ngeo_chart_distortion(result, "caller"), "data.frame")
  expect_error(
    ngeo_flatten_surface(
      x,
      "imported",
      coordinates = imported[-1L, ]
    ),
    class = "ngeo_error_alignment"
  )
})

test_that("view projections are explicit and never distance_method flattening", {
  x <- cartography_disk()
  orthographic <- ngeo_project_surface(
    x,
    "orthographic",
    view = "xz",
    name = "ortho"
  )
  pca_first <- ngeo_project_surface(x, "pca", name = "pca")
  pca_second <- ngeo_project_surface(x, "pca", name = "pca")

  expect_identical(
    orthographic$base$charts$ortho$kind,
    "view_projection"
  )
  expect_false(
    orthographic$base$charts$ortho$is_metric_flattening
  )
  expect_identical(
    orthographic$base$charts$ortho$invariants$view,
    "xz"
  )
  expect_equal(
    pca_first$base$geometry$coordinates$pca,
    pca_second$base$geometry$coordinates$pca
  )
  expect_equal(
    pca_first$base$charts$pca$invariants$center,
    colMeans(x$base$geometry$coordinates$active)
  )
  expect_identical(pca_first$base$charts$pca$tolerance, 1e-12)
  expect_error(
    ngeo_project_surface(x, "spherical"),
    class = "ngeo_error_chart"
  )

  sphere <- x
  sphere$base$geometry$coordinates$active <- sweep(
    sphere$base$geometry$coordinates$active,
    2L,
    colMeans(sphere$base$geometry$coordinates$active),
    "-"
  )
  sphere$base$geometry$coordinates$active[, 3L] <-
    sphere$base$geometry$coordinates$active[, 3L] + 1
  spherical <- ngeo_project_surface(
    sphere,
    "spherical",
    seam = pi / 2,
    name = "sphere_view"
  )
  expect_identical(
    spherical$base$charts$sphere_view$seam,
    pi / 2
  )
  expect_true(all(
    spherical$base$geometry$coordinates$sphere_view[, 1L] >= -pi &
      spherical$base$geometry$coordinates$sphere_view[, 1L] < pi
  ))
  expect_s3_class(
    spherical$base$charts$sphere_view$invariants$seam_edges,
    "data.frame"
  )
  expect_type(
    spherical$base$charts$sphere_view$invariants$seam_faces,
    "integer"
  )
  expect_error(
    ngeo_project_surface(x, tolerance = Inf),
    class = "ngeo_error_argument"
  )
})

test_that("spherical seam faces and boundaries render as wrapped copies", {
  closed <- ngeo_surface(
    matrix(
      c(
        -1, 0.1, 0,
        -1, -0.1, 0,
        0, 0, 1,
        0, 0, -1
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    matrix(
      c(1, 2, 3, 1, 4, 2, 2, 4, 3, 1, 3, 4),
      ncol = 3L,
      byrow = TRUE
    ),
    values = cbind(signal = 1:4)
  )
  sphere <- ngeo_project_surface(
    closed,
    "spherical",
    seam = 0,
    name = "sphere"
  )
  metadata <- sphere$base$charts$sphere
  expect_gt(length(metadata$invariants$seam_faces), 0L)
  expect_gt(nrow(metadata$invariants$seam_edges), 0L)
  expect_equal(metadata$invariants$center, colMeans(
    closed$base$geometry$coordinates$active
  ))

  map <- ngeo_cortical_map(
    sphere,
    chart = "sphere",
    atlas = c("A", "B", "A", "B")
  )
  expect_identical(
    map$history$seam_faces,
    metadata$invariants$seam_faces
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output, width = 6, height = 3)
  expect_silent(plot(map, show_boundaries = TRUE))
  grDevices::dev.off()
  expect_gt(file.info(output)$size, 1000)
})

test_that("cortical layers support vertex data, atlas boundaries, and exchange", {
  flat <- ngeo_flatten_surface(
    cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  partition <- ngeo_partition(
    flat,
    c("anterior", "anterior", "posterior", "posterior", "middle")
  )
  map <- ngeo_cortical_map(
    flat,
    layer = "signal",
    chart = "flat",
    atlas = partition
  )
  data <- ngeo_cortical_map_data(map)

  expect_s3_class(map, "ngeo_cortical_map")
  expect_identical(map$value_type, "continuous")
  expect_equal(nrow(data$vertices), 5L)
  expect_equal(nrow(data$faces), 4L)
  expect_true(nrow(data$boundaries) > 0L)
  expect_identical(data$faces$source_face, 1:4)
  expect_identical(data$vertices$source_vertex, 1:5)
  expect_identical(
    data$metadata$history$source_base_hash,
    base_hash(flat)
  )
  expect_identical(data$metadata$palette, "viridis")
  expect_identical(data$metadata$na_color, "grey85")

  categorical <- ngeo_cortical_map(
    flat,
    values = c("A", "A", "B", NA, "B"),
    chart = "flat",
    palette = "Set 3"
  )
  expect_identical(categorical$value_type, "categorical")
  expect_true(is.character(categorical$face_data$value))
  expect_setequal(categorical$legend$value, c("A", "B"))

  constant <- ngeo_cortical_map(
    flat,
    values = rep(2, 5L),
    chart = "flat"
  )
  expect_identical(constant$limits, c(2, 2))
  expect_length(unique(constant$face_data$color), 1L)

  before_chart <- cartography_disk()
  atlas_before_chart <- ngeo_partition(
    before_chart,
    c("A", "A", "B", "B", "C")
  )
  after_chart <- ngeo_flatten_surface(
    before_chart,
    "harmonic",
    boundary = 1:4
  )
  expect_s3_class(
    ngeo_cortical_map(after_chart, atlas = atlas_before_chart),
    "ngeo_cortical_map"
  )
})

test_that("imported flat surfaces verify and retain source-face subsets", {
  source <- cartography_disk()
  flat_surface <- ngeo_surface(
    coordinates = matrix(
      c(
        0, 0,
        2, 0,
        2, 1,
        0, 1,
        1, 0.5
      ),
      ncol = 2L,
      byrow = TRUE
    ),
    faces = source$base$geometry$faces[1:3, , drop = FALSE],
    coordinate_roles = "chart",
    index_base = "one"
  )
  flat <- ngeo_flatten_surface(
    source,
    "imported",
    coordinates = flat_surface
  )
  metadata <- flat$base$charts$flat
  map <- ngeo_cortical_map(flat, chart = "flat")

  expect_true(metadata$invariants$topology_verified)
  expect_identical(
    metadata$invariants$topology_relation,
    "face_subset"
  )
  expect_identical(metadata$invariants$source_face_in_chart, 1:3)
  expect_identical(sum(map$face_data$charted), 3L)
  expect_identical(sum(map$face_data$included), 3L)
  expect_gt(nrow(map$outline), 0L)

  flat_surface$base$geometry$faces[1L, ] <- c(1L, 2L, 4L)
  expect_error(
    ngeo_flatten_surface(
      source,
      "imported",
      coordinates = flat_surface,
      name = "bad"
    ),
    class = "ngeo_error_alignment"
  )

  flat_surface <- ngeo_surface(
    coordinates = matrix(
      c(0, 0, 2, 0, 2, 1, 0, 1, 1, 0.5),
      ncol = 2L,
      byrow = TRUE
    ),
    faces = source$base$geometry$faces[1:3, , drop = FALSE],
    coordinate_roles = "chart",
    index_base = "one"
  )
  flat_surface$base$elements$element_id[[1L]] <- "different"
  expect_error(
    ngeo_flatten_surface(
      source,
      "imported",
      coordinates = flat_surface,
      name = "misidentified"
    ),
    class = "ngeo_error_alignment"
  )
})

test_that("flatmaps combine masks, underlays, label atlases, and source colors", {
  source <- cartography_disk()
  source$base$labels$atlas <- list(
    values = c(1L, 1L, 2L, 2L, 2L),
    table = data.frame(
      label = c("anterior", "posterior"),
      Key = c(1L, 2L),
      Red = c(0.8, 0.1),
      Green = c(0.2, 0.5),
      Blue = c(0.1, 0.9),
      Alpha = c(1, 1),
      stringsAsFactors = FALSE
    )
  )
  flat <- ngeo_flatten_surface(
    source,
    "harmonic",
    boundary = 1:4
  )
  map <- ngeo_cortical_map(
    flat,
    chart = "flat",
    atlas = "atlas",
    fill = "atlas",
    mask = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    underlay = "signal",
    overlay_alpha = 0.6,
    na_color = NA_character_
  )
  data <- ngeo_cortical_map_data(map)

  expect_identical(map$fill, "atlas")
  expect_identical(map$layer_name, "atlas")
  expect_setequal(map$legend$value, c("anterior", "posterior"))
  expect_true(all(grepl("^#[0-9A-F]{8}$", map$legend$color)))
  expect_identical(sum(map$vertices$included), 4L)
  expect_identical(sum(map$face_data$included), 2L)
  expect_gt(nrow(data$outline), 0L)
  expect_gt(nrow(data$label_positions), 0L)
  expect_true(all(data$label_positions$source_vertex %in%
    which(map$vertices$included)))
  expect_true(any(is.finite(data$faces$underlay_value)))
  expect_identical(data$metadata$underlay_name, "signal")
  expect_identical(data$metadata$overlay_alpha, 0.6)
  expect_identical(
    data$metadata$history$atlas_source,
    "labels"
  )

  expect_error(
    ngeo_cortical_map(
      flat,
      chart = "flat",
      atlas = c("A", "A", "B", "B", "B"),
      fill = "atlas",
      colors = c(A = "red")
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_cortical_map(flat, chart = "flat", fill = "atlas"),
    class = "ngeo_error_argument"
  )
})

test_that("atlas coverage can mask unlabeled cortex without hiding vertex maps", {
  flat <- ngeo_flatten_surface(
    cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  atlas <- c(NA_character_, "A", "B", "B", "C")

  surface_extent <- ngeo_cortical_map(
    flat,
    chart = "flat",
    atlas = atlas,
    fill = "atlas",
    underlay = "signal",
    na_color = NA_character_,
    atlas_coverage = "surface"
  )
  labeled_extent <- ngeo_cortical_map(
    flat,
    chart = "flat",
    atlas = atlas,
    fill = "atlas",
    underlay = "signal",
    na_color = NA_character_,
    atlas_coverage = "labeled"
  )
  automatic_extent <- ngeo_cortical_map(
    flat,
    chart = "flat",
    atlas = atlas,
    fill = "atlas",
    underlay = "signal",
    na_color = NA_character_
  )
  vertex_extent <- ngeo_cortical_map(
    flat,
    chart = "flat",
    atlas = atlas,
    underlay = "signal",
    na_color = NA_character_
  )
  data <- ngeo_cortical_map_data(labeled_extent)

  expect_equal(sum(surface_extent$face_data$included), 4L)
  expect_equal(sum(labeled_extent$vertices$included), 4L)
  expect_equal(sum(labeled_extent$face_data$included), 2L)
  expect_identical(automatic_extent$atlas_coverage, "labeled")
  expect_equal(sum(automatic_extent$face_data$included), 2L)
  expect_identical(vertex_extent$atlas_coverage, "surface")
  expect_equal(sum(vertex_extent$face_data$included), 4L)
  expect_identical(data$metadata$atlas_coverage, "labeled")
  expect_identical(data$metadata$history$surface_mask_vertices, 5L)
  expect_identical(data$metadata$history$atlas_labeled_vertices, 4L)
  expect_identical(data$metadata$history$atlas_unlabeled_vertices, 1L)
  expect_true(all(is.na(
    labeled_extent$face_data$underlay_color[
      !labeled_extent$face_data$included
    ]
  )))

  expect_error(
    ngeo_cortical_map(
      flat,
      chart = "flat",
      atlas_coverage = "labeled"
    ),
    class = "ngeo_error_argument"
  )
})

test_that("cortical maps lift atlas-aligned parcellation values", {
  flat <- ngeo_flatten_surface(
    cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  atlas <- c(NA_character_, "A", "B", "B", "C")
  parcel_values <- ngeo_parcellation(
    data.frame(region_id = c("A", "B", "C")),
    values = cbind(effect = c(-0.2, 0.1, 0.3)),
    measures = ngeo_measure(support_behavior = "intensive"),
    coordinate_space = ngeo_spatial_base(flat)$coordinate_space
  )

  map <- ngeo_cortical_map(
    flat,
    values = parcel_values,
    layer = "effect",
    atlas = atlas,
    chart = "flat"
  )

  expect_equal(
    map$vertices$value,
    c(NA_real_, -0.2, 0.1, 0.1, 0.3)
  )
  expect_identical(map$layer_name, "effect")
  expect_identical(map$atlas_coverage, "labeled")
  expect_identical(map$history$atlas_coverage_requested, "auto")

  incomplete <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    values = cbind(effect = c(-0.2, 0.1)),
    measures = ngeo_measure(support_behavior = "intensive"),
    coordinate_space = ngeo_spatial_base(flat)$coordinate_space
  )
  expect_error(
    ngeo_cortical_map(flat, values = incomplete, atlas = atlas),
    class = "ngeo_error_alignment"
  )

  extra <- ngeo_parcellation(
    data.frame(region_id = c("A", "B", "C", "D")),
    values = cbind(effect = c(-0.2, 0.1, 0.3, 0.4)),
    measures = ngeo_measure(support_behavior = "intensive"),
    coordinate_space = ngeo_spatial_base(flat)$coordinate_space
  )
  expect_error(
    ngeo_cortical_map(flat, values = extra, atlas = atlas),
    class = "ngeo_error_alignment"
  )
})

test_that("named parcel vectors lift by identity rather than row position", {
  flat <- ngeo_flatten_surface(
    cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  atlas <- c(NA_character_, "A", "B", "B", "C")
  parcel_values <- c(B = 0.1, C = 0.3, A = -0.2)

  map <- ngeo_cortical_map(
    flat,
    values = parcel_values,
    atlas = atlas,
    chart = "flat"
  )

  expect_equal(
    map$vertices$value,
    c(NA_real_, -0.2, 0.1, 0.1, 0.3)
  )
  expect_identical(map$atlas_coverage, "labeled")
  expect_identical(map$layer_name, "parcel_value")

  invalid <- list(
    unname(parcel_values),
    c(A = -0.2, B = 0.1),
    c(A = -0.2, A = -0.1, B = 0.1, C = 0.3),
    c(A = -0.2, B = 0.1, C = 0.3, D = 0.4)
  )
  for (current in invalid) {
    expect_error(
      ngeo_cortical_map(flat, values = current, atlas = atlas),
      class = "ngeo_error_alignment"
    )
  }
})

test_that("cortical map alignment and chart selection fail explicitly", {
  x <- cartography_disk()
  expect_error(
    ngeo_cortical_map(x),
    class = "ngeo_error_chart"
  )
  flat <- ngeo_flatten_surface(x, "harmonic", boundary = 1:4)
  expect_error(
    ngeo_cortical_map(flat, values = 1:4),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_cortical_map(flat, values = matrix(1:5, ncol = 1L)),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_cortical_map(flat, atlas = 1:4),
    class = "ngeo_error_alignment"
  )
  unrelated <- cartography_disk()
  unrelated$base$geometry$coordinates$active[, 1L] <-
    unrelated$base$geometry$coordinates$active[, 1L] + 10
  unrelated_atlas <- ngeo_partition(
    unrelated,
    c("A", "A", "B", "B", "C")
  )
  expect_error(
    ngeo_cortical_map(flat, atlas = unrelated_atlas),
    class = "ngeo_error_base_mismatch"
  )
  expect_error(
    ngeo_cortical_map(flat, na_color = "not-a-real-color"),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_cortical_map(flat, palette = "not-a-real-palette"),
    class = "ngeo_error_argument"
  )
})

test_that("degenerate source geometry is rejected before distortion claims", {
  degenerate <- ngeo_surface(
    matrix(
      c(0, 0, 0, 1, 0, 0, 2, 0, 0),
      ncol = 3L,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3), ncol = 3L)
  )
  expect_error(
    ngeo_flatten_surface(
      degenerate,
      "imported",
      coordinates = matrix(c(0, 0, 1, 0, 2, 0), ncol = 2L, byrow = TRUE)
    ),
    class = "ngeo_error_geometry"
  )
  no_faces <- ngeo_surface(
    matrix(c(0, 0, 0, 1, 0, 0), ncol = 3L, byrow = TRUE),
    matrix(integer(), ncol = 3L)
  )
  expect_error(
    ngeo_project_surface(no_faces),
    class = "ngeo_error_topology"
  )
})

test_that("base rendering and multi-panel layout produce portable output", {
  flat <- ngeo_flatten_surface(
    cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  first <- ngeo_cortical_map(flat, atlas = c("A", "A", "B", "B", "C"))
  second <- ngeo_cortical_map(
    flat,
    values = c("low", "low", "high", "high", NA)
  )
  layout <- ngeo_cortical_layout(
    first,
    second,
    ncol = 2L,
    labels = c("continuous", "categorical")
  )
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output, width = 8, height = 4)
  expect_silent(plot(layout, show_boundaries = TRUE))
  grDevices::dev.off()

  expect_s3_class(layout, "ngeo_cortical_layout")
  expect_identical(layout$nrow, 1L)
  expect_identical(layout$ncol, 2L)
  expect_gt(file.info(output)$size, 1000)

  no_legend <- tempfile(fileext = ".pdf")
  grDevices::pdf(no_legend, width = 4, height = 4)
  expect_silent(plot(first, show_legend = FALSE))
  grDevices::dev.off()
  expect_gt(file.info(no_legend)$size, 1000)
})

test_that("cortical plot data matches the semantic golden fixture", {
  skip_if_not_installed("jsonlite")
  flat <- ngeo_flatten_surface(
    cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  map <- ngeo_cortical_map(
    flat,
    atlas = c("A", "A", "B", "B", "C")
  )
  golden <- jsonlite::read_json(
    system.file(
      "extdata", "golden", "cortical-map-43.json",
      package = "neurogeo"
    ),
    simplifyVector = TRUE
  )

  expect_identical(nrow(map$vertices), golden$vertices)
  expect_identical(nrow(map$face_data), golden$faces)
  expect_identical(nrow(map$boundaries), golden$boundaries)
  expect_equal(unname(map$coordinates), golden$coordinates, tolerance = 1e-12)
  expect_equal(
    map$face_data$value,
    golden$face_values,
    tolerance = 1e-12
  )
  expect_identical(map$face_data$color, golden$face_colors)
  expect_identical(
    unname(as.matrix(map$boundaries[, c("from", "to")])),
    unname(golden$boundary_edges)
  )
})
