with_pdf_device <- function(code) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  force(code)
}

plot_surface_fixture <- function() {
  ngeo_surface(
    matrix(
      c(
        0, 0, 0,
        1, 0, 0,
        1, 1, 0,
        0, 1, 0
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE),
    values = cbind(signal = 1:4)
  )
}

test_that("all five domains have bounded base-graphics diagnostics", {
  surface <- ngeo_set_chart(
    plot_surface_fixture(),
    matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol = 2L, byrow = TRUE),
    name = "flat"
  )
  volume <- ngeo_volume(
    values = array(seq_len(8), dim = c(2, 2, 2)),
    dim = c(2, 2, 2),
    affine = diag(4)
  )
  points <- ngeo_points(
    matrix(c(0, 0, 1, 0, 1, 1), ncol = 2L, byrow = TRUE),
    values = 1:3
  )
  grayordinates <- ngeo_grayordinates(
    list(
      list(
        component_id = "left",
        kind = "surface",
        structure = "CORTEX_LEFT",
        vertex_index = c(0L, 2L),
        surface_vertex_count = 4L,
        source_index_base = 0L
      ),
      list(
        component_id = "right",
        kind = "surface",
        structure = "CORTEX_RIGHT",
        vertex_index = c(1L, 3L),
        surface_vertex_count = 4L,
        source_index_base = 0L
      )
    ),
    values = 1:4
  )
  regions <- ngeo_regions(
    data.frame(region_id = c("A", "B")),
    values = c(2, 4),
    centroid = matrix(c(0, 0, 1, 1), ncol = 2L, byrow = TRUE)
  )

  with_pdf_device({
    expect_identical(plot(surface), surface)
    expect_identical(plot(volume), volume)
    expect_identical(plot(points), points)
    expect_identical(plot(grayordinates), grayordinates)
    expect_identical(plot(regions), regions)
  })
})

test_that("projection diagnostics are explicit and invalid slices fail", {
  surface <- plot_surface_fixture()
  points <- ngeo_points(
    matrix(c(0, 0, 0, 1, 1, 1), ncol = 3L, byrow = TRUE),
    values = 1:2
  )
  volume <- ngeo_volume(
    values = array(seq_len(8), dim = c(2, 2, 2)),
    dim = c(2, 2, 2),
    affine = diag(4)
  )

  with_pdf_device({
    expect_warning(plot(surface), class = "ngeo_warning_plot_projection")
    expect_warning(plot(points), class = "ngeo_warning_plot_projection")
    expect_error(plot(volume, slice = 99L), class = "ngeo_error_argument")
  })
})

test_that("weights and partition diagnostics return their inputs", {
  points <- ngeo_points(
    matrix(c(0, 0, 1, 0, 2, 0), ncol = 2L, byrow = TRUE)
  )
  weights <- ngeo_weights(points, method = "knn", k = 1L, style = "B")
  surface <- plot_surface_fixture()
  partition <- ngeo_partition(surface, c("A", "A", "B", "B"))

  with_pdf_device({
    expect_identical(plot(weights), weights)
    expect_identical(plot(partition), partition)
  })
})
