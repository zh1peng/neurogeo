test_that("continuous cortical layouts share one pooled scale", {
  flat <- ngeo_flatten_surface(
    qc_cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  left <- ngeo_cortical_map(flat, values = c(0, 1, 2, 3, 4))
  right <- ngeo_cortical_map(flat, values = c(10, 11, 12, 13, NA))

  layout <- ngeo_cortical_layout(
    left,
    right,
    labels = c("Left", "Right"),
    shared_scale = TRUE
  )
  expected_limits <- range(c(
    left$face_data$value[left$face_data$included],
    right$face_data$value[right$face_data$included]
  ), na.rm = TRUE)

  expect_true(layout$shared_scale)
  expect_equal(layout$layers[[1L]]$limits, expected_limits)
  expect_equal(layout$layers[[2L]]$limits, expected_limits)
  expect_identical(
    layout$layers[[1L]]$legend,
    layout$layers[[2L]]$legend
  )
  expect_identical(layout$legend, layout$layers[[1L]]$legend)

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = 8, height = 4.5)
  expect_silent(plot(layout))
  grDevices::dev.off()
  expect_gt(file.info(path)$size, 1000)
})

test_that("categorical cortical layouts preserve one color contract", {
  flat <- ngeo_flatten_surface(
    qc_cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  colors <- c(A = "#CC3311", B = "#0077BB", C = "#009988")
  left <- ngeo_cortical_map(
    flat,
    values = c("A", "A", "B", "B", NA),
    colors = colors
  )
  right <- ngeo_cortical_map(
    flat,
    values = c("B", "B", "C", "C", NA),
    colors = colors
  )

  layout <- ngeo_cortical_layout(left, right, shared_scale = TRUE)
  expect_identical(layout$legend$value, c("A", "B", "C"))
  expect_identical(
    layout$legend$color,
    unname(colors[c("A", "B", "C")])
  )
  expect_identical(
    layout$layers[[1L]]$legend,
    layout$layers[[2L]]$legend
  )

  conflict <- right
  conflict$legend$color[conflict$legend$value == "B"] <- "#000000"
  expect_error(
    ngeo_cortical_layout(left, conflict, shared_scale = TRUE),
    class = "ngeo_error_alignment"
  )
})

test_that("shared cortical layouts reject incompatible map types", {
  flat <- ngeo_flatten_surface(
    qc_cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  continuous <- ngeo_cortical_map(flat, values = 1:5)
  categorical <- ngeo_cortical_map(flat, values = letters[1:5])

  expect_error(
    ngeo_cortical_layout(
      continuous,
      categorical,
      shared_scale = TRUE
    ),
    class = "ngeo_error_alignment"
  )
  expect_error(
    ngeo_cortical_layout(continuous, shared_scale = NA),
    class = "ngeo_error_argument"
  )
  different_palette <- ngeo_cortical_map(
    flat,
    values = 6:10,
    palette = "Inferno"
  )
  expect_error(
    ngeo_cortical_layout(
      continuous,
      different_palette,
      shared_scale = TRUE
    ),
    class = "ngeo_error_alignment"
  )
})
