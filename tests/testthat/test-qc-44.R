test_that("QC reports bounded value and semantic risks", {
  x <- ngeo_point(
    matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE),
    values = cbind(
      constant = c(1, 1, 1),
      incomplete = c(1, NA, Inf)
    )
  )
  before <- x
  qc <- ngeo_qc(x)

  expect_s3_class(qc, "ngeo_qc")
  expect_identical(x, before)
  expect_true(all(c(
    "check", "status", "value", "message"
  ) %in% names(qc$checks)))
  expect_true(any(
    qc$checks$check == "space_known" &
      qc$checks$status == "warning"
  ))
  expect_true(any(
    qc$checks$check == "measurement_semantics" &
      qc$checks$status == "warning"
  ))
  expect_true(any(
    qc$checks$check == "constant_maps" &
      qc$checks$status == "warning"
  ))
  expect_true(any(
    qc$checks$check == "nonfinite_value_fraction" &
      qc$checks$status == "warning"
  ))
  expect_identical(qc$overall_status, "warning")
})

test_that("QC respects the values scan budget", {
  x <- ngeo_point(
    cbind(seq_len(10), 0),
    values = cbind(signal = seq_len(10)),
    measures = ngeo_measure(support_behavior = "intensive", unit = "a.u.")
  )
  qc <- ngeo_qc(x, max_value_cells = 5)

  expect_true(any(
    qc$checks$check == "values_scan" &
      qc$checks$status == "not_evaluated"
  ))
  expect_null(qc$map_summary)
})

test_that("QC covers every base without inventing topology", {
  surface <- qc_cartography_disk()
  volume <- ngeo_volume(
    c(2, 2, 1),
    affine = diag(4),
    values = array(1:4, dim = c(2, 2, 1))
  )
  point <- ngeo_point(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  gray <- ngeo_grayordinate(list(
    list(
      component_id = "left",
      kind = "surface",
      structure = "cortex_left",
      vertex_index = 0:4,
      surface_vertex_count = 5L,
      source_index_base = 0L,
      geometry = surface
    )
  ))
  parcellation <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    adjacency = Matrix::Matrix(matrix(c(0, 1, 1, 0), 2), sparse = TRUE)
  )

  reports <- lapply(
    list(surface, volume, point, gray, parcellation),
    ngeo_qc
  )
  expect_identical(
    vapply(reports, `[[`, character(1), "domain_type"),
    c("surface", "volume", "point", "grayordinate", "parcellation")
  )
  point_topology <- reports[[3L]]$checks[
    reports[[3L]]$checks$check == "topology",
    "status"
  ]
  expect_identical(point_topology, "not_applicable")
})

test_that("QC summarizes charts and sparse support diagnostics", {
  surface <- ngeo_flatten_surface(
    qc_cartography_disk(),
    "harmonic",
    boundary = 1:4
  )
  atlas <- ngeo_atlas_map(
    surface,
    c("A", "A", "B", "B", NA),
    source_support = rep(1, 5)
  )
  qc <- ngeo_qc(surface, support_map = atlas, chart = "flat")

  expect_identical(qc$chart_summary$chart, "flat")
  expect_true(all(c(
    "charted_faces", "folded_faces", "charted_fraction"
  ) %in% names(qc$chart_summary)))
  expect_s3_class(qc$support, "ngeo_support_diagnostics")
  expect_true(any(qc$checks$check == "support_coverage"))
  expect_true(any(qc$checks$check == "support_conservation"))

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  expect_identical(plot(qc), qc)
  grDevices::dev.off()
  expect_gt(file.info(path)$size, 1000)
})

test_that("QC validates arguments and support alignment", {
  x <- ngeo_point(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  other <- ngeo_point(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE))
  map <- ngeo_atlas_map(other, c("A", "B"), source_support = c(1, 1))

  expect_error(ngeo_qc(x, tolerance = 0), class = "ngeo_error_argument")
  expect_error(
    ngeo_qc(x, max_value_cells = -1),
    class = "ngeo_error_argument"
  )
  expect_error(
    ngeo_qc(x, support_map = map),
    class = "ngeo_error_alignment"
  )
})

test_that("4.4 API and migration contracts are installed from canonical sources", {
  for (resource in c("API-4.4.md", "migration-4.4.md")) {
    installed <- system.file("spec", resource, package = "neurogeo")
    source <- testthat::test_path("..", "..", "inst", "spec", resource)
    expect_true(nzchar(installed), info = resource)
    expect_true(file.exists(installed), info = resource)
    if (file.exists(source)) {
      expect_identical(
        readLines(installed, warn = FALSE),
        readLines(source, warn = FALSE),
        info = resource
      )
    }
  }
})
