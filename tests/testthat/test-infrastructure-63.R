test_that("layer views expose one complete spatial-field contract", {
  x <- ngeo_point(
    cbind(x = 0:2, y = 0),
    values = cbind(thickness = 1:3, myelin = 4:6),
    layers = data.frame(
      layer_id = c("sub-01_thickness", "sub-01_myelin"),
      name = c("thickness", "myelin"),
      measure_id = c("thickness", "myelin"),
      subject_id = "sub-01",
      stringsAsFactors = FALSE
    ),
    measures = rbind(
      ngeo_measure(
        measure_id = "thickness", name = "Cortical thickness",
        unit = "mm", support_behavior = "intensive"
      ),
      ngeo_measure(
        measure_id = "myelin", name = "Myelin proxy",
        unit = "a.u.", support_behavior = "intensive"
      )
    )
  )

  field <- ngeo_layer_view(x, "sub-01_myelin")

  expect_s3_class(field, "ngeo_layer_view")
  expect_named(field, c("base", "values", "measure", "metadata"))
  expect_identical(field$base, ngeo_spatial_base(x))
  expect_equal(as.numeric(field$values), 4:6)
  expect_identical(dim(field$values), c(3L, 1L))
  expect_identical(field$metadata$layer_id, "sub-01_myelin")
  expect_identical(field$metadata$subject_id, "sub-01")
  expect_identical(field$measure$measure_id, "myelin")
  expect_identical(base_hash(field), base_hash(x))
  expect_identical(base_signature(field), base_signature(x))
  expect_identical(ngeo_base_signature(field), base_signature(field))

  expect_error(ngeo_layer_view(x, 1:2), class = "ngeo_error_layer")
})

test_that("relations bind empirical pairwise data to an ordered base", {
  x <- ngeo_point(cbind(x = 0:2, y = 0))
  connectivity <- Matrix::Matrix(
    matrix(c(0, 2, 0, 2, 0, 3, 0, 3, 0), nrow = 3),
    sparse = TRUE
  )
  relation <- ngeo_relation(
    x,
    connectivity,
    type = "structural_connectivity",
    directed = FALSE,
    weighted = TRUE,
    measure = ngeo_measure(
      measure_id = "streamline_count",
      name = "Streamline count",
      value_type = "count",
      support_behavior = "count",
      unit = "streamlines"
    ),
    provenance = list(source = "tractography")
  )

  expect_s3_class(relation, "ngeo_relation")
  expect_named(
    relation,
    c("base", "data", "type", "directed", "weighted", "measure", "provenance")
  )
  expect_identical(base_hash(relation), base_hash(x))
  expect_identical(base_signature(relation), base_signature(x))
  expect_identical(relation$base$element_id, x$base$elements$element_id)
  expect_identical(relation$measure$measure_id, "streamline_count")
  expect_invisible(ngeo_validate_relation(relation, x))

  other <- ngeo_point(cbind(x = 1:3, y = 0))
  expect_error(
    ngeo_validate_relation(relation, other),
    class = "ngeo_error_alignment"
  )
})

test_that("relation edge lists are canonical and exclude analysis objects", {
  x <- ngeo_point(cbind(x = 0:2, y = 0))
  edges <- data.frame(
    from = c(1L, 2L),
    to = c(2L, 3L),
    value = c(-0.2, 0.8)
  )
  relation <- ngeo_relation(
    x, edges, type = "functional_connectivity",
    directed = FALSE, weighted = TRUE
  )

  expect_identical(
    relation$data$from,
    x$base$elements$element_id[c(1L, 2L)]
  )
  expect_identical(
    relation$data$to,
    x$base$elements$element_id[c(2L, 3L)]
  )
  expect_invisible(ngeo_validate_relation(relation))

  expect_error(
    ngeo_relation(x, edges, type = "distance"),
    class = "ngeo_error_relation"
  )
  expect_error(
    ngeo_relation(x, edges, type = "distance matrix "),
    class = "ngeo_error_relation"
  )
  expect_error(
    ngeo_relation(
      x,
      rbind(edges[1, ], transform(edges[1, ], from = 2L, to = 1L)),
      type = "functional_connectivity",
      directed = FALSE
    ),
    class = "ngeo_error_relation"
  )
  expect_error(
    ngeo_relation(
      x, Matrix::Matrix(matrix(1:9, 3), sparse = TRUE),
      type = "effective_connectivity", directed = FALSE
    ),
    class = "ngeo_error_relation"
  )
})

test_that("portable base signatures are stable and label independent", {
  x <- ngeo_point(cbind(x = 0:2, y = 0))
  signature <- base_signature(x)

  expect_match(signature, "^[0-9a-f]{64}$")
  expect_identical(signature, ngeo_base_signature(x$base))

  labelled <- x
  labelled$base$labels <- list(
    group = list(values = c("a", "a", "b"))
  )
  expect_identical(base_signature(labelled), signature)

  moved <- ngeo_point(cbind(x = 1:3, y = 0))
  expect_false(identical(base_signature(moved), signature))
})
