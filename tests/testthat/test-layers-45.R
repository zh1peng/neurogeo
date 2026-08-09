layer_surface <- function(values, subjects, features, delayed = FALSE) {
  layer_names <- paste(subjects, features, sep = "_")
  colnames(values) <- layer_names
  if (isTRUE(delayed)) {
    backing <- values
    values <- neurogeo:::.ngeo_delayed_values(
      function(rows, columns) backing[rows, columns, drop = FALSE],
      dim(backing),
      layer_names = layer_names,
      source = "layer-test-callback"
    )
  }
  ngeo_surface(
    coordinates = matrix(
      c(0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0),
      ncol = 3L,
      byrow = TRUE
    ),
    faces = matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE),
    values = values,
    layers = data.frame(
      layer_id = paste0("map_", seq_along(subjects)),
      name = layer_names,
      subject_id = subjects,
      feature = features,
      stringsAsFactors = FALSE
    ),
    measures = do.call(rbind, lapply(seq_along(subjects), function(i) {
      ngeo_measure(
        support_behavior = "intensive",
        unit = if (features[[i]] == "thickness") "mm" else "ratio"
      )
    })),
    coordinate_space = ngeo_coordinate_space("layer-square", kind = "surface")
  )
}

test_that("layer validation creates a deterministic unit-layer index", {
  subjects <- rep(c("s01", "s02"), each = 2L)
  features <- rep(c("thickness", "myelin"), 2L)
  x <- layer_surface(matrix(seq_len(16), 4L), subjects, features)

  first <- ngeo_validate_layers(
    x,
    required_layers = c("thickness", "myelin"),
    complete = "error"
  )
  second <- ngeo_validate_layers(
    x,
    required_layers = c("thickness", "myelin"),
    complete = "error"
  )

  expect_s3_class(first, "ngeo_layer_index")
  expect_identical(first$index_hash, second$index_hash)
  expect_identical(rownames(first$availability), c("s01", "s02"))
  expect_identical(colnames(first$availability), c("thickness", "myelin"))
  expect_true(all(first$availability))
  expect_identical(first$layer_index$layer_index, 1:4)
})

test_that("layer validation rejects ambiguous or inconsistent metadata", {
  x <- layer_surface(
    matrix(seq_len(16), 4L),
    rep(c("s01", "s02"), each = 2L),
    rep(c("thickness", "myelin"), 2L)
  )

  missing_column <- x
  missing_column$layers$feature <- NULL
  expect_error(
    ngeo_validate_layers(missing_column),
    class = "ngeo_error_layer_metadata"
  )

  duplicate <- x
  duplicate$layers$feature[[2L]] <- "thickness"
  expect_error(
    ngeo_validate_layers(duplicate),
    class = "ngeo_error_layer_duplicate"
  )

  inconsistent <- x
  inconsistent$measures$unit[[3L]] <- "cm"
  expect_error(
    ngeo_validate_layers(inconsistent),
    class = "ngeo_error_layer_measure"
  )

  incomplete <- x
  incomplete$layers$feature[[4L]] <- "other"
  reported <- ngeo_validate_layers(
    incomplete,
    required_layers = c("thickness", "myelin"),
    complete = "report"
  )
  expect_false(reported$availability["s02", "myelin"])
  expect_error(
    ngeo_validate_layers(
      incomplete,
      required_layers = c("thickness", "myelin"),
      complete = "error"
    ),
    class = "ngeo_error_layer_missing"
  )
})

test_that("layer validation supports explicit compound unit keys", {
  x <- layer_surface(
    matrix(seq_len(16), 4L),
    rep(c("s01", "s02"), each = 2L),
    rep(c("thickness", "myelin"), 2L)
  )
  x$layers$session <- rep("baseline", 4L)
  index <- ngeo_validate_layers(
    x,
    unit = c("subject_id", "session"),
    complete = "error"
  )
  expect_equal(nrow(index$unit), 2L)
  expect_true(all(c("subject_id", "session", "unit_id") %in%
    names(index$unit)))
})

test_that("layer binding preserves exact order, semantics, and history", {
  first <- layer_surface(
    matrix(1:8, 4L), c("s01", "s01"), c("thickness", "myelin")
  )
  second <- layer_surface(
    matrix(9:16, 4L), c("s02", "s02"), c("thickness", "myelin")
  )
  second$layers$layer_id <- c("second_1", "second_2")
  first$base$labels$atlas <- list(values = c("A", "A", "B", "B"))
  second$base$labels$network <- list(values = c("X", "Y", "Y", "X"))

  bound <- ngeo_bind_layers(first = first, second = second)

  expect_s3_class(bound, "ngeo_surface")
  expect_equal(bound$values, cbind(first$values, second$values))
  expect_identical(bound$layers$subject_id, c("s01", "s01", "s02", "s02"))
  expect_identical(bound$measures$unit, c("mm", "ratio"))
  expect_identical(
    bound$layers$measure_id,
    c("measure_0001", "measure_0002", "measure_0001", "measure_0002")
  )
  expect_length(bound$history$layer_binding$sources, 2L)
  expect_identical(bound$history$layer_binding$storage, "memory")
  expect_named(bound$base$labels, c("atlas", "network"))
})

test_that("layer binding keeps delayed sources lazy and chunk-equivalent", {
  first <- layer_surface(
    matrix(1:8, 4L), c("s01", "s01"), c("thickness", "myelin"),
    delayed = TRUE
  )
  second <- layer_surface(
    matrix(9:16, 4L), c("s02", "s02"), c("thickness", "myelin")
  )
  second$layers$layer_id <- c("second_1", "second_2")

  bound <- ngeo_bind_layers(first, second, storage = "auto")
  expect_s3_class(bound$values, "ngeo_delayed_values")
  expect_equal(as.matrix(bound$values), cbind(first$values[, ], second$values))

  chunks <- ngeo_value_chunks(bound, chunk_size = 2L)
  expect_equal(rbind(chunks()$values, chunks()$values), as.matrix(bound$values))
})

test_that("layer binding rejects hidden alignment and ID conflicts", {
  first <- layer_surface(
    matrix(1:8, 4L), c("s01", "s01"), c("thickness", "myelin")
  )
  second <- layer_surface(
    matrix(9:16, 4L), c("s02", "s02"), c("thickness", "myelin")
  )

  expect_error(
    ngeo_bind_layers(first, second),
    class = "ngeo_error_layer_conflict"
  )
  prefixed <- ngeo_bind_layers(
    first = first,
    second = second,
    conflicts = "prefix"
  )
  expect_identical(anyDuplicated(prefixed$layers$layer_id), 0L)
  expect_match(prefixed$layers$layer_id[[1L]], "^first::")

  shifted <- second
  shifted$base$coordinate_space <- ngeo_coordinate_space("different", kind = "surface")
  expect_error(
    ngeo_bind_layers(first, shifted, conflicts = "prefix"),
    class = "ngeo_error_base_mismatch"
  )

  expect_error(
    ngeo_bind_layers(
      first, second,
      conflicts = "prefix",
      storage = "memory",
      budget = ngeo_resource_budget(memory_bytes = 1)
    ),
    class = "ngeo_error_resource"
  )
})

test_that("map binding metadata aligns exactly with output columns", {
  first <- layer_surface(
    matrix(1:8, 4L), c("s01", "s01"), c("thickness", "myelin")
  )
  second <- layer_surface(
    matrix(9:16, 4L), c("s02", "s02"), c("thickness", "myelin")
  )
  metadata <- data.frame(
    cohort = rep("discovery", 4L),
    stringsAsFactors = FALSE
  )
  bound <- ngeo_bind_layers(
    first = first,
    second = second,
    conflicts = "prefix",
    metadata = metadata
  )
  expect_identical(bound$layers$cohort, metadata$cohort)
  expect_error(
    ngeo_bind_layers(
      first = first,
      second = second,
      conflicts = "prefix",
      metadata = metadata[-1L, ]
    ),
    class = "ngeo_error_alignment"
  )
})

test_that("file-backed map binding preserves mutation verification", {
  paths <- c(tempfile(), tempfile())
  on.exit(unlink(paths), add = TRUE)
  writeBin(as.raw(1:4), paths[[1L]])
  writeBin(as.raw(5:8), paths[[2L]])
  selection <- list(
    layout = "volume",
    element_index = 0:3,
    layer_index = 0L,
    full_element_count = 4L
  )
  binary <- list(
    what = "raw", bytes = 1L, signed = FALSE,
    endian = "little", data_offset = 0, compressed = FALSE
  )
  make_source <- function(path, subject, layer_id) {
    values <- ngeo_file_values(
      path, c(4, 1), paste0(subject, "_thickness"), "test",
      selection, binary, verify = "metadata"
    )
    ngeo_point(
      cbind(x = 1:4, y = 0),
      values = values,
      layers = data.frame(
        layer_id = layer_id,
        name = paste0(subject, "_thickness"),
        subject_id = subject,
        feature = "thickness"
      ),
      measures = ngeo_measure(
        support_behavior = "intensive", unit = "mm"
      )
    )
  }
  first <- make_source(paths[[1L]], "s01", "first")
  second <- make_source(paths[[2L]], "s02", "second")
  bound <- ngeo_bind_layers(first, second)
  expect_s3_class(bound$values, "ngeo_delayed_values")
  expect_equal(
    as.matrix(bound$values),
    matrix(as.numeric(1:8), 4L, 2L),
    ignore_attr = TRUE
  )

  connection <- file(paths[[1L]], "ab")
  writeBin(as.raw(9), connection)
  close(connection)
  expect_error(
    bound$values[1:2, 1L, drop = FALSE],
    class = "ngeo_error_file_mutation"
  )
})
