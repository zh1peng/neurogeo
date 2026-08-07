test_that("delayed values preserve one aligned block and chunk exactly", {
  backing <- matrix(seq_len(30), nrow = 10L, ncol = 3L)
  colnames(backing) <- c("a", "b", "c")
  delayed <- neurogeo:::.ngeo_delayed_values(
    function(rows, columns) backing[rows, columns, drop = FALSE],
    dim = dim(backing),
    layer_names = c("a", "b", "c")
  )
  x <- ngeo_point(
    cbind(x = 1:10, y = 0),
    values = delayed
  )
  chunks <- ngeo_value_chunks(
    x,
    chunk_size = 4L,
    FUN = function(block, rows) cbind(rows, block)
  )

  expect_s3_class(x$values, "ngeo_delayed_values")
  expect_equal(as.matrix(x$values), backing)
  expect_equal(do.call(rbind, chunks)[, -1L], backing)
  expect_equal(vapply(chunks, nrow, integer(1)), c(4L, 4L, 2L))
})

test_that("binary delayed values read selected cells without full materialization", {
  backing <- matrix(seq_len(30), nrow = 10L, ncol = 3L)
  path <- tempfile(fileext = ".bin")
  on.exit(unlink(path), add = TRUE)
  connection <- file(path, "wb")
  writeBin(as.double(backing), connection, size = 8L)
  close(connection)

  delayed <- neurogeo:::.ngeo_delayed_values(
    path,
    dim = dim(backing),
    layer_names = c("a", "b", "c")
  )

  expect_equal(
    delayed[c(10L, 1L, 5L), c(3L, 1L), drop = FALSE],
    backing[c(10L, 1L, 5L), c(3L, 1L), drop = FALSE],
    ignore_attr = TRUE
  )
  expect_error(
    neurogeo:::.ngeo_delayed_values(path, dim = c(11L, 3L)),
    class = "ngeo_error_alignment"
  )
})

test_that("pure-R CIFTI writer round-trips all supported dense types", {
  skip_if_not_installed("cifti")
  cases <- c("dscalar", "dlabel", "dtseries")
  for (type in cases) {
    source <- read_ngeo_cifti(
      golden_path(paste0("tiny.", type, ".nii")),
      checksum = FALSE
    )
    path <- tempfile(fileext = paste0(".", type, ".nii"))
    write_ngeo_cifti(source, path, type = type)
    restored <- read_ngeo_cifti(path, checksum = FALSE)

    expect_equal(restored$values, source$values, tolerance = 1e-6)
    expect_identical(restored$layers$name, source$layers$name)
    expect_identical(
      restored$base$elements$structure,
      source$base$elements$structure
    )
    expect_equal(
      lapply(restored$base$geometry$components, function(z) {
        if (is.null(z$vertex_index)) z$voxel_index else z$vertex_index
      }),
      lapply(source$base$geometry$components, function(z) {
        if (is.null(z$vertex_index)) z$voxel_index else z$vertex_index
      })
    )
    if (type == "dlabel") {
      expect_identical(
        restored$base$labels[[1L]]$table$Label,
        source$base$labels[[1L]]$table$Label
      )
    }
    if (type == "dtseries") {
      expect_equal(restored$layers$time, source$layers$time)
      expect_identical(restored$layers$time_unit, source$layers$time_unit)
    }
  }
})

test_that("BIDS derivative sidecars retain semantics and history", {
  skip_if_not_installed("cifti")
  skip_if_not_installed("jsonlite")
  x <- read_ngeo_cifti(
    golden_path("tiny.dscalar.nii"),
    checksum = FALSE
  )
  path <- tempfile(fileext = ".dscalar.nii")
  output <- write_ngeo_bids_derivative(
    x,
    path,
    entities = list(space = "fsLR", desc = "effect")
  )
  sidecar <- jsonlite::fromJSON(output[["sidecar"]])

  expect_true(all(file.exists(output)))
  expect_identical(sidecar$DomainHash, base_hash(x))
  expect_identical(sidecar$Entities$space, "fsLR")
  expect_equal(nrow(sidecar$MeasurementSemantics), nrow(x$layers))
})

test_that("delayed contracts reject misalignment", {
  expect_error(
    neurogeo:::.ngeo_delayed_values(
      function(rows, columns) matrix(1, 1, 1),
      c(3, 2)
    )[1:2, 1:2],
    class = "ngeo_error_alignment"
  )
})
