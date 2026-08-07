expect_strict_error <- function(x, class) {
  expect_error(ngeo_validate(x, "strict"), class = class)
}

test_that("common object validation detects top-level alignment corruption", {
  x <- ngeo_point(
    matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
    values = cbind(signal = c(1, 2))
  )
  expect_error(ngeo_validate(list()), class = "ngeo_error_schema")

  changed <- x
  changed$base <- list(elements = changed$base$elements)
  expect_strict_error(changed, "ngeo_error_base")

  changed <- x
  changed$base$elements$element_id[[2L]] <-
    changed$base$elements$element_id[[1L]]
  expect_strict_error(changed, "ngeo_error_index")

  changed <- x
  changed$values <- list(1, 2)
  expect_strict_error(changed, "ngeo_error_values")

  changed <- x
  changed$values <- matrix(1, nrow = 1, ncol = 1)
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- x
  changed$measures <- changed$measures[FALSE, , drop = FALSE]
  expect_strict_error(changed, "ngeo_error_measure")

  changed <- x
  changed$layers <- rbind(changed$layers, changed$layers)
  expect_strict_error(changed, "ngeo_error_alignment")
})

test_that("surface validation audits charts, faces, and coordinate metadata", {
  surface <- builder_surface()

  changed <- surface
  changed$base$geometry$coordinates$active <- changed$base$geometry$coordinates$active[-1, ]
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- surface
  changed$base$geometry$coordinate_meta$name <- "wrong"
  expect_strict_error(changed, "ngeo_error_geometry")

  chart <- ngeo_set_chart(
    surface,
    surface$base$geometry$coordinates$active[, 1:2, drop = FALSE],
    name = "flat"
  )
  changed <- chart
  changed$base$geometry$coordinate_meta$metric_eligible[
    changed$base$geometry$coordinate_meta$name == "flat"
  ] <- TRUE
  expect_strict_error(changed, "ngeo_error_chart")

  changed <- chart
  changed$base$charts <- list(missing = list())
  expect_strict_error(changed, "ngeo_error_chart")

  changed <- surface
  changed$base$geometry$faces[1, 1] <- 99L
  expect_strict_error(changed, "ngeo_error_index")

  changed <- surface
  changed$base$geometry$faces[1, 2] <- changed$base$geometry$faces[1, 1]
  expect_strict_error(changed, "ngeo_error_geometry")

  changed <- surface
  changed$base$geometry$faces <- rbind(
    changed$base$geometry$faces,
    changed$base$geometry$faces[1, ]
  )
  expect_warning(
    ngeo_validate(changed, "strict"),
    class = "ngeo_warning_duplicate_faces"
  )
})

test_that("volume and region validators reject invalid spatial indexing", {
  volume <- ngeo_volume(dim = c(2, 2, 1), affine = diag(4))

  changed <- volume
  changed$base$geometry$dim <- c(2L, 2L)
  expect_strict_error(changed, "ngeo_error_base")

  changed <- volume
  changed$base$geometry$affine[1, 1] <- 0
  expect_strict_error(changed, "ngeo_error_geometry")

  changed <- volume
  changed$base$geometry$voxel_index <- changed$base$geometry$voxel_index[-1, ]
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- volume
  changed$base$geometry$voxel_index[1, 1] <- 3L
  expect_strict_error(changed, "ngeo_error_index")

  changed <- volume
  changed$base$geometry$voxel_index[2, ] <- changed$base$geometry$voxel_index[1, ]
  expect_strict_error(changed, "ngeo_error_index")

  parcellation <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    centroid = matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
    adjacency = matrix(c(0, 1, 1, 0), 2)
  )
  changed <- parcellation
  changed$base$elements$region_id[[2L]] <- "A"
  expect_strict_error(changed, "ngeo_error_base")

  changed <- parcellation
  changed$base$geometry$centroid <- matrix(1, 1, 2)
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- parcellation
  changed$base$topology$adjacency <- diag(3)
  expect_strict_error(changed, "ngeo_error_alignment")
})

test_that("grayordinate validation enforces component partition and geometry", {
  surface <- builder_surface()
  grayordinate <- ngeo_grayordinate(list(
    list(
      component_id = "surface",
      kind = "surface",
      structure = "CORTEX_LEFT",
      vertex_index = 0:3,
      surface_vertex_count = 4L,
      geometry = surface
    ),
    list(
      component_id = "volume",
      kind = "volume",
      structure = "THALAMUS_LEFT",
      voxel_index = matrix(c(0, 0, 0), nrow = 1),
      affine = diag(4),
      volume_dim = c(2L, 2L, 2L)
    )
  ))

  changed <- grayordinate
  changed$base$geometry$components[[1L]]$global_rows[[1L]] <- 2L
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- grayordinate
  changed$base$geometry$components[[2L]]$component_id <- "surface"
  expect_strict_error(changed, "ngeo_error_base")

  changed <- grayordinate
  changed$base$geometry$components[[1L]]$n_element <- 3L
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- grayordinate
  changed$base$geometry$components[[1L]]$vertex_index[[2L]] <-
    changed$base$geometry$components[[1L]]$vertex_index[[1L]]
  expect_strict_error(changed, "ngeo_error_index")

  changed <- grayordinate
  changed$base$geometry$components[[1L]]$surface_vertex_count <- 5L
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- grayordinate
  changed$base$geometry$components[[2L]]$voxel_index <- matrix(0, nrow = 1, ncol = 2)
  expect_strict_error(changed, "ngeo_error_base")

  changed <- grayordinate
  changed$base$geometry$components[[2L]]$kind <- "tract"
  expect_strict_error(changed, "ngeo_error_base")
})
