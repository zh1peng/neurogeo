expect_strict_error <- function(x, class) {
  expect_error(ngeo_validate(x, "strict"), class = class)
}

test_that("common object validation detects top-level alignment corruption", {
  x <- ngeo_points(
    matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
    values = cbind(signal = c(1, 2))
  )
  expect_error(ngeo_validate(list()), class = "ngeo_error_schema")

  changed <- x
  changed$domain <- list(elements = changed$domain$elements)
  expect_strict_error(changed, "ngeo_error_domain")

  changed <- x
  changed$domain$elements$element_id[[2L]] <-
    changed$domain$elements$element_id[[1L]]
  expect_strict_error(changed, "ngeo_error_index")

  changed <- x
  changed$values <- list(1, 2)
  expect_strict_error(changed, "ngeo_error_values")

  changed <- x
  changed$values <- matrix(1, nrow = 1, ncol = 1)
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- x
  changed$measures <- changed$measures[FALSE, , drop = FALSE]
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- x
  changed$maps <- rbind(changed$maps, changed$maps)
  changed$measures <- rbind(changed$measures, changed$measures)
  expect_strict_error(changed, "ngeo_error_alignment")
})

test_that("surface validation audits charts, faces, and coordinate metadata", {
  surface <- builder_surface()

  changed <- surface
  changed$domain$coordinates$active <- changed$domain$coordinates$active[-1, ]
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- surface
  changed$domain$coordinate_meta$name <- "wrong"
  expect_strict_error(changed, "ngeo_error_geometry")

  chart <- ngeo_set_chart(
    surface,
    surface$domain$coordinates$active[, 1:2, drop = FALSE],
    name = "flat"
  )
  changed <- chart
  changed$domain$coordinate_meta$metric_eligible[
    changed$domain$coordinate_meta$name == "flat"
  ] <- TRUE
  expect_strict_error(changed, "ngeo_error_chart")

  changed <- chart
  changed$domain$charts <- list(missing = list())
  expect_strict_error(changed, "ngeo_error_chart")

  changed <- surface
  changed$domain$faces[1, 1] <- 99L
  expect_strict_error(changed, "ngeo_error_index")

  changed <- surface
  changed$domain$faces[1, 2] <- changed$domain$faces[1, 1]
  expect_strict_error(changed, "ngeo_error_geometry")

  changed <- surface
  changed$domain$faces <- rbind(
    changed$domain$faces,
    changed$domain$faces[1, ]
  )
  expect_warning(
    ngeo_validate(changed, "strict"),
    class = "ngeo_warning_duplicate_faces"
  )
})

test_that("volume and region validators reject invalid spatial indexing", {
  volume <- ngeo_volume(dim = c(2, 2, 1), affine = diag(4))

  changed <- volume
  changed$domain$dim <- c(2L, 2L)
  expect_strict_error(changed, "ngeo_error_domain")

  changed <- volume
  changed$domain$affine[1, 1] <- 0
  expect_strict_error(changed, "ngeo_error_geometry")

  changed <- volume
  changed$domain$voxel_index <- changed$domain$voxel_index[-1, ]
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- volume
  changed$domain$voxel_index[1, 1] <- 3L
  expect_strict_error(changed, "ngeo_error_index")

  changed <- volume
  changed$domain$voxel_index[2, ] <- changed$domain$voxel_index[1, ]
  expect_strict_error(changed, "ngeo_error_index")

  regions <- ngeo_regions(
    data.frame(region_id = c("A", "B")),
    centroid = matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE),
    adjacency = matrix(c(0, 1, 1, 0), 2)
  )
  changed <- regions
  changed$domain$elements$region_id[[2L]] <- "A"
  expect_strict_error(changed, "ngeo_error_domain")

  changed <- regions
  changed$domain$centroid <- matrix(1, 1, 2)
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- regions
  changed$domain$adjacency <- diag(3)
  expect_strict_error(changed, "ngeo_error_alignment")
})

test_that("grayordinate validation enforces component partition and geometry", {
  surface <- builder_surface()
  grayordinates <- ngeo_grayordinates(list(
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

  changed <- grayordinates
  changed$domain$components[[1L]]$global_rows[[1L]] <- 2L
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- grayordinates
  changed$domain$components[[2L]]$component_id <- "surface"
  expect_strict_error(changed, "ngeo_error_domain")

  changed <- grayordinates
  changed$domain$components[[1L]]$n_element <- 3L
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- grayordinates
  changed$domain$components[[1L]]$vertex_index[[2L]] <-
    changed$domain$components[[1L]]$vertex_index[[1L]]
  expect_strict_error(changed, "ngeo_error_index")

  changed <- grayordinates
  changed$domain$components[[1L]]$surface_vertex_count <- 5L
  expect_strict_error(changed, "ngeo_error_alignment")

  changed <- grayordinates
  changed$domain$components[[2L]]$voxel_index <- matrix(0, nrow = 1, ncol = 2)
  expect_strict_error(changed, "ngeo_error_domain")

  changed <- grayordinates
  changed$domain$components[[2L]]$kind <- "tract"
  expect_strict_error(changed, "ngeo_error_domain")
})
