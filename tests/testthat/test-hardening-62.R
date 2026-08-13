hardening_surface_62 <- function() {
  coordinates <- list(
    anatomical = matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3L,
      byrow = TRUE
    ),
    registration = matrix(
      c(0, 0, 0, 2, 0, 0, 0, 2, 0),
      ncol = 3L,
      byrow = TRUE
    )
  )
  ngeo_surface(
    coordinates,
    matrix(c(1L, 2L, 3L), nrow = 1L),
    values = cbind(signal = c(1, 2, 3)),
    measures = ngeo_measure(support_behavior = "intensive"),
    active_coordinates = "anatomical",
    coordinate_space = ngeo_coordinate_space(
      "hardening-surface",
      kind = "surface",
      unit = "mm"
    )
  )
}

test_that("manifest schema 2 binds the active surface coordinates", {
  anatomical <- hardening_surface_62()
  registration <- anatomical
  registration$base$geometry$active_coordinates <- "registration"

  anatomical_manifest <- ngeo_object_manifest(anatomical)
  registration_manifest <- ngeo_object_manifest(registration)

  expect_identical(anatomical_manifest$schema, "NGCS-object-manifest-2")
  expect_identical(
    anatomical_manifest$metadata$active_coordinates,
    "anatomical"
  )
  expect_false(identical(
    anatomical_manifest$metadata$base_sha256,
    registration_manifest$metadata$base_sha256
  ))
  expect_false(identical(
    ngeo_logical_hash(anatomical),
    ngeo_logical_hash(registration)
  ))

  legacy <- anatomical_manifest
  legacy$schema <- "NGCS-object-manifest-1"
  legacy$canonical_sha256 <- neurogeo:::.ngeo_manifest_sha256(legacy)
  expect_true(
    "MANIFEST_SCHEMA" %in% ngeo_validate_manifest(legacy)$issues$code
  )
})

test_that("strict validation rejects mutated core invariants", {
  x <- hardening_surface_62()

  bad_active <- x
  bad_active$base$geometry$active_coordinates <- "missing"
  expect_error(
    ngeo_validate(bad_active, "strict"),
    class = "ngeo_error_geometry"
  )

  bad_space <- x
  bad_space$base$coordinate_space$unit <- NA_character_
  expect_error(
    ngeo_validate(bad_space, "strict"),
    class = "ngeo_error_coordinate_space"
  )

  bad_space_kind <- x
  bad_space_kind$base$coordinate_space$kind <- c("surface", "volume")
  expect_error(
    ngeo_validate(bad_space_kind, "strict"),
    class = "ngeo_error_coordinate_space"
  )

  bad_space_structure <- x
  bad_space_structure$base$coordinate_space <- 42
  expect_error(
    ngeo_validate(bad_space_structure, "strict"),
    class = "ngeo_error_coordinate_space"
  )

  bad_measure <- x
  bad_measure$measures$support_behavior[[1L]] <- "invented"
  expect_error(
    ngeo_validate(bad_measure, "strict"),
    class = "ngeo_error_measure"
  )

  bad_history <- x
  bad_history$history <- 42
  expect_error(
    ngeo_validate(bad_history, "strict"),
    class = "ngeo_error_history"
  )

  bad_operation <- x
  bad_operation$history$operations[[1L]]$software$version <- NA_character_
  expect_error(
    ngeo_validate(bad_operation, "strict"),
    class = "ngeo_error_history"
  )

  bad_timestamp <- x
  bad_timestamp$history$operations[[1L]]$timestamp_utc <- "not-a-timestamp"
  expect_error(
    ngeo_validate(bad_timestamp, "strict"),
    class = "ngeo_error_history"
  )
})

test_that("registered support identities fail closed against their bases", {
  fixture <- diagnostic_fixture()
  changed <- fixture$soft
  changed$source_base_hash <- paste(rep("0", 64L), collapse = "")

  expect_error(
    aggregate_to(
      fixture$source,
      fixture$target,
      changed,
      layers = "outcome"
    ),
    class = "ngeo_error_base_mismatch"
  )
})
