legacy_space <- function(kind = "unknown") {
  structure(
    list(
      space_id = paste0("legacy-", kind),
      kind = kind,
      units = "mm",
      structure = NULL,
      template = NULL,
      density = NULL,
      resolution = NULL,
      source_metadata = list(scanner = "fixture")
    ),
    class = "ngeo_space"
  )
}

legacy_metadata <- function(n_layer = 2L) {
  maps <- data.frame(
    map_id = paste0("map-", seq_len(n_layer)),
    name = paste0("signal_", seq_len(n_layer)),
    contrast = seq_len(n_layer),
    stringsAsFactors = FALSE
  )
  measures <- data.frame(
    map_id = maps$map_id,
    value_type = rep("continuous", n_layer),
    spatial_semantics = rep("intensive", n_layer),
    units = rep("z", n_layer),
    missing_policy = rep("preserve", n_layer),
    default_aggregation = rep("mean", n_layer),
    stringsAsFactors = FALSE
  )
  list(maps = maps, measures = measures)
}

legacy_object <- function(domain, values = matrix(seq_len(2L * nrow(domain$elements)),
                                                   ncol = 2L),
                          metadata = legacy_metadata()) {
  colnames(values) <- metadata$maps$name
  structure(
    list(
      domain = domain,
      values = values,
      maps = metadata$maps,
      measures = metadata$measures,
      labels = list(),
      provenance = list(spec_version = "2.0", source = "golden fixture")
    ),
    class = c(paste0("legacy_", domain$type), "ngeo")
  )
}

legacy_elements <- function(n) {
  data.frame(
    element_id = sprintf("element_%08d", seq_len(n)),
    source_index = seq_len(n),
    source_index_base = rep(1L, n),
    included = rep(TRUE, n),
    stringsAsFactors = FALSE
  )
}

test_that("5.x point migration preserves order, IDs, semantics, and provenance", {
  domain <- structure(
    list(
      type = "points",
      elements = legacy_elements(3L),
      coordinates = matrix(c(0, 0, 1, 0, 2, 0), ncol = 2L, byrow = TRUE),
      space = legacy_space(),
      uncertainty = c(0.1, 0.2, 0.3)
    ),
    class = c("ngeo_points_domain", "ngeo_domain")
  )
  old <- legacy_object(domain)
  migrated <- ngeo_migrate_5x(old)

  expect_s3_class(migrated, "ngeo_point")
  expect_identical(migrated$values, old$values)
  expect_identical(migrated$base$elements, old$domain$elements)
  expect_identical(migrated$layers$layer_id, old$maps$map_id)
  expect_identical(migrated$layers$contrast, old$maps$contrast)
  expect_equal(nrow(migrated$measures), 1L)
  expect_identical(migrated$layers$measure_id, rep(migrated$measures$measure_id, 2L))
  expect_identical(migrated$measures$support_behavior, "intensive")
  expect_identical(migrated$measures$unit, "z")
  expect_identical(migrated$measures$aggregation, "mean")
  expect_identical(migrated$base$coordinate_space$unit, "mm")
  expect_identical(migrated$history$source_provenance, old$provenance)
  expect_identical(
    migrated$history$operations[[1L]]$operation,
    "ngeo_migrate_5x"
  )
  expect_invisible(ngeo_validate(migrated, "strict"))
})

test_that("5.x surface migration preserves geometry and metadata", {
  coordinates <- list(
    anatomical = matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3L, byrow = TRUE
    )
  )
  domain <- structure(
    list(
      type = "surface",
      elements = legacy_elements(3L),
      coordinates = coordinates,
      coordinate_meta = data.frame(
        name = "anatomical", dimension = 3L, role = "anatomical",
        units = "mm", metric_eligible = TRUE, stringsAsFactors = FALSE
      ),
      active_coordinates = "anatomical",
      faces = matrix(c(1L, 2L, 3L), ncol = 3L),
      face_source_index_base = 1L,
      space = legacy_space("surface"),
      mask = rep(TRUE, 3L)
    ),
    class = c("ngeo_surface_domain", "ngeo_domain")
  )
  migrated <- ngeo_migrate_5x(legacy_object(domain))

  expect_s3_class(migrated, "ngeo_surface")
  expect_identical(migrated$base$geometry$coordinates, coordinates)
  expect_identical(migrated$base$geometry$faces, domain$faces)
  expect_named(migrated$base$geometry$coordinate_meta,
               c("name", "dimension", "role", "unit", "metric_eligible"))
  expect_invisible(ngeo_validate(migrated, "strict"))
})

test_that("5.x volume migration preserves active voxel alignment", {
  dim <- c(2L, 2L, 2L)
  mask <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  voxel_index <- arrayInd(which(mask), .dim = dim)
  domain <- structure(
    list(
      type = "volume",
      elements = legacy_elements(sum(mask)),
      dim = dim,
      affine = diag(4),
      voxel_index = voxel_index,
      source_voxel_index = voxel_index,
      source_index_base = 1L,
      header_transforms = list(qform = diag(4)),
      space = legacy_space("volume"),
      mask = mask
    ),
    class = c("ngeo_volume_domain", "ngeo_domain")
  )
  old <- legacy_object(domain)
  migrated <- ngeo_migrate_5x(old)

  expect_s3_class(migrated, "ngeo_volume")
  expect_identical(migrated$values, old$values)
  expect_identical(migrated$base$geometry$voxel_index, voxel_index)
  expect_identical(migrated$base$geometry$header_transforms,
                   domain$header_transforms)
  expect_invisible(ngeo_validate(migrated, "strict"))
})

test_that("5.x region migration becomes a parcellation", {
  elements <- legacy_elements(2L)
  elements$element_id <- c("region:A", "region:B")
  elements$region_id <- c("A", "B")
  adjacency <- matrix(c(0, 1, 1, 0), nrow = 2L)
  domain <- structure(
    list(
      type = "regions",
      elements = elements,
      base_domain_hash = "legacy-base-hash",
      membership = c("A", "B", "A"),
      centroid = matrix(c(0, 0, 1, 0), ncol = 2L, byrow = TRUE),
      support_size = c(2, 1),
      adjacency = adjacency,
      space = legacy_space()
    ),
    class = c("ngeo_regions_domain", "ngeo_domain")
  )
  migrated <- ngeo_migrate_5x(legacy_object(domain))

  expect_s3_class(migrated, "ngeo_parcellation")
  expect_identical(migrated$base$geometry$source_base_hash,
                   "legacy-base-hash")
  expect_identical(migrated$base$topology$adjacency, adjacency)
  expect_invisible(ngeo_validate(migrated, "strict"))
})

test_that("5.x grayordinate migration preserves component ordering", {
  components <- list(
    left = list(
      kind = "surface", vertex_index = c(0L, 2L),
      internal_vertex_index = c(1L, 3L), surface_vertex_count = 3L,
      source_index_base = 0L, geometry = NULL, n_element = 2L,
      component_id = "left", structure = "CORTEX_LEFT", global_rows = 1:2
    ),
    subcortical = list(
      kind = "volume", voxel_index = matrix(c(1L, 1L, 1L), ncol = 3L),
      affine = diag(4), source_index_base = 1L, n_element = 1L,
      component_id = "subcortical", structure = "THALAMUS_LEFT",
      global_rows = 3L
    )
  )
  elements <- data.frame(
    element_id = c("left:0", "left:1", "subcortical:0"),
    source_index = c(0L, 2L, 1L),
    source_index_base = c(0L, 0L, 1L),
    structure = c("CORTEX_LEFT", "CORTEX_LEFT", "THALAMUS_LEFT"),
    included = TRUE,
    component_id = c("left", "left", "subcortical"),
    component_index = c(1L, 2L, 1L),
    stringsAsFactors = FALSE
  )
  domain <- structure(
    list(
      type = "grayordinates", elements = elements, components = components,
      space = legacy_space("hybrid")
    ),
    class = c("ngeo_grayordinates_domain", "ngeo_domain")
  )
  migrated <- ngeo_migrate_5x(legacy_object(domain))

  expect_s3_class(migrated, "ngeo_grayordinate")
  expect_identical(names(migrated$base$geometry$components), names(components))
  expect_identical(
    migrated$base$geometry$components$subcortical$voxel_index,
    components$subcortical$voxel_index
  )
  expect_invisible(ngeo_validate(migrated, "strict"))
})

test_that("unsupported legacy storage returns an actionable report only", {
  domain <- structure(
    list(
      type = "points", elements = legacy_elements(2L),
      coordinates = matrix(c(0, 0, 1, 1), ncol = 2L, byrow = TRUE),
      space = legacy_space(), uncertainty = NULL
    ),
    class = c("ngeo_points_domain", "ngeo_domain")
  )
  old <- legacy_object(domain)
  old$values <- structure(list(path = "missing.bin"), class = "legacy_file_values")
  report <- ngeo_migrate_5x(old)

  expect_s3_class(report, "ngeo_migration_report")
  expect_identical(report$status, "reconstruction_required")
  expect_null(report$migrated)
  expect_match(report$issues, "re-read", fixed = TRUE)
  expect_output(print(report), "reconstruction_required")
})
