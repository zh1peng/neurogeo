partition_surface <- function() {
  coordinates <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      1, 1, 0,
      0, 1, 0
    ),
    ncol = 3L,
    byrow = TRUE
  )
  faces <- matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE)
  values <- cbind(
    intensity = c(1, 2, 3, 4),
    mass = c(1 / 3, 1 / 6, 1 / 3, 1 / 6),
    events = rep.int(1, 4),
    class = c(1, 1, 2, 2),
    unspecified = c(2, 4, 6, 8)
  )
  measures <- do.call(
    rbind,
    list(
      ngeo_measure(support_behavior = "intensive"),
      ngeo_measure(support_behavior = "extensive"),
      ngeo_measure(support_behavior = "count"),
      ngeo_measure(
        value_type = "integer",
        support_behavior = "categorical"
      ),
      ngeo_measure(support_behavior = "unknown")
    )
  )
  ngeo_surface(
    coordinates,
    faces,
    values = values,
    measures = measures
  )
}

test_that("crisp partitions retain explicit background semantics", {
  x <- partition_surface()
  partition <- ngeo_partition(
    x,
    c(0, 1, 1, 2),
    background = 0
  )

  expect_s3_class(partition, "ngeo_partition")
  expect_equal(partition$membership, c(NA, "1", "1", "2"))
  expect_equal(partition$parcellation$region_id, c("1", "2"))
  expect_equal(partition$source_base_hash, base_hash(x))
  expect_error(
    ngeo_partition(
      x,
      c(0, 1, 1, 2),
      background = 0,
      unlabeled_policy = "error"
    ),
    class = "ngeo_error_partition"
  )
})

test_that("region adjacency and boundary derive from base topology", {
  x <- partition_surface()
  partition <- ngeo_partition(x, c("A", "A", "B", "B"))

  adjacency <- ngeo_region_adjacency(x, partition)
  edge_count <- ngeo_region_adjacency(
    x,
    partition,
    weight = "edge_count"
  )
  boundary <- ngeo_boundary(x, partition)

  expect_equal(as.matrix(adjacency), matrix(c(0, 1, 1, 0), 2L))
  expect_equal(as.matrix(edge_count), matrix(c(0, 3, 3, 0), 2L))
  expect_equal(nrow(boundary), 3L)
  expect_setequal(boundary$from_region, "A")
  expect_setequal(boundary$to_region, "B")
})

test_that("aggregation follows measurement semantics and conserves support", {
  x <- partition_surface()
  partition <- ngeo_partition(x, c("A", "A", "B", "B"))

  result <- ngeo_aggregate(
    x,
    partition,
    layers = c("intensity", "mass", "events", "class")
  )

  expect_s3_class(result, "ngeo_parcellation")
  expect_equal(
    result$values[, "intensity"],
    c(4 / 3, 10 / 3),
    tolerance = 1e-12
  )
  expect_equal(result$values[, "mass"], c(1 / 2, 1 / 2))
  expect_equal(sum(result$values[, "mass"]), sum(x$values[, "mass"]))
  expect_equal(result$values[, "events"], c(2, 2))
  expect_equal(result$values[, "class"], c(1, 2))
  expect_equal(result$base$geometry$support_size, c(1 / 2, 1 / 2))
  expect_equal(result$base$geometry$source_base_hash, base_hash(x))
  expect_true(inherits(result$base$topology$adjacency, "Matrix"))
})

test_that("unknown semantics require an explicit aggregation function", {
  x <- partition_surface()
  partition <- ngeo_partition(x, c("A", "A", "B", "B"))

  expect_error(
    ngeo_aggregate(x, partition, layers = "unspecified"),
    class = "ngeo_error_measure_unknown"
  )
  result <- ngeo_aggregate(
    x,
    partition,
    layers = "unspecified",
    fun = stats::median
  )
  expect_equal(result$values[, 1L], c(3, 7))
  expect_equal(result$measures$aggregation, "custom")
})

test_that("partition base hashes prevent accidental reuse", {
  x <- partition_surface()
  partition <- ngeo_partition(x, c("A", "A", "B", "B"))
  shifted <- partition_surface()
  shifted$base$geometry$coordinates$active[, 1L] <-
    shifted$base$geometry$coordinates$active[, 1L] + c(0, 0, 0, 0.1)

  expect_error(
    ngeo_aggregate(shifted, partition),
    class = "ngeo_error_base_mismatch"
  )
})

test_that("volume aggregation uses voxel support", {
  values <- cbind(
    concentration = c(1, 2, 3, 4),
    amount = c(24, 24, 24, 24)
  )
  measures <- rbind(
    ngeo_measure(support_behavior = "intensive"),
    ngeo_measure(support_behavior = "extensive")
  )
  x <- ngeo_volume(
    values = values,
    dim = c(2, 2, 1),
    affine = diag(c(2, 3, 4, 1)),
    measures = measures
  )
  partition <- ngeo_partition(x, c("A", "A", "B", "B"))
  result <- ngeo_aggregate(x, partition)

  expect_equal(result$values[, "concentration"], c(1.5, 3.5))
  expect_equal(result$values[, "amount"], c(48, 48))
  expect_equal(result$base$geometry$support_size, c(48, 48))
})

test_that("supported label readers feed partitions without external binaries", {
  skip_if_not_installed("gifti")
  skip_if_not_installed("freesurferformats")
  skip_if_not_installed("cifti")

  gifti <- read_ngeo_gifti(
    geometry = golden_path("tetra.surf.gii"),
    labels = c(atlas = golden_path("tetra.label.gii")),
    checksum = FALSE
  )
  gifti_partition <- ngeo_partition(gifti, "atlas", background = 0)
  expect_equal(nrow(gifti_partition$parcellation), 1L)
  expect_equal(sum(is.na(gifti_partition$membership)), 2L)

  freesurfer <- read_ngeo_freesurfer(
    geometry = golden_path("tetra.surface"),
    labels = golden_path("tetra.annot"),
    checksum = FALSE
  )
  annot_partition <- ngeo_partition(freesurfer, "annot")
  expect_equal(length(annot_partition$membership), 4L)
  expect_gte(nrow(annot_partition$parcellation), 1L)

  cifti <- read_ngeo_cifti(
    golden_path("tiny.dlabel.nii"),
    checksum = FALSE
  )
  cifti_partition <- ngeo_partition(cifti, "atlas", background = 0)
  expect_equal(nrow(cifti_partition$parcellation), 1L)
  expect_equal(sum(is.na(cifti_partition$membership)), 3L)
})

test_that("partition conformance fixture has language-independent outputs", {
  surface_fixture <- read_fixture("surface-tetrahedron.json")
  partition_fixture <- read_fixture("partition-tetrahedron.json")
  coordinates <- rows_to_matrix(surface_fixture$coordinates)
  faces <- rows_to_matrix(surface_fixture$faces, mode = "integer")
  expected <- partition_fixture$expected
  measures <- rbind(
    ngeo_measure(support_behavior = "intensive"),
    ngeo_measure(support_behavior = "extensive")
  )
  x <- ngeo_surface(
    coordinates,
    faces,
    values = cbind(
      intensity = unlist(surface_fixture$values),
      amount = unlist(surface_fixture$values)
    ),
    measures = measures,
    index_base = "zero"
  )
  partition <- ngeo_partition(x, unlist(partition_fixture$membership))
  result <- ngeo_aggregate(x, partition)

  expect_equal(
    as.matrix(ngeo_region_adjacency(x, partition)),
    rows_to_matrix(expected$region_adjacency)
  )
  expect_equal(
    as.matrix(ngeo_region_adjacency(x, partition, "edge_count")),
    rows_to_matrix(expected$cross_region_edge_count)
  )
  expect_equal(
    result$values[, "intensity"],
    unlist(expected$intensive_aggregation)
  )
  expect_equal(
    result$values[, "amount"],
    unlist(expected$extensive_aggregation)
  )
  expect_equal(
    result$base$geometry$support_size,
    unlist(expected$region_support_size),
    tolerance = partition_fixture$tolerance$absolute
  )
})
