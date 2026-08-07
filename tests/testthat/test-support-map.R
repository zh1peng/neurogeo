support_fixture <- function() {
  source <- ngeo_surface(
    matrix(
      c(
        0, 0, 0,
        1, 0, 0,
        1, 1, 0,
        0, 1, 0
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    matrix(c(1, 2, 3, 1, 3, 4), ncol = 3L, byrow = TRUE),
    values = cbind(
      intensity = c(10, 20, 30, 40),
      mass = c(1, 2, 3, 4),
      category = c(1, 1, 2, 2)
    ),
    measures = rbind(
      ngeo_measure(support_behavior = "intensive"),
      ngeo_measure(support_behavior = "extensive"),
      ngeo_measure(
        value_type = "integer",
        support_behavior = "categorical"
      )
    )
  )
  atlas_a <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    support_size = c(3, 3),
    coordinate_space = source$base$coordinate_space
  )
  atlas_b <- ngeo_parcellation(
    data.frame(region_id = c("C", "D")),
    support_size = c(3, 3),
    coordinate_space = source$base$coordinate_space
  )
  support <- c(1, 2, 1, 2)
  map_a <- ngeo_support_map(
    source,
    atlas_a,
    c("A", "A", "B", "B"),
    source_support = support
  )
  map_b <- ngeo_support_map(
    source,
    atlas_b,
    c("C", "D", "D", "C"),
    source_support = support
  )
  list(
    source = source,
    atlas_a = atlas_a,
    atlas_b = atlas_b,
    support = support,
    map_a = map_a,
    map_b = map_b
  )
}

test_that("crisp support layers preserve intensive and extensive semantics", {
  fixture <- support_fixture()
  result <- aggregate_to(
    fixture$source,
    fixture$atlas_a,
    fixture$map_a
  )

  expect_s3_class(fixture$map_a, "ngeo_support_map")
  expect_s3_class(result, "ngeo_parcellation")
  expect_equal(
    result$values[, "intensity"],
    c(50 / 3, 110 / 3)
  )
  expect_equal(result$values[, "mass"], c(3, 7))
  expect_equal(result$values[, "category"], c(1, 2))
  expect_equal(sum(result$values[, "mass"]), sum(fixture$source$values[, "mass"]))
  expect_identical(result$history$spec_version, "2.0")
})

test_that("probabilistic and overlapping operators enforce their invariants", {
  fixture <- support_fixture()
  probabilistic <- rbind(
    c(0.8, 0.2, 0, 0),
    c(0.2, 0.8, 1, 1)
  )
  probabilistic_target <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    support_size = c(1.2, 4.8),
    coordinate_space = fixture$source$base$coordinate_space
  )
  probabilistic_map <- ngeo_support_map(
    fixture$source,
    probabilistic_target,
    probabilistic,
    type = "probabilistic",
    source_support = fixture$support
  )
  overlapping <- rbind(
    c(1, 1, 1, 0),
    c(0, 1, 1, 1)
  )
  overlapping_target <- ngeo_parcellation(
    data.frame(region_id = c("A", "B")),
    support_size = c(4, 5),
    coordinate_space = fixture$source$base$coordinate_space
  )
  overlapping_map <- ngeo_support_map(
    fixture$source,
    overlapping_target,
    overlapping,
    type = "overlapping",
    source_support = fixture$support
  )

  expect_silent(ngeo_validate_support_map(probabilistic_map))
  expect_silent(ngeo_validate_support_map(overlapping_map))
  expect_error(
    aggregate_to(
      fixture$source,
      overlapping_target,
      overlapping_map
    ),
    class = "ngeo_error_conservation"
  )
  normalized <- aggregate_to(
    fixture$source,
    overlapping_target,
    overlapping_map,
    allocation = "normalize"
  )
  expect_equal(
    sum(normalized$values[, "mass"]),
    sum(fixture$source$values[, "mass"])
  )
  invalid <- probabilistic
  invalid[1L, 1L] <- 1.2
  expect_error(
    ngeo_support_map(
      fixture$source,
      probabilistic_target,
      invalid,
      type = "probabilistic",
      source_support = fixture$support
    ),
    class = "ngeo_error_support_map"
  )
})

test_that("support uncertainty follows linear and ratio derivatives", {
  fixture <- support_fixture()
  variance <- ngeo_support_variance(
    fixture$source,
    fixture$atlas_a,
    fixture$map_a,
    value_variance = matrix(1, nrow = 4L, ncol = 2L),
    layers = c("intensity", "mass")
  )

  expect_equal(variance[, "intensity"], rep(5 / 9, 2L))
  expect_equal(variance[, "mass"], rep(2, 2L))

  uncertain <- fixture$map_a
  uncertain$weight_variance <- methods::as(
    fixture$map_a$operator * 0.01,
    "dgCMatrix"
  )
  propagated <- ngeo_support_variance(
    fixture$source,
    fixture$atlas_a,
    uncertain,
    value_variance = rep(1, 4L),
    layers = "intensity"
  )
  expect_true(all(propagated > variance[, "intensity"]))
})

test_that("support layers compose with explicit intermediate domains", {
  fixture <- support_fixture()
  whole <- ngeo_parcellation(
    data.frame(region_id = "whole"),
    support_size = 6,
    coordinate_space = fixture$source$base$coordinate_space
  )
  second <- ngeo_support_map(
    fixture$atlas_a,
    whole,
    c("whole", "whole"),
    source_support = c(3, 3)
  )
  composed <- ngeo_compose_support_map(fixture$map_a, second)
  direct <- aggregate_to(
    fixture$source,
    whole,
    composed
  )

  expect_s3_class(composed, "ngeo_support_map")
  expect_equal(unname(direct$values[, "intensity"]), 80 / 3)
  expect_equal(unname(direct$values[, "mass"]), 10)
})

test_that("cross-atlas overlap and model-based transfer are auditable", {
  fixture <- support_fixture()
  intersection <- ngeo_atlas_overlap(
    fixture$map_a,
    fixture$map_b
  )
  comparison <- ngeo_atlas_compare(
    fixture$map_a,
    fixture$map_b
  )
  transfer <- ngeo_cross_atlas(
    c(1, 3),
    fixture$map_a,
    fixture$map_b,
    semantics = "intensive",
    value_variance = c(1, 1)
  )

  expect_equal(
    unname(as.matrix(intersection)),
    matrix(c(1, 2, 2, 1), 2L)
  )
  expect_equal(as.numeric(transfer$values), c(7 / 3, 5 / 3))
  expect_equal(as.numeric(transfer$variance), rep(5 / 9, 2L))
  expect_identical(transfer$model, "piecewise_constant")
  expect_equal(nrow(comparison), 2L)
  expect_error(
    ngeo_cross_atlas(
      c(1, 3),
      fixture$map_a,
      fixture$map_b,
      model = "implicit_inverse"
    ),
    class = "ngeo_error_model"
  )
})

test_that("global support inference is invariant to crisp parcellation", {
  fixture <- support_fixture()
  inference <- ngeo_parcellation_inference(
    fixture$source,
    support_maps = list(A = fixture$map_a, B = fixture$map_b),
    targets = list(fixture$atlas_a, fixture$atlas_b),
    map = "intensity",
    nsim = 49,
    seed = 2026
  )

  expect_s3_class(inference, "ngeo_parcellation_inference")
  expect_equal(
    inference$estimates$estimate,
    rep(80 / 3, 3L)
  )
  expect_lt(inference$max_deviation, 1e-10)
  expect_equal(length(inference$bootstrap), 49L)
})

test_that("support map hashes bind both source and target domains", {
  fixture <- support_fixture()
  changed <- fixture$atlas_a
  changed$base$elements$element_id[[1L]] <- "changed"
  expect_error(
    aggregate_to(
      fixture$source,
      changed,
      fixture$map_a
    ),
    class = "ngeo_error_base_mismatch"
  )
  expect_identical(
    ngeo_support_map_hash(fixture$map_a),
    ngeo_support_map_hash(fixture$map_a)
  )
})

test_that("NGCS 1.x crisp partitions migrate without changing aggregation", {
  fixture <- support_fixture()
  partition <- ngeo_partition(
    fixture$source,
    c("A", "A", "B", "B")
  )
  old <- ngeo_aggregate(
    fixture$source,
    partition,
    layers = c("intensity", "mass")
  )
  migrated <- ngeo_support_map_from_partition(
    fixture$source,
    partition,
    old
  )
  new <- aggregate_to(
    fixture$source,
    old,
    migrated,
    layers = c("intensity", "mass")
  )

  expect_equal(new$values, old$values)
  expect_identical(migrated$type, "crisp")
})
