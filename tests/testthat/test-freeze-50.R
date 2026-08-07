test_that("5.0 multilayer public surface remains bounded", {
  stable <- c(
    "ngeo_bind_layers", "ngeo_validate_layers", "ngeo_spatial_basis",
    "ngeo_basis_project", "ngeo_layer_coupling", "ngeo_exchangeability",
    "ngeo_group_test"
  )
  exports <- getNamespaceExports("neurogeo")
  expect_true(all(stable %in% exports))
  expect_false("ngeo_spatial_features" %in% exports)
  expect_setequal(
    intersect(exports, c("ngeo_group_test", "ngeo_spatial_features")),
    "ngeo_group_test"
  )
})

test_that("5.0 stable function signatures are frozen", {
  expected <- list(
    ngeo_bind_layers = c("...", "metadata", "source_id", "conflicts", "storage", "budget"),
    ngeo_validate_layers = c("x", "unit", "layer", "required_layers", "complete", "require_consistent_measures"),
    ngeo_spatial_basis = c("x", "spatial_weights", "operator", "coordinates", "support", "n_modes", "components", "symmetrize", "tolerance", "budget"),
    ngeo_basis_project = c("x", "basis", "index", "layers", "bands", "center", "scale", "summaries", "chunk_rows", "chunk_layers"),
    ngeo_layer_coupling = c("x", "index", "pairs", "basis", "bands", "spatial_weights", "estimands", "lag_direction", "energy_floor", "null", "chunk_layers"),
    ngeo_exchangeability = c("unit_id", "scheme", "blocks", "schedule", "permutations", "seed", "budget"),
    ngeo_group_test = c("features", "data", "model", "test", "exchangeability", "family", "transform", "adjustment", "omnibus", "missing", "retain_null", "workers", "budget")
  )
  for (name in names(expected)) {
    expect_identical(names(formals(getExportedValue("neurogeo", name))),
                     expected[[name]], info = name)
  }
})

test_that("5.0 scientific boundary failures keep typed conditions", {
  expect_error(
    ngeo_exchangeability(
      c("s1", "s2"), scheme = "within_block", blocks = c("site", "site"),
      permutations = 1L
    ),
    class = "ngeo_error_exchangeability"
  )
  expect_error(
    ngeo_exchangeability(c("s1", "s1"), permutations = 1L),
    class = "ngeo_error_independent_unit"
  )
})
