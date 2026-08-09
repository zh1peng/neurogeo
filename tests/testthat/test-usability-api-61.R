test_that("prefixed accessors preserve the 6.x compatibility accessors", {
  x <- ngeo_point(cbind(x = 0:2, y = 0), values = cbind(signal = 1:3))

  expect_identical(ngeo_spatial_base(x), spatial_base(x))
  expect_identical(ngeo_base_elements(x), base_elements(x))
  expect_identical(ngeo_values(x), values(x))
  expect_identical(ngeo_layers(x), layers(x))
  expect_identical(ngeo_measures(x), measures(x))
  expect_identical(ngeo_history(x), history(x))
  expect_identical(ngeo_base_type(x), base_type(x))
  expect_identical(ngeo_base_hash(x), base_hash(x))
})

test_that("layer index uses feature terminology without breaking the old name", {
  x <- ngeo_point(
    cbind(x = 0:1, y = 0),
    values = cbind(a = 1:2, b = 3:4),
    layers = data.frame(
      layer_id = c("s1-thickness", "s1-area"),
      name = c("thickness", "area"),
      subject_id = c("s1", "s1"),
      feature = c("thickness", "area")
    )
  )
  current <- ngeo_layer_index(x, required_features = c("thickness", "area"))
  legacy <- ngeo_validate_layers(
    x, required_layers = c("thickness", "area")
  )

  expect_identical(current, legacy)
})

test_that("measure IDs and safe updates are explicit and audited", {
  measure <- ngeo_measure(
    measure_id = "cortical_thickness",
    name = "Cortical thickness",
    support_behavior = "intensive",
    unit = "mm"
  )
  x <- ngeo_point(
    cbind(x = 0:2, y = 0),
    values = cbind(thickness = 1:3),
    layers = data.frame(
      layer_id = "subject-01",
      name = "thickness",
      measure_id = "cortical_thickness"
    ),
    measures = measure
  )
  before <- length(history(x)$operations)
  updated <- ngeo_update_measure(
    x, "cortical_thickness", name = "Mean cortical thickness"
  )

  expect_identical(measures(x)$name, "Cortical thickness")
  expect_identical(measures(updated)$name, "Mean cortical thickness")
  expect_equal(length(history(updated)$operations), before + 1L)
  expect_silent(ngeo_validate(updated, "strict"))
  expect_error(
    ngeo_update_measure(x, "missing", unit = "cm"),
    class = "ngeo_error_measure"
  )
})

test_that("unknown coordinate spaces do not claim millimetre units", {
  expect_identical(ngeo_coordinate_space()$unit, "unknown")
  expect_identical(
    ngeo_coordinate_space("MNI152", kind = "volume", unit = "mm")$unit,
    "mm"
  )
})
