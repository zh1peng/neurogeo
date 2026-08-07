args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "support-builder-conformance.json")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Support-builder conformance requires jsonlite.")
}
if (!exists("ngeo_surface_nearest_map", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}
fixture_dir <- file.path(
  "inst", "extdata", "conformance-ngcs21"
)
read_fixture <- function(name) {
  jsonlite::fromJSON(
    file.path(fixture_dir, name),
    simplifyVector = TRUE
  )
}
assert_close <- function(actual, expected, tolerance, case) {
  if (!isTRUE(all.equal(
    actual,
    expected,
    tolerance = tolerance,
    check.attributes = FALSE
  ))) {
    stop("NGCS 2.1 builder conformance failed: ", case)
  }
}

surface_fixture <- read_fixture("surface-nearest-identity.json")
surface_source <- ngeo_surface(
  surface_fixture$source$coordinates,
  surface_fixture$source$faces,
  coordinate_space = ngeo_coordinate_space(
    surface_fixture$space_id,
    kind = "surface"
  )
)
surface_target <- ngeo_surface(
  surface_fixture$target$coordinates,
  surface_fixture$target$faces,
  coordinate_space = ngeo_coordinate_space(
    surface_fixture$space_id,
    kind = "surface"
  )
)
surface_map <- ngeo_surface_nearest_map(
  surface_source,
  surface_target
)
assert_close(
  as.matrix(surface_map$operator),
  surface_fixture$expected$operator,
  surface_fixture$tolerance,
  surface_fixture$case
)

volume_fixture <- read_fixture("volume-trilinear-half.json")
volume_source <- ngeo_volume(
  dim = volume_fixture$source$dim,
  affine = volume_fixture$source$affine,
  coordinate_space = ngeo_coordinate_space(volume_fixture$space_id, kind = "volume"),
  index_base = volume_fixture$source$index_base
)
volume_target <- ngeo_volume(
  dim = volume_fixture$target$dim,
  affine = volume_fixture$target$affine,
  coordinate_space = ngeo_coordinate_space(volume_fixture$space_id, kind = "volume"),
  index_base = volume_fixture$target$index_base
)
volume_map <- ngeo_affine_grid_map(
  volume_source,
  volume_target,
  method = volume_fixture$method
)
assert_close(
  as.matrix(volume_map$operator),
  volume_fixture$expected$operator,
  volume_fixture$tolerance,
  volume_fixture$case
)

atlas_fixture <- read_fixture("atlas-probabilistic.json")
atlas_source <- ngeo_point(
  atlas_fixture$source$coordinates,
  coordinate_space = ngeo_coordinate_space("atlas-fixture")
)
atlas_map <- ngeo_probabilistic_atlas_map(
  atlas_source,
  atlas_fixture$probabilities_source_by_region,
  region_id = atlas_fixture$region_id,
  source_support = atlas_fixture$source$support
)
assert_close(
  as.matrix(atlas_map$operator),
  atlas_fixture$expected$operator_target_by_source,
  atlas_fixture$tolerance,
  atlas_fixture$case
)
assert_close(
  atlas_map$target_support,
  atlas_fixture$expected$target_support,
  atlas_fixture$tolerance,
  atlas_fixture$case
)

cases <- list(
  surface_nearest_identity = list(
    type = surface_map$type,
    coverage = surface_map$coverage,
    nonzero = length(surface_map$operator@x),
    validation = "passed"
  ),
  volume_trilinear_half = list(
    type = volume_map$type,
    coverage = volume_map$coverage,
    nonzero = length(volume_map$operator@x),
    validation = "passed"
  ),
  atlas_probabilistic = list(
    type = atlas_map$type,
    coverage = atlas_map$coverage,
    nonzero = length(atlas_map$operator@x),
    validation = "passed"
  )
)
if (!all(vapply(
  cases,
  function(case) identical(case$validation, "passed"),
  logical(1)
))) {
  stop("One or more NGCS 2.1 builder fixtures failed.")
}
result <- list(
  specification = "NGCS 2.1 support-builder addendum",
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  validation = "passed",
  cases = cases
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
