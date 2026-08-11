.source_brain_case_study <- function(envir = parent.frame()) {
  sys.source(
    system.file(
      "tutorial-code", "brain-case-study.R",
      package = "neurogeo", mustWork = TRUE
    ),
    envir = envir
  )
}

test_that("synthetic tutorial fallback is explicit and atlas-sized", {
  withr::local_envvar(NEUROGEO_TUTORIAL_DATA_MODE = "synthetic")
  .source_brain_case_study(environment())

  dk <- ngeo_tutorial_dk_case_control(n_per_group = 100, seed = 20260810)
  expect_s3_class(dk$cohort, "ngeo_parcellation")
  expect_equal(dim(ngeo_values(dk$cohort)), c(68L, 200L))
  expect_equal(as.integer(table(dk$design$group)), c(100L, 100L))
  observed_difference <- ngeo_values(dk$difference)[, 1L]
  expect_gt(stats::cor(observed_difference, dk$truth), 0.7)
  expect_lt(
    mean(observed_difference[dk$truth_components$primary]),
    mean(observed_difference[
      !dk$truth_components$primary & !dk$truth_components$neighbor
    ]) - 0.05
  )
  expect_identical(dk$data_source, "synthetic_fallback")
  expect_false(dk$published_atlases)
  expect_length(dk$underlay, 5124L)
  expect_length(dk$mask, 5124L)
  expect_match(dk$support_note, "not published", fixed = TRUE)
  expect_true(all(is.finite(dk$centroids)))
  expect_true(all(dk$support_size > 0L))
  expect_true(all(dk$adjacency == t(dk$adjacency)))
  expect_false(any(diag(dk$adjacency)))
  expect_silent(ngeo_validate(dk$cohort, "strict"))

  flat_map <- ngeo_cortical_map(
    dk$surface,
    values = dk$difference,
    layer = "SCZ_minus_HC",
    chart = "flat",
    atlas = "DK68",
    underlay = dk$underlay,
    palette = "Blue-Red 3",
    na_color = NA_character_
  )
  expect_s3_class(flat_map, "ngeo_cortical_map")
  expect_equal(nrow(ngeo_cortical_map_data(flat_map)$vertices), 5124L)
  expect_identical(flat_map$atlas_coverage, "labeled")
})

test_that("synthetic vertex fallback exposes every declared support", {
  withr::local_envvar(NEUROGEO_TUTORIAL_DATA_MODE = "synthetic")
  .source_brain_case_study(environment())

  vertex <- ngeo_tutorial_vertex_case_control(n_per_group = 5)
  expect_s3_class(vertex$surface, "ngeo_surface")
  expect_equal(dim(ngeo_values(vertex$surface)), c(5124L, 10L))
  expect_equal(
    nrow(ngeo_spatial_base(vertex$surface)$geometry$faces),
    10240L
  )
  expect_equal(vertex$n_vertex_per_hemi, 2562L)
  expect_equal(as.integer(table(vertex$group)), c(5L, 5L))
  vertex_difference <- ngeo_values(vertex$difference)[, 1L]
  expect_lt(
    mean(vertex_difference[vertex$primary & vertex$mask], na.rm = TRUE),
    mean(vertex_difference[
      !vertex$primary & !vertex$neighbor & vertex$mask
    ], na.rm = TRUE) - 0.04
  )
  expect_equal(
    vapply(vertex$supports, function(x) {
      length(unique(stats::na.omit(x)))
    }, integer(1)),
    c(
      DK68 = 68L, Schaefer100 = 100L, Schaefer200 = 200L,
      Schaefer300 = 300L, Glasser360 = 360L
    )
  )
  expect_identical(vertex$data_source, "synthetic_fallback")
  expect_false(vertex$published_atlases)
  expect_true(sum(vertex$mask) < length(vertex$mask))
  expect_true(all(is.finite(ngeo_values(vertex$surface))))
  expect_silent(ngeo_validate(vertex$surface, "strict"))
  expect_silent(ngeo_validate(vertex$difference, "strict"))

  atlas_map <- ngeo_cortical_map(
    vertex$surface,
    chart = "flat",
    atlas = "Schaefer200",
    fill = "atlas",
    underlay = vertex$underlay,
    palette = "Dynamic",
    na_color = NA_character_
  )
  expect_s3_class(atlas_map, "ngeo_cortical_map")
  expect_identical(atlas_map$atlas_coverage, "labeled")
  maps <- ngeo_tutorial_support_maps(vertex)
  expect_equal(
    vapply(maps, function(x) nrow(x$operator), integer(1)),
    c(
      DK68 = 68L, Schaefer100 = 100L, Schaefer200 = 200L,
      Schaefer300 = 300L, Glasser360 = 360L
    )
  )

  schaefer100 <- aggregate_to(
    vertex$difference,
    maps$Schaefer100$target,
    maps$Schaefer100
  )
  parcel_map <- ngeo_cortical_map(
    vertex$surface,
    values = schaefer100,
    layer = 1L,
    chart = "flat",
    atlas = "Schaefer100",
    underlay = vertex$underlay,
    palette = "Blue-Red 3",
    na_color = NA_character_
  )
  expect_s3_class(parcel_map, "ngeo_cortical_map")
  expect_identical(parcel_map$atlas_coverage, "labeled")

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit({
    grDevices::dev.off()
    unlink(plot_file)
  }, add = TRUE)
  expect_silent(
    graphics::plot(
      parcel_map,
      show_boundaries = TRUE,
      boundary_color = grDevices::adjustcolor("white", 0.55),
      boundary_lwd = 0.25,
      show_outline = TRUE,
      outline_lwd = 1.1,
      show_labels = FALSE,
      show_legend = TRUE
    )
  )
})

test_that("tutorial cache is shared across page source environments", {
  withr::local_envvar(NEUROGEO_TUTORIAL_DATA_MODE = "synthetic")
  first <- new.env(parent = globalenv())
  second <- new.env(parent = globalenv())
  .source_brain_case_study(first)
  .source_brain_case_study(second)

  expect_identical(first$.ngeo_tutorial_cache, second$.ngeo_tutorial_cache)
  expect_identical(
    first$ngeo_tutorial_vertex_case_control(mode = "synthetic"),
    second$ngeo_tutorial_vertex_case_control(mode = "synthetic")
  )
})

test_that("verified real fixtures preserve Conte69 and atlas contracts", {
  skip_if_not_installed("gifti")
  repository <- normalizePath(
    testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE
  )
  withr::local_envvar(c(
    NEUROGEO_TUTORIAL_FLATMAP_CACHE = file.path(
      repository, ".tools", "reference-flatmap"
    ),
    NEUROGEO_TUTORIAL_REFERENCE50_CACHE = file.path(
      repository, ".tools", "reference-5.0"
    )
  ))
  .source_brain_case_study(environment())
  fixture <- try(.ngeo_tutorial_fixture_paths(), silent = TRUE)
  skip_if(inherits(fixture, "try-error") || is.null(fixture),
          "verified real tutorial fixtures are unavailable")
  withr::local_envvar(NEUROGEO_TUTORIAL_DATA_MODE = "real")

  vertex <- ngeo_tutorial_vertex_case_control(
    n_per_group = 5, seed = 20260811, mode = "real"
  )
  core <- .ngeo_tutorial_surface_core("real")
  dk <- ngeo_tutorial_dk_case_control(
    n_per_group = 100, seed = 20260810, mode = "real"
  )
  expect_identical(vertex$data_source, "real_conte69")
  expect_true(vertex$published_atlases)
  expect_equal(dim(ngeo_values(vertex$surface)), c(64984L, 10L))
  expect_equal(nrow(core$faces), 129960L)
  expect_equal(nrow(core$flat_faces), 117927L)
  expect_equal(sum(core$mask), 59412L)
  expect_length(dk$underlay, 64984L)
  expect_equal(vertex$n_vertex_per_hemi, 32492L)
  expect_equal(
    core$support_counts,
    c(
      DK68 = 68L, Schaefer100 = 100L, Schaefer200 = 200L,
      Schaefer300 = 300L, Glasser360 = 360L
    )
  )
  expect_equal(
    vertex$atlas_corrections$Glasser360_right_code_180_excluded,
    63L
  )
  flat_chart <- ngeo_spatial_base(vertex$surface)$charts$flat
  expect_true(flat_chart$invariants$imported)
  expect_true(flat_chart$invariants$topology_verified)
  expect_identical(flat_chart$invariants$topology_relation, "face_subset")
  expect_identical(flat_chart$method, "imported")
  expect_identical(flat_chart$kind, "parameterization")
  expect_equal(sum(flat_chart$distortion$charted), 117927L)
  expect_silent(ngeo_validate(vertex$surface, "strict"))
  expect_silent(ngeo_validate(vertex$difference, "strict"))

  effect_map <- ngeo_cortical_map(
    vertex$difference,
    layer = 1L,
    chart = "flat",
    atlas = "DK68",
    underlay = vertex$underlay,
    palette = "Blue-Red 3",
    na_color = NA_character_
  )
  expect_s3_class(effect_map, "ngeo_cortical_map")
  expect_equal(nrow(ngeo_cortical_map_data(effect_map)$vertices), 64984L)
  expect_identical(effect_map$atlas_coverage, "surface")

  parcel_map <- ngeo_cortical_map(
    dk$surface,
    values = dk$difference,
    layer = 1L,
    chart = "flat",
    atlas = "DK68",
    underlay = dk$underlay,
    na_color = NA_character_
  )
  expect_identical(parcel_map$atlas_coverage, "labeled")
  expect_identical(parcel_map$history$atlas_coverage_requested, "auto")
  expect_equal(sum(parcel_map$vertices$included), 56206L)
  expect_equal(sum(parcel_map$face_data$included), 110751L)
  expect_identical(
    parcel_map$history$atlas_unlabeled_vertices,
    3206L
  )
})
