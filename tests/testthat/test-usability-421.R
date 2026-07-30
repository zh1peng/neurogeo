test_that("4.2.1 distribution metadata is discoverable", {
  description <- utils::packageDescription("neurogeo")
  expect_gte(
    utils::compareVersion(as.character(description$Version), "4.2.1"),
    0L
  )
  expect_match(description$URL, "github.com/zh1peng/neurogeo", fixed = TRUE)
  expect_match(
    description$BugReports,
    "github.com/zh1peng/neurogeo/issues",
    fixed = TRUE
  )

  citation <- utils::citation("neurogeo")
  expect_gte(length(citation), 1L)
  citation_text <- paste(format(citation[[1L]]), collapse = "\n")
  expect_match(citation_text, "neurogeo", ignore.case = TRUE)
})

test_that("4.2.1 installed API navigation records are present", {
  for (name in c(
    "API-4.2.1.md",
    "migration-4.2.1.md",
    "API-tiers-4.2.1.md"
  )) {
    path <- system.file("spec", name, package = "neurogeo")
    expect_true(nzchar(path), info = name)
    expect_true(file.exists(path), info = name)
  }
})

test_that("declared core help topics contain executable examples", {
  core_topics <- c(
    "ngeo_surface", "ngeo_volume", "ngeo_points", "ngeo_grayordinates",
    "ngeo_regions", "ngeo_space", "ngeo_measure", "ngeo_subset",
    "ngeo_partition", "ngeo_weights", "ngeo_atlas_map",
    "ngeo_probabilistic_atlas_map", "ngeo_support_diagnostics",
    "ngeo_change_support", "ngeo_permutation_control", "ngeo_moran",
    "ngeo_geary", "ngeo_local_moran", "ngeo_variogram",
    "ngeo_fit_variogram", "ngeo_kriging", "ngeo_gwr",
    "ngeo_spatial_lm", "ngeo_spatial_regression", "read_ngeo",
    "ngeo_example_data", "ngeo_validate", "ngeo_object_manifest",
    "ngeo_logical_hash"
  )
  source_man <- testthat::test_path("..", "..", "man")
  database <- if (dir.exists(source_man)) {
    files <- list.files(source_man, pattern = "[.]Rd$", full.names = TRUE)
    stats::setNames(
      lapply(files, tools::parse_Rd),
      basename(files)
    )
  } else {
    tools::Rd_db("neurogeo")
  }
  for (topic in core_topics) {
    name <- paste0(topic, ".Rd")
    expect_true(name %in% names(database), info = topic)
    if (name %in% names(database)) {
      tags <- vapply(
        database[[name]],
        function(x) {
          tag <- attr(x, "Rd_tag")
          if (is.null(tag)) "" else tag
        },
        character(1)
      )
      expect_true("\\examples" %in% tags, info = topic)
    }
  }
})
