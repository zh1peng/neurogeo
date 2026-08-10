test_that("DK tutorial cohort preserves subject and parcel semantics", {
  skip_if_not_installed("ggseg")
  skip_if_not_installed("sf")
  sys.source(
    system.file(
      "tutorial-code", "brain-case-study.R",
      package = "neurogeo", mustWork = TRUE
    ),
    envir = environment()
  )

  dk <- ngeo_tutorial_dk_case_control(n_per_group = 100, seed = 20260810)
  expect_s3_class(dk$cohort, "ngeo_parcellation")
  expect_equal(dim(ngeo_values(dk$cohort)), c(68L, 200L))
  expect_equal(as.integer(table(dk$design$group)), c(100L, 100L))
  expect_equal(nrow(ngeo_values(dk$difference)), 68L)
  expect_true(all(dk$adjacency == t(dk$adjacency)))
  expect_false(any(diag(dk$adjacency)))
  expect_silent(ngeo_validate(dk$cohort, "strict"))
})

test_that("vertex tutorial data expose the declared support family", {
  sys.source(
    system.file(
      "tutorial-code", "brain-case-study.R",
      package = "neurogeo", mustWork = TRUE
    ),
    envir = environment()
  )

  vertex <- ngeo_tutorial_vertex_case_control(n_per_group = 5)
  expect_s3_class(vertex$surface, "ngeo_surface")
  expect_equal(dim(ngeo_values(vertex$surface)), c(672L, 10L))
  expect_equal(as.integer(table(vertex$group)), c(5L, 5L))
  expect_equal(
    vapply(vertex$supports, function(x) length(unique(x)), integer(1)),
    c(DK68 = 68L, Schaefer100 = 100L, Schaefer200 = 200L,
      Schaefer300 = 300L, Glasser360 = 360L)
  )
  expect_silent(ngeo_validate(vertex$surface, "strict"))
  expect_silent(ngeo_validate(vertex$difference, "strict"))
})
