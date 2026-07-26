test_that("32k surface topology remains sparse and practical", {
  skip_on_cran()

  side <- 180L
  coordinates <- as.matrix(expand.grid(
    x = seq_len(side),
    y = seq_len(side)
  ))
  coordinates <- cbind(coordinates, 0)
  lower_left <- as.vector(outer(
    seq_len(side - 1L),
    (seq_len(side - 1L) - 1L) * side,
    "+"
  ))
  faces <- rbind(
    cbind(lower_left, lower_left + 1L, lower_left + side),
    cbind(
      lower_left + 1L,
      lower_left + side + 1L,
      lower_left + side
    )
  )
  surface <- ngeo_surface(coordinates, faces)

  timing <- system.time(adjacency <- ngeo_adjacency(surface))

  expect_equal(nrow(adjacency), side^2)
  expect_lt(length(adjacency@x), 7L * side^2)
  expect_lt(unname(timing[["elapsed"]]), 30)
  expect_lt(as.numeric(object.size(adjacency)), 20 * 1024^2)
})

