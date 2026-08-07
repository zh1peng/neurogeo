validation_grid_42 <- function(size = 5L) {
  as.matrix(expand.grid(x = 0:(size - 1L), y = 0:(size - 1L)))
}

validation_weights_42 <- function(x, style = "W") {
  ngeo_spatial_weights(
    x,
    method = "distance_band",
    threshold = 1.01,
    style = style
  )
}

test_that("Moran, Geary, and local Moran agree with spdep", {
  skip_if_not_installed("spdep")
  coordinates <- validation_grid_42()
  signal <- sin(coordinates[, 1L]) +
    0.3 * coordinates[, 2L] +
    cos(coordinates[, 1L] * coordinates[, 2L] / 4)
  x <- ngeo_point(coordinates, values = cbind(signal = signal))

  for (style in c("W", "B")) {
    spatial_weights <- validation_weights_42(x, style)
    listw <- as_spdep_listw(spatial_weights)
    n <- length(signal)
    s0 <- spdep::Szero(listw)
    expect_equal(
      ngeo_moran(x, spatial_weights)$estimate,
      spdep::moran(
        signal, listw, n = n, S0 = s0, zero.policy = TRUE
      )$I,
      tolerance = 1e-10
    )
    expect_equal(
      ngeo_geary(x, spatial_weights)$estimate,
      spdep::geary(
        signal,
        listw,
        n = n,
        n1 = n - 1L,
        S0 = s0,
        zero.policy = TRUE
      )$C,
      tolerance = 1e-10
    )
  }

  spatial_weights <- validation_weights_42(x, "W")
  expect_equal(
    ngeo_local_moran(x, spatial_weights)$local_i,
    unname(spdep::localmoran(
      signal,
      as_spdep_listw(spatial_weights),
      zero.policy = TRUE,
      conditional = TRUE,
      mlvar = TRUE
    )[, "Ii"]),
    tolerance = 1e-10
  )
})

test_that("missing values rebuild W normalization on the retained graph", {
  coordinates <- validation_grid_42()
  signal <- sin(seq_len(nrow(coordinates)))
  complete_data <- ngeo_point(
    coordinates,
    values = cbind(signal = signal)
  )
  spatial_weights <- validation_weights_42(complete_data, "W")
  signal[[1L]] <- NA_real_
  missing_data <- ngeo_point(
    coordinates,
    values = cbind(signal = signal)
  )

  input <- neurogeo:::.ngeo_spatial_inputs(
    missing_data,
    spatial_weights,
    map = 1L,
    na_action = "omit",
    zero_policy = TRUE
  )
  positive <- Matrix::rowSums(abs(input$matrix)) > 0
  expect_equal(
    as.numeric(Matrix::rowSums(input$matrix)[positive]),
    rep(1, sum(positive)),
    tolerance = 1e-12
  )

  centered <- input$values - mean(input$values)
  manual <- length(centered) / sum(input$matrix) *
    as.numeric(crossprod(
      centered,
      input$matrix %*% centered
    )) / sum(centered^2)
  observed <- ngeo_moran(
    missing_data,
    spatial_weights,
    na_action = "omit",
    zero_policy = TRUE
  )
  expect_equal(observed$estimate, manual, tolerance = 1e-12)
  expect_identical(observed$omitted, 1L)
})

test_that("isolates, disconnected graphs, and seeds stay explicit", {
  isolate_coordinates <- rbind(
    matrix(
      c(0, 0, 1, 0, 0, 1, 1, 1),
      ncol = 2L,
      byrow = TRUE
    ),
    c(10, 10)
  )
  isolate_data <- ngeo_point(
    isolate_coordinates,
    values = cbind(signal = c(1, 2, 4, 8, 16))
  )
  isolate_weights <- validation_weights_42(isolate_data)
  expect_error(
    ngeo_moran(isolate_data, isolate_weights),
    class = "ngeo_error_zero_policy"
  )
  expect_true(is.finite(ngeo_moran(
    isolate_data,
    isolate_weights,
    zero_policy = TRUE
  )$estimate))

  disconnected_coordinates <- rbind(
    matrix(
      c(0, 0, 1, 0, 0, 1, 1, 1),
      ncol = 2L,
      byrow = TRUE
    ),
    matrix(
      c(10, 0, 11, 0, 10, 1, 11, 1),
      ncol = 2L,
      byrow = TRUE
    )
  )
  disconnected_data <- ngeo_point(
    disconnected_coordinates,
    values = cbind(signal = seq_len(8L))
  )
  disconnected_weights <- validation_weights_42(disconnected_data)
  expect_length(
    unique(ngeo_components(disconnected_weights$raw_matrix)),
    2L
  )
  expect_true(is.finite(ngeo_moran(
    disconnected_data,
    disconnected_weights
  )$estimate))

  first <- ngeo_moran(
    disconnected_data,
    disconnected_weights,
    permutations = 49L,
    seed = 4242
  )
  second <- ngeo_moran(
    disconnected_data,
    disconnected_weights,
    permutations = 49L,
    seed = 4242
  )
  expect_identical(first$simulated, second$simulated)
  expect_identical(first$p.value, second$p.value)
})

test_that("SLX, SAR, and SEM agree with independent references", {
  skip_if_not_installed("spatialreg")
  coordinates <- validation_grid_42()
  predictor <- as.numeric(scale(
    sin(coordinates[, 1L]) + 0.2 * coordinates[, 2L]
  ))
  base <- ngeo_point(
    coordinates,
    values = cbind(predictor = predictor)
  )
  spatial_weights <- validation_weights_42(base)
  matrix <- as.matrix(spatial_weights$matrix)
  design <- cbind(1, predictor)
  set.seed(4201)
  error <- stats::rnorm(nrow(coordinates), sd = 0.35)

  lagged <- as.numeric(spatial_weights$matrix %*% predictor)
  slx_response <- 2 + 3 * predictor + 1.5 * lagged +
    0.01 * sin(seq_along(predictor))
  slx_data <- ngeo_point(
    coordinates,
    values = cbind(
      response = slx_response,
      predictor = predictor
    )
  )
  slx <- ngeo_spatial_lm(
    slx_data,
    "response",
    "predictor",
    spatial_weights,
    model = "slx"
  )
  slx_reference <- stats::lm.fit(
    cbind(1, predictor, lagged),
    slx_response
  )$coefficients
  expect_equal(
    slx$coefficients$estimate,
    unname(slx_reference),
    tolerance = 1e-10
  )

  sar_response <- as.numeric(solve(
    diag(nrow(coordinates)) - 0.35 * matrix,
    design %*% c(1, 1.5) + error
  ))
  sem_response <- as.numeric(
    design %*% c(1, 1.5) +
      solve(
        diag(nrow(coordinates)) - 0.30 * matrix,
        error
      )
  )
  sar_data <- ngeo_point(
    coordinates,
    values = cbind(
      response = sar_response,
      predictor = predictor
    )
  )
  sem_data <- ngeo_point(
    coordinates,
    values = cbind(
      response = sem_response,
      predictor = predictor
    )
  )
  sar <- ngeo_spatial_regression(
    sar_data,
    "response",
    "predictor",
    spatial_weights,
    model = "sar"
  )
  sem <- ngeo_spatial_regression(
    sem_data,
    "response",
    "predictor",
    spatial_weights,
    model = "sem"
  )
  listw <- as_spdep_listw(spatial_weights)
  sar_reference <- spatialreg::lagsarlm(
    response ~ predictor,
    data = data.frame(
      response = sar_response,
      predictor = predictor
    ),
    listw = listw,
    method = "eigen",
    zero.policy = TRUE,
    quiet = TRUE
  )
  sem_reference <- spatialreg::errorsarlm(
    response ~ predictor,
    data = data.frame(
      response = sem_response,
      predictor = predictor
    ),
    listw = listw,
    method = "eigen",
    zero.policy = TRUE,
    quiet = TRUE
  )
  expect_equal(
    sar$spatial_parameter,
    unname(sar_reference$rho),
    tolerance = 1e-6
  )
  expect_equal(
    sar$coefficients$estimate,
    unname(stats::coef(sar_reference)[c(
      "(Intercept)", "predictor"
    )]),
    tolerance = 1e-6
  )
  expect_equal(
    sar$logLik,
    as.numeric(stats::logLik(sar_reference)),
    tolerance = 1e-6
  )
  expect_equal(
    sem$spatial_parameter,
    unname(sem_reference$lambda),
    tolerance = 1e-6
  )
  expect_equal(
    sem$coefficients$estimate,
    unname(stats::coef(sem_reference)[c(
      "(Intercept)", "predictor"
    )]),
    tolerance = 1e-6
  )
  expect_equal(
    sem$logLik,
    as.numeric(stats::logLik(sem_reference)),
    tolerance = 1e-6
  )
})

test_that("spherical variogram and ordinary kriging agree with gstat", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sf")
  coordinates <- matrix(
    c(
      0, 0, 1, 0, 0, 1, 1, 1,
      2, 0, 0, 2, 2, 2, 1, 2
    ),
    ncol = 2L,
    byrow = TRUE
  )
  signal <- sin(coordinates[, 1L]) + cos(coordinates[, 2L])
  x <- ngeo_point(coordinates, values = cbind(signal = signal))
  parameters <- c(nugget = 0.1, partial_sill = 1.2, range = 3)
  fit <- structure(
    list(
      model = "spherical",
      parameters = parameters,
      distance_method = "euclidean",
      base_hash = base_hash(x)
    ),
    class = "ngeo_variogram_fit"
  )
  reference_model <- gstat::vgm(
    psill = parameters[["partial_sill"]],
    model = "Sph",
    range = parameters[["range"]],
    nugget = parameters[["nugget"]]
  )
  distance <- c(0, 0.25, 1, 2.5, 3, 4)
  expect_equal(
    neurogeo:::.ngeo_variogram_curve(
      distance,
      "spherical",
      parameters[["nugget"]],
      parameters[["partial_sill"]],
      parameters[["range"]]
    ),
    gstat::variogramLine(
      reference_model,
      dist_vector = distance
    )$gamma,
    tolerance = 1e-10
  )

  target_2d <- matrix(
    c(0.5, 0.5, 1.5, 1.5),
    ncol = 2L,
    byrow = TRUE
  )
  observed <- ngeo_kriging(
    x,
    "signal",
    fit,
    targets = cbind(target_2d, 0),
    neighbors = nrow(coordinates),
    distance_method = "euclidean"
  )
  training_sf <- sf::st_as_sf(
    data.frame(
      signal = signal,
      x = coordinates[, 1L],
      y = coordinates[, 2L]
    ),
    coords = c("x", "y")
  )
  target_sf <- sf::st_as_sf(
    data.frame(x = target_2d[, 1L], y = target_2d[, 2L]),
    coords = c("x", "y")
  )
  invisible(utils::capture.output(
    reference <- suppressMessages(gstat::krige(
      signal ~ 1,
      training_sf,
      target_sf,
      model = reference_model,
      nmax = nrow(coordinates)
    ))
  ))
  expect_equal(
    observed$prediction,
    reference$var1.pred,
    tolerance = 1e-8
  )
  expect_equal(
    observed$variance,
    reference$var1.var,
    tolerance = 1e-8
  )
})

test_that("Gaussian GWR coefficients agree with GWmodel", {
  skip_if_not_installed("GWmodel")
  skip_if_not_installed("sf")
  coordinates <- validation_grid_42()
  predictor <- sin(coordinates[, 1L]) +
    0.2 * coordinates[, 2L]
  response <- 1 + 2 * predictor +
    0.1 * coordinates[, 1L] * predictor
  x <- ngeo_point(
    coordinates,
    values = cbind(response = response, predictor = predictor)
  )
  observed <- ngeo_gwr(
    x,
    "response",
    "predictor",
    bandwidth = 3,
    distance_method = "euclidean",
    kernel = "gaussian"
  )
  reference_sf <- sf::st_as_sf(
    data.frame(
      response = response,
      predictor = predictor,
      x = coordinates[, 1L],
      y = coordinates[, 2L]
    ),
    coords = c("x", "y")
  )
  reference <- GWmodel::gwr.basic(
    response ~ predictor,
    data = reference_sf,
    bw = 3,
    kernel = "gaussian",
    adaptive = FALSE,
    longlat = FALSE
  )
  reference_data <- sf::st_drop_geometry(reference$SDF)
  expect_equal(
    observed[["(Intercept)"]],
    reference_data$Intercept,
    tolerance = 1e-8
  )
  expect_equal(
    observed$predictor,
    reference_data$predictor,
    tolerance = 1e-8
  )
})
