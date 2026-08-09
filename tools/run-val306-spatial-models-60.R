args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--smoke" %in% args) "smoke" else "full"
args <- args[args != "--smoke"]
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val306-spatial-models-60.json")
required <- c(
  "digest", "jsonlite", "pkgload", "spatialreg", "spdep",
  "gstat", "GWmodel", "sf"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-306 requires: ", paste(missing, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
phase3 <- jsonlite::fromJSON(design_path, simplifyVector = FALSE)
validation <- Filter(
  function(x) identical(x$id, "VAL-306"), phase3$validations
)[[1L]]
replicates <- if (identical(run_mode, "full")) {
  phase3$common$replicates_per_calibration_cell
} else {
  500L
}

wilson_interval <- function(successes, attempted, confidence = 0.95) {
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  estimate <- successes / attempted
  denominator <- 1 + z^2 / attempted
  center <- (estimate + z^2 / (2 * attempted)) / denominator
  half_width <- z * sqrt(
    estimate * (1 - estimate) / attempted + z^2 / (4 * attempted^2)
  ) / denominator
  list(
    estimate = estimate, lower = max(0, center - half_width),
    upper = min(1, center + half_width), successes = successes,
    attempted = attempted
  )
}

geometry_coordinates <- function(geometry) {
  grid <- as.matrix(expand.grid(x = 0:4, y = 0:4))
  if (identical(geometry, "irregular-points")) {
    grid[, 1L] <- grid[, 1L] + 0.17 * sin(seq_len(nrow(grid)))
    grid[, 2L] <- grid[, 2L] + 0.13 * cos(seq_len(nrow(grid)))
  }
  grid
}

make_object <- function(geometry, response, predictor) {
  coordinates <- geometry_coordinates(geometry)
  values <- cbind(response = response, predictor = predictor)
  if (identical(geometry, "parcellation")) {
    return(ngeo_parcellation(
      data.frame(region_id = sprintf("region_%02d", seq_len(nrow(coordinates)))),
      values = values, centroid = coordinates,
      coordinate_space = ngeo_coordinate_space(unit = "mm")
    ))
  }
  ngeo_point(
    coordinates, values = values,
    coordinate_space = ngeo_coordinate_space(unit = "mm")
  )
}

metric_for <- function(geometry) {
  if (identical(geometry, "parcellation")) "region_centroid" else "euclidean"
}

make_weights <- function(x, geometry) {
  ngeo_spatial_weights(
    x, method = "knn", k = 4L, symmetry = "union", style = "W",
    distance_method = metric_for(geometry)
  )
}

spherical_structure <- function(distance, range) {
  ratio <- distance / range
  ifelse(ratio < 1, 1.5 * ratio - 0.5 * ratio^3, 1)
}

run_kriging <- function(geometry, parameter, seed) {
  coordinates <- geometry_coordinates(geometry)
  response <- sin(coordinates[, 1L]) + cos(coordinates[, 2L])
  predictor <- coordinates[, 1L] - coordinates[, 2L]
  x <- make_object(geometry, response, predictor)
  object_coordinates <- .ngeo_element_coordinates(x)
  target <- matrix(colMeans(object_coordinates), nrow = 1L)
  target[1L, 1L] <- target[1L, 1L] + 0.11
  range <- 5
  fit <- structure(list(
    model = "spherical",
    parameters = c(
      nugget = 1 - parameter, partial_sill = parameter, range = range
    ),
    distance_method = metric_for(geometry), base_hash = base_hash(x)
  ), class = "ngeo_variogram_fit")
  observed <- ngeo_kriging(
    x, "response", fit, targets = target,
    neighbors = nrow(coordinates), distance_method = metric_for(geometry)
  )
  training_sf <- sf::st_as_sf(data.frame(
    response = response, x = coordinates[, 1L], y = coordinates[, 2L]
  ), coords = c("x", "y"))
  target_sf <- sf::st_as_sf(data.frame(
    x = target[, 1L], y = target[, 2L]
  ), coords = c("x", "y"))
  reference_model <- gstat::vgm(
    psill = parameter, model = "Sph", range = range, nugget = 1 - parameter
  )
  invisible(utils::capture.output(
    reference <- suppressMessages(gstat::krige(
      response ~ 1, training_sf, target_sf, model = reference_model,
      nmax = nrow(coordinates)
    ))
  ))
  reference <- sf::st_drop_geometry(reference)
  estimate_error <- abs(observed$prediction[[1L]] - reference$var1.pred[[1L]])
  se_error <- abs(observed$standard_error[[1L]] - sqrt(reference$var1.var[[1L]]))

  all_coordinates <- rbind(object_coordinates, target)
  distance <- as.matrix(stats::dist(all_coordinates))
  covariance <- parameter * (1 - spherical_structure(distance, range))
  diag(covariance) <- 1
  set.seed(seed)
  simulation <- matrix(
    stats::rnorm(replicates * nrow(covariance)), nrow = replicates
  ) %*% chol(covariance)
  weight <- as.numeric(attr(observed, "linear_weights")[1L, ])
  prediction <- as.numeric(simulation[, seq_along(weight), drop = FALSE] %*% weight)
  truth <- simulation[, ncol(simulation)]
  covered <- abs(truth - prediction) <=
    stats::qnorm(0.975) * observed$standard_error[[1L]]
  coverage <- wilson_interval(sum(covered), replicates)
  coverage_pass <- if (identical(run_mode, "full")) {
    coverage$lower >= 0.93 && coverage$upper <= 0.97
  } else {
    TRUE
  }
  list(
    point_estimate_error = estimate_error,
    standard_error_error = se_error,
    coverage = coverage, type1 = NULL,
    row_order_cv_difference = NULL,
    comparator = "gstat ordinary kriging",
    inferential_boundary = paste(
      "Gaussian conditional prediction coverage is calibrated;",
      "no coefficient hypothesis test is returned."
    ),
    gate_pass = estimate_error <= 1e-6 && se_error <= 1e-6 && coverage_pass
  )
}

run_spatial_regression <- function(method, geometry, parameter, seed) {
  coordinates <- geometry_coordinates(geometry)
  predictor <- as.numeric(scale(
    sin(coordinates[, 1L] / 2) + 0.2 * coordinates[, 2L]
  ))
  template <- make_object(geometry, predictor, predictor)
  weights <- make_weights(template, geometry)
  weight <- as.matrix(weights$matrix)
  model <- cbind(1, predictor)
  set.seed(seed)
  error <- stats::rnorm(length(predictor), sd = 0.35)
  response <- if (identical(method, "sar")) {
    as.numeric(solve(diag(length(predictor)) - parameter * weight,
                     model %*% c(1, 1.5) + error))
  } else {
    as.numeric(model %*% c(1, 1.5) +
      solve(diag(length(predictor)) - parameter * weight, error))
  }
  x <- make_object(geometry, response, predictor)
  observed <- ngeo_spatial_regression(
    x, "response", "predictor", weights, model = method
  )
  data <- data.frame(response = response, predictor = predictor)
  listw <- as_spdep_listw(weights)
  reference <- if (identical(method, "sar")) {
    spatialreg::lagsarlm(
      response ~ predictor, data = data, listw = listw, method = "eigen",
      zero.policy = TRUE, quiet = TRUE
    )
  } else {
    spatialreg::errorsarlm(
      response ~ predictor, data = data, listw = listw, method = "eigen",
      zero.policy = TRUE, quiet = TRUE
    )
  }
  reference_parameter <- if (identical(method, "sar")) reference$rho else
    reference$lambda
  error <- max(abs(c(
    observed$spatial_parameter - reference_parameter,
    observed$coefficients$estimate - stats::coef(reference)[
      c("(Intercept)", "predictor")
    ],
    observed$logLik - as.numeric(stats::logLik(reference))
  )))
  list(
    point_estimate_error = error,
    standard_error_error = NULL, coverage = NULL, type1 = NULL,
    row_order_cv_difference = NULL,
    comparator = paste("spatialreg", toupper(method), "maximum likelihood"),
    inferential_boundary = paste(
      "The stable base result exposes matched maximum-likelihood point",
      "estimates and no calibrated coefficient interval or p-value."
    ),
    gate_pass = error <= 1e-6
  )
}

run_gwr <- function(geometry, parameter) {
  coordinates <- geometry_coordinates(geometry)
  predictor <- sin(coordinates[, 1L]) + 0.2 * coordinates[, 2L]
  response <- 1 + 2 * predictor +
    parameter * coordinates[, 1L] * predictor
  x <- make_object(geometry, response, predictor)
  bandwidth <- 3
  observed <- ngeo_gwr(
    x, "response", "predictor", bandwidth = bandwidth,
    distance_method = metric_for(geometry), kernel = "gaussian"
  )
  reference_sf <- sf::st_as_sf(data.frame(
    response = response, predictor = predictor,
    x = coordinates[, 1L], y = coordinates[, 2L]
  ), coords = c("x", "y"))
  reference <- GWmodel::gwr.basic(
    response ~ predictor, data = reference_sf, bw = bandwidth,
    kernel = "gaussian", adaptive = FALSE, longlat = FALSE
  )
  reference <- sf::st_drop_geometry(reference$SDF)
  estimate_error <- max(abs(c(
    observed[["(Intercept)"]] - reference$Intercept,
    observed$predictor - reference$predictor
  )))
  candidates <- c(1.5, 2.5, 4)
  original_cv <- ngeo_gwr_bandwidth(
    x, "response", "predictor", candidates = candidates,
    distance_method = metric_for(geometry), cv = "kfold", folds = 5L
  )
  ids <- x$base$elements$element_id
  reordered_x <- ngeo_subset(x, elements = rev(ids))
  reordered_cv <- ngeo_gwr_bandwidth(
    reordered_x, "response", "predictor", candidates = candidates,
    distance_method = metric_for(geometry), cv = "kfold", folds = 5L
  )
  order_error <- max(abs(
    original_cv$candidates$rmse - reordered_cv$candidates$rmse
  ))
  fold_equal <- identical(
    original_cv$fold_id[ids], reordered_cv$fold_id[ids]
  )
  list(
    point_estimate_error = estimate_error,
    standard_error_error = NULL, coverage = NULL, type1 = NULL,
    row_order_cv_difference = order_error,
    comparator = "GWmodel single-bandwidth Gaussian GWR",
    inferential_boundary = paste(
      "Local coefficients and spatial-block predictive loss are",
      "descriptive; the base result returns no local p-value map."
    ),
    gate_pass = estimate_error <= 1e-6 && order_error <= 1e-12 && fold_equal
  )
}

run_car <- function(geometry, parameter) {
  coordinates <- geometry_coordinates(geometry)
  response <- sin(coordinates[, 1L]) + cos(coordinates[, 2L])
  predictor <- coordinates[, 1L] - coordinates[, 2L]
  x <- make_object(geometry, response, predictor)
  weights <- make_weights(x, geometry)
  observed <- ngeo_car(
    x, "response", weights, type = "proper", rho = parameter,
    precision = 1
  )
  weight <- as.matrix(weights$matrix)
  weight <- (weight + t(weight)) / 2
  degree <- rowSums(abs(weight))
  precision <- diag(length(response)) + diag(degree) - parameter * weight
  reference <- as.numeric(solve(precision, response))
  error <- max(abs(observed$fitted - reference))
  list(
    point_estimate_error = error,
    standard_error_error = NULL, coverage = NULL, type1 = NULL,
    row_order_cv_difference = NULL,
    comparator = "analytic penalized-smoother precision solve",
    inferential_boundary = paste(
      "This is a penalized spatial smoother, not posterior CAR inference;",
      "the base result returns no credible interval or p-value."
    ),
    gate_pass = error <= 1e-6
  )
}

methods <- unlist(validation$factors$method, use.names = FALSE)
geometries <- unlist(validation$factors$geometry, use.names = FALSE)
parameters <- unlist(validation$factors$spatial_parameter, use.names = FALSE)
cells <- list()
cell_index <- 0L
for (method in methods) {
  for (geometry in geometries) {
    for (parameter in parameters) {
      cell_index <- cell_index + 1L
      metrics <- switch(
        method,
        kriging = run_kriging(
          geometry, parameter, validation$seed_base + cell_index
        ),
        gwr = run_gwr(geometry, parameter),
        sar = run_spatial_regression(
          method, geometry, parameter, validation$seed_base + cell_index
        ),
        sem = run_spatial_regression(
          method, geometry, parameter, validation$seed_base + cell_index
        ),
        car = run_car(geometry, parameter)
      )
      cells[[cell_index]] <- c(list(
        method = method, geometry = geometry, spatial_parameter = parameter,
        seed = validation$seed_base + cell_index,
        replicates_attempted = if (identical(method, "kriging"))
          replicates else 1L,
        failed_fits = 0L, failed_fit_rate = 0,
        type1_applicable = FALSE,
        coverage_applicable = identical(method, "kriging"),
        standard_error_applicable = identical(method, "kriging"),
        row_order_cv_applicable = identical(method, "gwr")
      ), metrics, list(pass = metrics$gate_pass))
    }
  }
}

expected_cells <- length(methods) * length(geometries) * length(parameters)
keys <- vapply(cells, function(cell) paste(
  cell$method, cell$geometry, cell$spatial_parameter, sep = "|"
), character(1))
coverage_complete <- length(cells) == expected_cells && !anyDuplicated(keys)
passed <- coverage_complete && all(vapply(cells, `[[`, logical(1), "pass"))
result <- list(
  schema = "neurogeo/phase3-validation/1",
  validation_id = validation$id, simulation_id = validation$simulation_id,
  design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  dependency_versions = as.list(vapply(
    required, function(package) as.character(utils::packageVersion(package)),
    character(1)
  )),
  platform = R.version$platform, r_version = R.version.string,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  run_mode = run_mode, seed_base = validation$seed_base,
  replicates_per_calibration_cell = replicates,
  primary_evidence_eligible = identical(run_mode, "full") && passed,
  validation = if (passed && identical(run_mode, "full")) {
    "passed-with-inferential-restriction"
  } else if (passed) {
    "debug-passed"
  } else {
    "failed"
  },
  registered_cell_count = expected_cells,
  observed_cell_count = length(cells),
  registered_cell_coverage_complete = coverage_complete,
  coverage_cell_count = sum(vapply(cells, `[[`, logical(1), "coverage_applicable")),
  type1_cell_count = sum(vapply(cells, `[[`, logical(1), "type1_applicable")),
  evidence_boundary = paste(
    "Kriging prediction coverage is calibrated. SAR/SEM, GWR, and CAR",
    "base results are restricted to matched point estimates or descriptive",
    "diagnostics and expose no calibrated coefficient p-values. The frozen",
    "type-I metric is therefore not applicable rather than imputed from a",
    "reference package."
  ),
  cells = unname(cells)
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
