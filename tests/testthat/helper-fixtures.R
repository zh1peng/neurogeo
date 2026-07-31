fixture_path <- function(name) {
  installed <- system.file(
    "extdata", "conformance", name,
    package = "neurogeo"
  )
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst", "extdata", "conformance", name)
}

read_fixture <- function(name) {
  testthat::skip_if_not_installed("jsonlite")
  jsonlite::fromJSON(fixture_path(name), simplifyVector = FALSE)
}

golden_path <- function(name) {
  installed <- system.file(
    "extdata", "golden", name,
    package = "neurogeo"
  )
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst", "extdata", "golden", name)
}

rows_to_matrix <- function(x, mode = "numeric") {
  value <- do.call(rbind, x)
  storage.mode(value) <- mode
  value
}

builder_surface <- function(values = NULL, measures = NULL) {
  ngeo_surface(
    coordinates = matrix(
      c(
        0, 0, 0,
        1, 0, 0,
        1, 1, 0,
        0, 1, 0
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    faces = matrix(
      c(1, 2, 3, 1, 3, 4),
      ncol = 3L,
      byrow = TRUE
    ),
    values = values,
    measures = measures,
    space = ngeo_space(
      "registered-square",
      kind = "surface",
      structure = "CORTEX_LEFT"
    )
  )
}

diagnostic_fixture <- function() {
  source <- builder_surface(
    values = cbind(
      outcome = c(2, 4, 6, 8),
      predictor = c(1, 2, 4, 7)
    ),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive"),
      ngeo_measure(spatial_semantics = "intensive")
    )
  )
  hard <- ngeo_atlas_map(source, c("A", "A", "B", "B"))
  probability <- matrix(
    c(
      1, 0,
      0.5, 0.5,
      0.5, 0.5,
      0, 1
    ),
    ncol = 2L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B"))
  )
  soft <- ngeo_probabilistic_atlas_map(
    source,
    probability,
    target = hard$target
  )
  list(source = source, hard = hard, soft = soft, target = hard$target)
}

inference_fixture <- function() {
  coordinates <- cbind(
    x = c(0, 1, 2, 0, 1, 2),
    y = c(0, 0, 0, 1, 1, 1),
    z = 0
  )
  faces <- matrix(
    c(
      1, 2, 5,
      1, 5, 4,
      2, 3, 6,
      2, 6, 5
    ),
    ncol = 3L,
    byrow = TRUE
  )
  predictor <- c(0, 1, 2, 1, 3, 5)
  outcome <- 1 + 2 * predictor + c(0.1, -0.1, 0.2, -0.2, 0.1, -0.1)
  source <- ngeo_surface(
    coordinates,
    faces,
    values = cbind(outcome = outcome, predictor = predictor),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive"),
      ngeo_measure(spatial_semantics = "intensive")
    ),
    space = ngeo_space("inference-grid", kind = "surface")
  )
  atlas_a <- ngeo_atlas_map(
    source,
    c("A", "A", "B", "B", "C", "C")
  )
  atlas_b <- ngeo_atlas_map(
    source,
    c("A", "B", "B", "C", "C", "A"),
    target = atlas_a$target
  )
  list(
    source = source,
    maps = list(A = atlas_a, B = atlas_b),
    targets = list(atlas_a$target, atlas_a$target)
  )
}

model_grid <- function() {
  coordinates <- as.matrix(expand.grid(x = 0:4, y = 0:4))
  predictor <- coordinates[, 1L] + 0.25 * coordinates[, 2L]
  base <- ngeo_points(
    coordinates,
    values = cbind(predictor = predictor)
  )
  weights <- ngeo_weights(
    base,
    method = "distance_band",
    threshold = 1.01,
    style = "W"
  )
  lagged <- as.numeric(weights$matrix %*% predictor)
  set.seed(2026)
  response <- 2 + 3 * predictor + 1.5 * lagged +
    stats::rnorm(length(predictor), sd = 0.02)
  x <- ngeo_points(
    coordinates,
    values = cbind(
      response = response,
      predictor = predictor
    ),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive"),
      ngeo_measure(spatial_semantics = "intensive")
    )
  )
  weights$domain_hash <- ngeo_domain_hash(x)
  list(x = x, weights = weights)
}
qc_cartography_disk <- function() {
  ngeo_surface(
    matrix(
      c(
        -1, -1, 0,
        1, -1, 0,
        1, 1, 0,
        -1, 1, 0,
        0, 0, 0.2
      ),
      ncol = 3L,
      byrow = TRUE
    ),
    matrix(
      c(
        1, 2, 5,
        2, 3, 5,
        3, 4, 5,
        4, 1, 5
      ),
      ncol = 3L,
      byrow = TRUE
    )
  )
}
