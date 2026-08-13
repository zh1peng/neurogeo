args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "hardening-62-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The 6.2 hardening validation requires jsonlite.")
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
error_class <- function(expression) {
  condition <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )
  if (is.null(condition)) NA_character_ else class(condition)[[1L]]
}
surface_fixture <- function() {
  coordinates <- list(
    anatomical = matrix(
      c(0, 0, 0, 1, 0, 0, 0, 1, 0),
      ncol = 3L,
      byrow = TRUE
    ),
    registration = matrix(
      c(0, 0, 0, 2, 0, 0, 0, 2, 0),
      ncol = 3L,
      byrow = TRUE
    )
  )
  ngeo_surface(
    coordinates,
    matrix(c(1L, 2L, 3L), nrow = 1L),
    values = cbind(signal = c(1, 2, 3)),
    measures = ngeo_measure(support_behavior = "intensive"),
    active_coordinates = "anatomical",
    coordinate_space = ngeo_coordinate_space(
      "hardening-surface",
      kind = "surface",
      unit = "mm"
    )
  )
}

started <- proc.time()[["elapsed"]]
surface_a <- surface_fixture()
surface_b <- surface_a
surface_b$base$geometry$active_coordinates <- "registration"
manifest_a <- ngeo_object_manifest(surface_a)
manifest_b <- ngeo_object_manifest(surface_b)

identity_checks <- c(
  manifest_schema_advanced = identical(
    manifest_a$schema,
    "NGCS-object-manifest-2"
  ),
  active_coordinate_recorded = identical(
    manifest_a$metadata$active_coordinates,
    "anatomical"
  ),
  base_hash_distinguishes_active_coordinates = !identical(
    ngeo_base_hash(surface_a),
    ngeo_base_hash(surface_b)
  ),
  portable_base_hash_distinguishes_active_coordinates = !identical(
    manifest_a$metadata$base_sha256,
    manifest_b$metadata$base_sha256
  ),
  logical_hash_distinguishes_active_coordinates = !identical(
    ngeo_logical_hash(surface_a),
    ngeo_logical_hash(surface_b)
  )
)
legacy <- manifest_a
legacy$schema <- "NGCS-object-manifest-1"
legacy$canonical_sha256 <- neurogeo:::.ngeo_manifest_sha256(legacy)
identity_checks <- c(
  identity_checks,
  legacy_manifest_rejected = "MANIFEST_SCHEMA" %in%
    ngeo_validate_manifest(legacy)$issues$code
)

mutations <- list()
bad <- surface_a
bad$base$geometry$active_coordinates <- "missing"
mutations$active_coordinate <- error_class(ngeo_validate(bad, "strict"))
bad <- surface_a
bad$base$coordinate_space$unit <- NA_character_
mutations$coordinate_space <- error_class(ngeo_validate(bad, "strict"))
bad <- surface_a
bad$measures$support_behavior[[1L]] <- "invented"
mutations$measure_vocabulary <- error_class(ngeo_validate(bad, "strict"))
bad <- surface_a
bad$history <- 42
mutations$history <- error_class(ngeo_validate(bad, "strict"))
bad <- surface_a
bad$history$operations[[1L]]$timestamp_utc <- "not-a-timestamp"
mutations$provenance <- error_class(ngeo_validate(bad, "strict"))

points <- ngeo_point(
  cbind(x = c(0, 1, 0), y = c(0, 0, 1)),
  values = cbind(signal = c(1, 2, 3)),
  measures = ngeo_measure(support_behavior = "intensive"),
  coordinate_space = ngeo_coordinate_space(
    "hardening-points",
    kind = "unknown",
    unit = "mm"
  )
)
support <- ngeo_support_map(points, points, diag(3L))
support$source_base_hash <- paste(rep("0", 64L), collapse = "")
mutations$support_identity <- error_class(
  aggregate_to(points, points, support, layers = "signal")
)

expected_classes <- c(
  active_coordinate = "ngeo_error_geometry",
  coordinate_space = "ngeo_error_coordinate_space",
  measure_vocabulary = "ngeo_error_measure",
  history = "ngeo_error_history",
  provenance = "ngeo_error_history",
  support_identity = "ngeo_error_base_mismatch"
)
mutation_checks <- unlist(mutations, use.names = TRUE) == expected_classes
assert(all(identity_checks), "A portable identity hardening gate failed.")
assert(all(mutation_checks), "An adversarial strict-validation gate failed.")

result <- list(
  schema = "neurogeo/hardening-62-validation/1",
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(utils::packageVersion("neurogeo")),
  validation = "passed",
  identity = as.list(identity_checks),
  adversarial_mutations = lapply(names(mutations), function(name) {
    list(
      invariant = name,
      observed_error_class = mutations[[name]],
      expected_error_class = expected_classes[[name]],
      rejected = isTRUE(mutation_checks[[name]])
    )
  }),
  elapsed_seconds = proc.time()[["elapsed"]] - started,
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
