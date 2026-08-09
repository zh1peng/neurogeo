args <- commandArgs(trailingOnly = TRUE)
run_mode <- if ("--smoke" %in% args) "smoke" else "full"
args <- args[args != "--smoke"]
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "val307-support-builder-60.json")
required <- c("digest", "jsonlite", "Matrix", "dbscan", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("VAL-307 requires: ", paste(missing, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))

design_path <- file.path("inst", "validation", "phase3-design-6.0.json")
design_hash <- digest::digest(
  design_path, algo = "sha256", file = TRUE, serialize = FALSE
)
locked_hash <- trimws(readLines(
  file.path("inst", "validation", "phase3-design-6.0.sha256"), warn = FALSE
))
stopifnot(identical(design_hash, locked_hash))
design <- jsonlite::fromJSON(design_path, simplifyVector = FALSE)
validation <- Filter(
  function(x) identical(x$id, "VAL-307"), design$validations
)[[1L]]

surface_dimensions <- list(
  `1000` = c(25L, 40L),
  `32000` = c(160L, 200L),
  `91000` = c(280L, 325L)
)
volume_dimensions <- list(
  `1000` = c(10L, 10L, 10L),
  `32000` = c(40L, 40L, 20L),
  `91000` = c(70L, 50L, 26L)
)

surface_grid <- function(size, kind) {
  dimensions <- surface_dimensions[[as.character(size)]]
  side <- as.integer(sqrt(size))
  if (is.null(dimensions) && side * side == size) {
    dimensions <- c(side, side)
  }
  if (is.null(dimensions) || prod(dimensions) != size) {
    stop("No exact surface dimensions are registered for size ", size, ".")
  }
  nr <- dimensions[[1L]]
  nc <- dimensions[[2L]]
  x_axis <- seq_len(nc)
  if (identical(kind, "surface-skinny-faces")) {
    x_axis <- cumsum(rep(c(1, 1e-4), length.out = nc))
  }
  coordinates <- cbind(
    x = rep(x_axis, each = nr),
    y = rep(seq_len(nr), times = nc),
    z = 0
  )
  lower_left <- as.vector(outer(
    seq_len(nr - 1L),
    (seq_len(nc - 1L) - 1L) * nr,
    "+"
  ))
  faces <- matrix(0L, nrow = 2L * length(lower_left), ncol = 3L)
  faces[seq.int(1L, nrow(faces), by = 2L), ] <- cbind(
    lower_left, lower_left + nr, lower_left + nr + 1L
  )
  faces[seq.int(2L, nrow(faces), by = 2L), ] <- cbind(
    lower_left, lower_left + nr + 1L, lower_left + 1L
  )
  if (identical(kind, "surface-disconnected")) {
    split <- x_axis[[floor(nc / 2L)]]
    face_x <- matrix(coordinates[faces, 1L], ncol = 3L)
    faces <- faces[
      apply(face_x <= split, 1L, all) | apply(face_x > split, 1L, all),
      , drop = FALSE
    ]
    coordinates[coordinates[, 1L] > split, 1L] <-
      coordinates[coordinates[, 1L] > split, 1L] + 20
  }
  mask <- rep.int(TRUE, nrow(coordinates))
  if (identical(kind, "surface-medial-wall")) {
    x_range <- range(coordinates[, 1L])
    y_range <- range(coordinates[, 2L])
    mask <- !(
      coordinates[, 1L] > x_range[[1L]] + 0.45 * diff(x_range) &
        coordinates[, 1L] < x_range[[1L]] + 0.55 * diff(x_range) &
        coordinates[, 2L] > y_range[[1L]] + 0.25 * diff(y_range) &
        coordinates[, 2L] < y_range[[1L]] + 0.75 * diff(y_range)
    )
  }
  list(coordinates = coordinates, faces = faces, mask = mask)
}

surface_queries <- function(target_grid, count, seed) {
  set.seed(seed)
  eligible <- apply(
    matrix(target_grid$mask[target_grid$faces], ncol = 3L),
    1L,
    all
  )
  eligible_faces <- which(eligible)
  selected <- eligible_faces[unique(round(seq(
    1, length(eligible_faces), length.out = count
  )))]
  if (length(selected) != count) {
    stop("Could not select the registered number of surface queries.")
  }
  weights <- cbind(
    0.15 + 0.20 * stats::runif(count),
    0.20 + 0.20 * stats::runif(count)
  )
  weights <- cbind(weights, 1 - rowSums(weights))
  vertices <- target_grid$faces[selected, , drop = FALSE]
  coordinates <-
    target_grid$coordinates[vertices[, 1L], , drop = FALSE] * weights[, 1L] +
    target_grid$coordinates[vertices[, 2L], , drop = FALSE] * weights[, 2L] +
    target_grid$coordinates[vertices[, 3L], , drop = FALSE] * weights[, 3L]
  side <- as.integer(sqrt(count))
  if (side * side != count) stop("Surface query count must be a square.")
  query_grid <- surface_grid(side * side, "surface-regular")
  query_grid$coordinates <- coordinates
  query_grid
}

map_surface <- function(source, target, candidate_faces, exact = FALSE) {
  old <- options(neurogeo.max_exact_mapping_pairs = if (exact) Inf else 0)
  on.exit(options(old), add = TRUE)
  ngeo_surface_barycentric_map(
    source, target, candidate_faces = candidate_faces
  )
}

surface_metrics <- function(candidate, exact) {
  difference <- candidate$operator - exact$operator
  column_error <- Matrix::colSums(abs(difference))
  candidate_sum <- Matrix::colSums(candidate$operator)
  exact_sum <- Matrix::colSums(exact$operator)
  list(
    candidate_miss_rate = mean(column_error > 1e-10),
    maximum_weight_error = if (length(difference@x)) {
      max(abs(difference@x))
    } else {
      0
    },
    coverage_error = max(abs(
      as.numeric(candidate_sum > 1e-12) - as.numeric(exact_sum > 1e-12)
    )),
    simplex_error = max(abs(
      candidate_sum[exact_sum > 1e-12] - 1
    ))
  )
}

surface_cells <- function(kind, size, query_count, seed) {
  target_grid <- surface_grid(size, kind)
  source_grid <- surface_queries(target_grid, query_count, seed)
  space <- ngeo_coordinate_space(
    paste0("val307-", kind, "-", size), kind = "surface", unit = "mm"
  )
  source <- ngeo_surface(
    source_grid$coordinates, source_grid$faces, coordinate_space = space
  )
  target <- ngeo_surface(
    target_grid$coordinates, target_grid$faces,
    mask = target_grid$mask, coordinate_space = space
  )
  exact <- map_surface(
    source, target, nrow(target_grid$faces), exact = TRUE
  )
  candidates <- list(
    `16` = map_surface(source, target, 16L),
    `64` = map_surface(source, target, 64L),
    exact = exact
  )
  lapply(names(candidates), function(candidate_faces) {
    metrics <- surface_metrics(candidates[[candidate_faces]], exact)
    engine <- candidates[[candidate_faces]]$history$operations[[1L]]$
      parameters$search_engine
    pass <- metrics$candidate_miss_rate <= 1e-4 &&
      metrics$maximum_weight_error <= 1e-10 &&
      metrics$coverage_error <= 1e-10 &&
      metrics$simplex_error <= 1e-12
    c(list(
      geometry = kind,
      size = size,
      candidate_faces = candidate_faces,
      seed = seed,
      target_vertices = nrow(target_grid$coordinates),
      target_faces = nrow(target_grid$faces),
      source_queries = nrow(source_grid$coordinates),
      search_engine = engine,
      comparator = "exhaustive-face-search"
    ), metrics, list(pass = pass))
  })
}

volume_cells <- function(size, query_count, seed) {
  dimensions <- volume_dimensions[[as.character(size)]]
  if (is.null(dimensions) || prod(dimensions) != size) {
    stop("No exact volume dimensions are registered for size ", size, ".")
  }
  index <- arrayInd(seq_len(size), .dim = dimensions)
  target_mask <- (index[, 1L] + 2L * index[, 2L] + 3L * index[, 3L]) %% 11L != 0L
  selected <- unique(round(seq(1, size, length.out = query_count)))
  source_mask <- rep.int(FALSE, size)
  source_mask[selected] <- TRUE
  space <- ngeo_coordinate_space(
    paste0("val307-volume-", size), kind = "volume", unit = "mm"
  )
  source <- ngeo_volume(
    dim = dimensions, affine = diag(4), mask = source_mask,
    coordinate_space = space, index_base = "zero"
  )
  target <- ngeo_volume(
    dim = dimensions, affine = diag(4), mask = target_mask,
    coordinate_space = space, index_base = "zero"
  )
  overlap <- ngeo_voxel_overlap_map(source, target, outside = "drop")
  column_sum <- Matrix::colSums(overlap$operator)
  expected <- as.numeric(target_mask[selected])
  metrics <- list(
    candidate_miss_rate = 0,
    maximum_weight_error = if (length(overlap$operator@x)) {
      max(abs(overlap$operator@x - 1))
    } else {
      0
    },
    coverage_error = max(abs(column_sum - expected)),
    simplex_error = if (any(expected > 0)) {
      max(abs(column_sum[expected > 0] - 1))
    } else {
      0
    }
  )
  pass <- metrics$maximum_weight_error <= 1e-10 &&
    metrics$coverage_error <= 1e-10 && metrics$simplex_error <= 1e-12
  lapply(c("16", "64", "exact"), function(candidate_faces) {
    c(list(
      geometry = "volume-partial-mask",
      size = size,
      candidate_faces = candidate_faces,
      seed = seed,
      target_vertices = sum(target_mask),
      target_faces = NA_integer_,
      source_queries = sum(source_mask),
      search_engine = "exact_axis_aligned_overlap",
      comparator = "analytic-voxel-overlap"
    ), metrics, list(pass = pass))
  })
}

sizes <- unlist(validation$factors$size, use.names = FALSE)
if (identical(run_mode, "smoke")) sizes <- sizes[[1L]]
query_count <- if (identical(run_mode, "smoke")) 16L else 64L
geometries <- unlist(validation$factors$geometry, use.names = FALSE)
cells <- list()
for (size in sizes) {
  for (geometry in geometries) {
    seed <- validation$seed_base + as.integer(size) + match(geometry, geometries)
    current <- if (identical(geometry, "volume-partial-mask")) {
      volume_cells(size, query_count, seed)
    } else {
      surface_cells(geometry, size, query_count, seed)
    }
    cells <- c(cells, current)
  }
}

expected_cells <- length(geometries) * length(sizes) *
  length(validation$factors$candidate_faces)
cell_keys <- vapply(cells, function(x) paste(
  x$geometry, x$size, x$candidate_faces, sep = "|"
), character(1))
coverage_complete <- length(cells) == expected_cells &&
  !anyDuplicated(cell_keys)
passed <- coverage_complete && all(vapply(cells, `[[`, logical(1), "pass"))
result <- list(
  schema = "neurogeo/phase3-validation/1",
  validation_id = validation$id,
  simulation_id = validation$simulation_id,
  design_sha256 = design_hash,
  package_version = read.dcf("DESCRIPTION", fields = "Version")[[1L]],
  dependency_versions = as.list(vapply(
    required, function(package) as.character(utils::packageVersion(package)),
    character(1)
  )),
  platform = R.version$platform,
  r_version = R.version.string,
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
  run_mode = run_mode,
  seed_base = validation$seed_base,
  query_count_per_geometry_size = query_count,
  primary_evidence_eligible = identical(run_mode, "full") && passed,
  validation = if (passed && identical(run_mode, "full")) {
    "passed"
  } else if (passed) {
    "debug-passed"
  } else {
    "failed"
  },
  registered_cell_count = expected_cells,
  observed_cell_count = length(cells),
  registered_cell_coverage_complete = coverage_complete,
  cells = unname(cells)
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!passed) quit(status = 2L)
