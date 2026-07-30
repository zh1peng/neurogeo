args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "support-map-conformance.json")
}
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("pkgload", quietly = TRUE)) {
  stop("NGCS 2.0 conformance requires jsonlite and pkgload.")
}
pkgload::load_all(export_all = FALSE, helpers = FALSE)
fixture_dir <- file.path("inst", "extdata", "conformance-ngcs2")
paths <- list.files(
  fixture_dir,
  pattern = "[.]json$",
  full.names = TRUE
)
if (length(paths) < 3L) {
  stop("NGCS 2.0 requires crisp, probabilistic, and overlapping fixtures.")
}

results <- lapply(paths, function(path) {
  fixture <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  if (!identical(fixture$spec_version, "2.0") ||
      !identical(fixture$direction, "target_by_source")) {
    stop("Invalid NGCS 2.0 fixture metadata in ", basename(path))
  }
  operator <- as.matrix(fixture$operator)
  source_support <- as.numeric(fixture$source_support)
  source <- ngeo_points(
    cbind(x = seq_along(source_support), y = 0),
    values = cbind(
      intensive = as.numeric(fixture$values$intensive),
      extensive = as.numeric(fixture$values$extensive)
    ),
    measures = rbind(
      ngeo_measure(spatial_semantics = "intensive"),
      ngeo_measure(spatial_semantics = "extensive")
    )
  )
  target <- ngeo_regions(
    data.frame(region_id = paste0("target_", seq_len(nrow(operator)))),
    support_size = as.numeric(fixture$expected$target_support)
  )
  support_map <- ngeo_support_map(
    source,
    target,
    operator,
    type = fixture$type,
    source_support = source_support,
    coverage = fixture$coverage
  )
  changed <- ngeo_change_support(
    source,
    target,
    support_map,
    allocation = fixture$allocation
  )
  tolerance <- as.numeric(fixture$tolerance)
  checks <- c(
    target_support = isTRUE(all.equal(
      support_map$target_support,
      as.numeric(fixture$expected$target_support),
      tolerance = tolerance
    )),
    intensive = isTRUE(all.equal(
      as.numeric(changed$values[, "intensive"]),
      as.numeric(fixture$expected$intensive),
      tolerance = tolerance
    )),
    extensive = isTRUE(all.equal(
      as.numeric(changed$values[, "extensive"]),
      as.numeric(fixture$expected$extensive),
      tolerance = tolerance
    )),
    conservation = isTRUE(all.equal(
      sum(changed$values[, "extensive"]),
      as.numeric(fixture$expected$extensive_total),
      tolerance = tolerance
    ))
  )
  if (!all(checks)) {
    stop("NGCS 2.0 conformance failed for ", basename(path))
  }
  list(
    fixture = basename(path),
    fixture_id = fixture$fixture_id,
    type = fixture$type,
    md5 = unname(tools::md5sum(path)),
    checks = as.list(checks),
    validation = "passed"
  )
})
types <- vapply(results, `[[`, character(1), "type")
if (!all(c("crisp", "probabilistic", "overlapping") %in% types)) {
  stop("NGCS 2.0 conformance did not cover all support-map types.")
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  list(
    generated_at_utc = format(
      Sys.time(),
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    ),
    specification = "NGCS 2.0",
    package_version = read.dcf("DESCRIPTION")[1L, "Version"],
    validation = "passed",
    fixtures = results,
    platform = R.version$platform,
    r_version = R.version.string
  ),
  output,
  pretty = TRUE,
  auto_unbox = TRUE
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
