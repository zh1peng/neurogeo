args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "coverage-421.json")
required <- c("covr", "jsonlite")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Coverage validation requires: ", paste(missing, collapse = ", "))
}

threshold <- 85
started <- proc.time()[["elapsed"]]
coverage <- covr::package_coverage(
  path = ".",
  type = c("tests", "examples"),
  combine_types = TRUE,
  quiet = TRUE
)
percentage <- as.numeric(covr::percent_coverage(coverage))
elapsed <- unname(proc.time()[["elapsed"]] - started)
passed <- is.finite(percentage) && percentage >= threshold
package_version <- read.dcf("DESCRIPTION")[1L, "Version"]
line_coverage <- covr::tally_coverage(coverage, by = "line")
per_file <- do.call(
  rbind,
  lapply(
    split(line_coverage, line_coverage$filename),
    function(x) {
      total <- nrow(x)
      covered <- sum(x$value > 0)
      data.frame(
        file = x$filename[[1L]],
        covered_lines = covered,
        total_lines = total,
        coverage_percent = if (total) 100 * covered / total else NA_real_
      )
    }
  )
)
per_file <- per_file[
  order(per_file$coverage_percent, per_file$file),
  ,
  drop = FALSE
]
rownames(per_file) <- NULL
line_coverage$functions[is.na(line_coverage$functions)] <- "<top-level>"
function_key <- interaction(
  line_coverage$filename,
  line_coverage$functions,
  drop = TRUE,
  lex.order = TRUE
)
per_function <- do.call(
  rbind,
  lapply(
    split(line_coverage, function_key),
    function(x) {
      total <- nrow(x)
      covered <- sum(x$value > 0)
      data.frame(
        file = x$filename[[1L]],
        function_name = x$functions[[1L]],
        covered_lines = covered,
        total_lines = total,
        uncovered_lines = total - covered,
        coverage_percent = if (total) 100 * covered / total else NA_real_
      )
    }
  )
)
per_function <- per_function[
  order(
    -per_function$uncovered_lines,
    per_function$coverage_percent,
    per_function$file,
    per_function$function_name
  ),
  ,
  drop = FALSE
]
rownames(per_function) <- NULL

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  list(
    schema_version = "1",
    package = "neurogeo",
    package_version = package_version,
    measurement = "line coverage from tests and executable examples",
    coverage_percent = percentage,
    threshold_percent = threshold,
    elapsed_seconds = elapsed,
    passed = passed,
    claim_boundary = paste(
      "Coverage is a maintenance regression signal, not evidence of",
      "scientific validity or real-data correctness."
    ),
    per_file = per_file,
    per_function = per_function
  ),
  output,
  pretty = TRUE,
  auto_unbox = TRUE
)
if (!passed) {
  stop(
    sprintf(
      "Coverage %.2f%% is below the registered %.2f%% threshold.",
      percentage, threshold
    ),
    call. = FALSE
  )
}
cat(sprintf("Coverage %.2f%% (threshold %.2f%%)\n", percentage, threshold))
