args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "doc-entrypoints-60.json")
candidate_tar <- Sys.getenv("NEUROGEO_CANDIDATE_TAR", unset = "")
if (!nzchar(candidate_tar)) {
  stop("Set NEUROGEO_CANDIDATE_TAR to the documented source archive.",
       call. = FALSE)
}
required_packages <- c("digest", "jsonlite", "neurogeo")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop("Documentation smoke test requires: ",
       paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))
sys.source("tools/evidence-identity-60.R", envir = environment())

entrypoints <- c(
  readme = "README.md",
  home_zh = "website/index.md",
  home_en = "website/en/index.md",
  guide_zh = "website/guide/index.md",
  guide_en = "website/en/guide/index.md",
  concepts_zh = "website/concepts/index.md",
  tutorials_zh = "website/tutorials/index.md",
  tutorials_en = "website/en/tutorials/index.md"
)
missing <- entrypoints[!file.exists(entrypoints)]
if (length(missing)) {
  stop("Missing documentation entry points: ", paste(missing, collapse = ", "))
}

read_utf8 <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  text <- rawToChar(raw)
  converted <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA)
  if (is.na(converted)) {
    stop("Invalid UTF-8 documentation: ", path, call. = FALSE)
  }
  converted
}
texts <- lapply(entrypoints, read_utf8)
all_current_sources <- c(
  entrypoints,
  list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE),
  list.files("website", pattern = "\\.md$", full.names = TRUE, recursive = TRUE)
)
source_text <- paste(vapply(all_current_sources, read_utf8, character(1)),
                     collapse = "\n")
removed_moran_files <- all_current_sources[vapply(
  all_current_sources,
  function(path) grepl(
    "ngeo_moran\\s*\\([^)]{0,1200}\\bmap\\s*=",
    read_utf8(path),
    perl = TRUE
  ),
  logical(1)
)]

checks <- list(
  utf8 = TRUE,
  six_zero_entrypoints = !any(vapply(
    texts,
    function(text) grepl(
      "neurogeo 5\\.[01]|5\\.[01] data model",
      text,
      ignore.case = TRUE
    ),
    logical(1)
  )),
  no_removed_moran_selector = !length(removed_moran_files),
  current_render_version = !grepl(
    "packageVersion\\(\\\"neurogeo\\\"\\).{0,160}5\\.[01]\\.0",
    source_text,
    perl = TRUE
  )
)

coordinates <- as.matrix(expand.grid(x = 0:2, y = 0:2))
x <- ngeo_point(
  coordinates,
  values = cbind(signal = c(1, 2, 3, 2, 4, 7, 3, 7, 9)),
  measures = ngeo_measure(support_behavior = "intensive", unit = "a.u."),
  coordinate_space = ngeo_coordinate_space(
    space_id = "example-grid", kind = "unknown", unit = "mm"
  )
)
w <- ngeo_spatial_weights(
  x, method = "distance_band", threshold = 1.01,
  distance_method = "euclidean", style = "W"
)
smoke <- ngeo_moran(
  x, w, layer = "signal", permutations = 19, seed = 2026
)
checks$guide_smoke <- inherits(smoke, "ngeo_global_stat") &&
  identical(smoke$layer_id, layers(x)$layer_id[[1L]])

identity <- ngeo_evidence_identity(candidate_tar, entrypoints)
report <- list(
  schema = "neurogeo/evidence-report/1",
  suite = "documentation-entrypoints-6.0",
  candidate = identity,
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  checks = checks,
  pass = all(unlist(checks, use.names = FALSE))
)
if (!report$pass) {
  stop(
    "Documentation entry-point checks failed: ",
    paste(names(checks)[!unlist(checks)], collapse = ", "),
    if (length(removed_moran_files)) paste0(
      "; removed Moran selector in ",
      paste(removed_moran_files, collapse = ", ")
    ) else "",
    call. = FALSE
  )
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
