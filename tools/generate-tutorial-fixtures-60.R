args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else
  file.path("inst", "extdata", "tutorial-fixtures-6.0.csv")
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Tutorial fixture generation requires digest.")
}

fixture <- data.frame(
  fixture_id = c(
    "point-grid", "nifti-volume", "gifti-surface", "cifti-grayordinate",
    "roi-cohort"
  ),
  format = c("CSV", "NIfTI-1", "GIFTI", "CIFTI-2", "CSV"),
  workflow = c("quickstart", "volume", "surface", "grayordinate", "ROI/cohort"),
  path = file.path(
    "inst", "extdata", "golden",
    c(
      "tiny-point-grid.csv", "tiny.nii.gz", "tetra.surf.gii",
      "tiny.dscalar.nii", "tiny-roi-cohort.csv"
    )
  ),
  origin = "synthetic neurogeo 6.0 fixture corpus",
  license = "CC0-1.0",
  version = "6.0.0",
  redistribution = "bundled",
  offline_strategy = "installed package extdata",
  online_strategy = "immutable neurogeo release archive",
  expected_result = c(
    "9 points; one intensive signal layer; finite Moran statistic",
    "2x2x2 lattice; two value layers; affine diagonal 2/3/4 mm",
    "4 vertices; 4 triangular faces; millimetre surface coordinates",
    "6 grayordinates; left/right cortex plus left thalamus; two scalar layers",
    "6 subjects; two groups; four ROI columns"
  ),
  stringsAsFactors = FALSE
)
if (any(!file.exists(fixture$path))) {
  stop("A declared tutorial fixture is missing.")
}
fixture$size_bytes <- as.numeric(file.info(fixture$path)$size)
fixture$sha256 <- vapply(fixture$path, function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}, character(1))
fixture$path <- gsub("\\\\", "/", fixture$path)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(fixture, output, row.names = FALSE, na = "")
cat("Tutorial fixtures:", nrow(fixture), "licensed files.\n")
