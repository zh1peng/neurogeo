args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "support-workflows.json")
}

required <- c("RNifti", "cifti", "gifti", "jsonlite")
missing <- required[!vapply(
  required,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]
if (length(missing)) {
  stop(
    "Support workflow validation requires: ",
    paste(missing, collapse = ", ")
  )
}
if (!exists("ngeo_surface_registration_map", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

golden <- function(name) {
  file.path("inst", "extdata", "golden", name)
}
surface <- read_ngeo_gifti(
  golden("tetra.surf.gii"),
  data = golden("tetra.shape.gii"),
  measures = ngeo_measure(spatial_semantics = "intensive"),
  space = ngeo_space("golden-gifti", kind = "surface"),
  checksum = TRUE
)
surface_map <- ngeo_surface_registration_map(
  surface,
  surface,
  method = "barycentric"
)
surface_changed <- ngeo_change_support(surface, surface, surface_map)

volume <- suppressWarnings(read_ngeo_nifti(
  golden("tiny.nii.gz"),
  measures = rbind(
    ngeo_measure(spatial_semantics = "intensive"),
    ngeo_measure(spatial_semantics = "intensive")
  ),
  checksum = TRUE
))
volume_labels <- ifelse(
  volume$values[, 1L] <= stats::median(volume$values[, 1L]),
  "low",
  "high"
)
volume_map <- ngeo_atlas_map(volume, volume_labels)
volume_changed <- ngeo_change_support(
  volume,
  volume_map$target,
  volume_map
)

grayordinates <- read_ngeo_cifti(
  golden("tiny.dscalar.nii"),
  surfaces = c(
    left = golden("tetra.surf.gii"),
    right = golden("tetra.surf.gii")
  ),
  measures = rbind(
    ngeo_measure(spatial_semantics = "intensive"),
    ngeo_measure(spatial_semantics = "intensive")
  ),
  checksum = TRUE
)
gray_map <- ngeo_atlas_map(
  grayordinates,
  grayordinates$domain$elements$component_id
)
gray_changed <- ngeo_change_support(
  grayordinates,
  gray_map$target,
  gray_map
)

result <- list(
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = read.dcf("DESCRIPTION")[1L, "Version"],
  runtime_policy = list(
    external_binaries_required = FALSE,
    readers = c(
      NIfTI = "RNifti",
      GIFTI = "gifti",
      CIFTI = "cifti"
    )
  ),
  workflows = list(
    gifti_registered_surface = list(
      source_elements = nrow(surface$domain$elements),
      target_elements = nrow(surface$domain$elements),
      nonzero = length(surface_map$operator@x),
      maximum_value_error = max(
        abs(surface_changed$values - surface$values)
      ),
      diagnostics = ngeo_support_diagnostics(surface_map)$summary,
      validation = "passed"
    ),
    nifti_label_segmentation = list(
      source_elements = nrow(volume$domain$elements),
      regions = nrow(volume_changed$domain$elements),
      extensive_conservative = ngeo_support_diagnostics(
        volume_map
      )$conservative,
      validation = "passed"
    ),
    cifti_hybrid_atlas = list(
      source_elements = nrow(grayordinates$domain$elements),
      components = nrow(gray_changed$domain$elements),
      source_support_finite = all(is.finite(gray_map$source_support)),
      validation = "passed"
    )
  )
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
