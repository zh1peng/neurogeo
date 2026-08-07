args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) {
  args[[1L]]
} else {
  file.path("release", "external-workflows.json")
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
    "External workflow validation requires: ",
    paste(missing, collapse = ", ")
  )
}
if (!exists("read_ngeo_nifti", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    pkgload::load_all(export_all = FALSE, helpers = FALSE)
  } else {
    library(neurogeo)
  }
}

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

nifti_path <- system.file(
  "extdata",
  "example.nii.gz",
  package = "RNifti"
)
nifti <- read_ngeo_nifti(
  nifti_path,
  mask = "nonzero",
  measures = ngeo_measure(support_behavior = "intensive"),
  checksum = TRUE
)
nifti_weights <- ngeo_spatial_weights(
  nifti,
  method = "voxel_contiguity",
  connectivity = 6L,
  style = "W"
)
nifti_moran <- ngeo_moran(
  nifti,
  nifti_weights,
  zero_policy = TRUE
)

cifti_dir <- system.file("extdata", package = "cifti")
cifti_path <- file.path(
  cifti_dir,
  "curvature.32k_fs_LR.dscalar.nii"
)
cifti_surfaces <- c(
  left = file.path(
    cifti_dir,
    "Q1-Q6_R440.L.inflated.32k_fs_LR.surf.gii"
  ),
  right = file.path(
    cifti_dir,
    "Q1-Q6_R440.R.inflated.32k_fs_LR.surf.gii"
  )
)
cifti <- read_ngeo_cifti(
  cifti_path,
  surfaces = cifti_surfaces,
  measures = ngeo_measure(support_behavior = "intensive"),
  checksum = TRUE
)
cifti_weights <- ngeo_spatial_weights(
  cifti,
  method = "component_contiguity",
  style = "W"
)
cifti_moran <- ngeo_moran(
  cifti,
  cifti_weights,
  zero_policy = TRUE
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
      CIFTI = "cifti",
      GIFTI = "gifti"
    )
  ),
  workflows = list(
    nifti_volume_to_moran = list(
      source_package = "RNifti",
      source_package_version = as.character(
        utils::packageVersion("RNifti")
      ),
      source_file = basename(nifti_path),
      source_md5 = unname(tools::md5sum(nifti_path)),
      base = base_type(nifti),
      elements = nrow(base_elements(nifti)),
      layers = nrow(layers(nifti)),
      weights_nonzero = length(nifti_weights$matrix@x),
      base_hash = base_hash(nifti),
      moran_i = nifti_moran$estimate,
      validation = "passed"
    ),
    cifti_surface_to_moran = list(
      source_package = "cifti",
      source_package_version = as.character(
        utils::packageVersion("cifti")
      ),
      source_file = basename(cifti_path),
      source_md5 = unname(tools::md5sum(cifti_path)),
      surface_files = basename(cifti_surfaces),
      base = base_type(cifti),
      elements = nrow(base_elements(cifti)),
      components = vapply(
        cifti$base$geometry$components,
        function(component) component$component_id,
        character(1)
      ),
      weights_nonzero = length(cifti_weights$matrix@x),
      base_hash = base_hash(cifti),
      moran_i = cifti_moran$estimate,
      validation = "passed"
    )
  )
)

jsonlite::write_json(
  result,
  output,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
