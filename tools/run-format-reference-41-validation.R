args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "format-reference-41-validation.json")
required <- c(
  "cifti", "freesurferformats", "gifti", "jsonlite", "RNifti"
)
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Format reference 4.1 validation requires: ",
       paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
fixture <- function(name) ngeo_example_data(name)$path[[1L]]

started <- proc.time()[["elapsed"]]
manifest <- ngeo_example_data()
assert(nrow(manifest) == 6L && all(manifest$verified),
       "Reference fixture manifest failed verification.")

root <- tempfile("neurogeo-41-validation-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE), add = TRUE)

nifti <- suppressWarnings(
  read_ngeo_nifti(fixture("rnifti-example"), checksum = TRUE)
)
nifti_output <- write_ngeo_nifti(
  nifti, file.path(root, "reference.nii.gz")
)
nifti_restored <- read_ngeo_nifti(
  nifti_output$data,
  layers = nifti$layers,
  measures = nifti$measures,
  checksum = FALSE
)
nifti_valid <- identical(nifti$base$geometry$dim, c(96L, 96L, 60L)) &&
  isTRUE(all.equal(
    nifti_restored$base$geometry$affine,
    nifti$base$geometry$affine,
    tolerance = 1e-6
  )) &&
  isTRUE(all.equal(nifti_restored$values, nifti$values))
assert(nifti_valid, "NIfTI reference round-trip differs.")

gifti_geometry <- read_ngeo_gifti(
  fixture("freesurferformats-cube"),
  checksum = TRUE
)
coordinates <- gifti_geometry$base$geometry$coordinates
surface <- ngeo_surface(
  coordinates = coordinates,
  faces = gifti_geometry$base$geometry$faces,
  values = cbind(curvature = rowSums(
    coordinates[[gifti_geometry$base$geometry$active_coordinates]]
  )),
  layers = data.frame(name = "curvature"),
  measures = ngeo_measure(
    value_type = "continuous",
    support_behavior = "intensive",
    unit = "a.u."
  ),
  coordinate_space = gifti_geometry$base$coordinate_space,
  coordinate_roles = gifti_geometry$base$geometry$coordinate_meta$role,
  index_base = "one",
  source_index_base = 0L
)
gifti_output <- write_ngeo_gifti(
  surface, file.path(root, "surface-reference.surf.gii")
)
gifti_restored <- read_ngeo_gifti(
  gifti_output$geometry,
  data = gifti_output$data,
  measures = surface$measures,
  checksum = FALSE
)
gifti_valid <- identical(
  gifti_restored$base$geometry$faces, surface$base$geometry$faces
) && isTRUE(all.equal(
  gifti_restored$values, surface$values, tolerance = 1e-6
))
assert(gifti_valid, "GIFTI reference round-trip differs.")

cifti <- read_ngeo_cifti(
  fixture("cifti-curvature-fslr32k"),
  checksum = TRUE
)
cifti_path <- file.path(root, "reference.dscalar.nii")
write_ngeo_cifti(cifti, cifti_path, type = "dscalar")
cifti_restored <- read_ngeo_cifti(cifti_path, checksum = FALSE)
cifti_valid <- identical(dim(cifti$values), c(59412L, 1L)) &&
  identical(
    cifti_restored$base$elements$structure,
    cifti$base$elements$structure
  ) &&
  isTRUE(all.equal(
    cifti_restored$values, cifti$values, tolerance = 1e-6
  ))
assert(cifti_valid, "CIFTI reference round-trip differs.")

missing_affine_rejected <- inherits(
  tryCatch(
    read_ngeo_freesurfer(
      fixture("freesurfer-tiny-mgh"),
      base = "volume",
      checksum = FALSE
    ),
    error = identity
  ),
  "ngeo_error_transform"
)
malformed_surface_rejected <- inherits(
  tryCatch(
    read_ngeo_freesurfer(
      fixture("freesurfer-tiny-surface"),
      checksum = FALSE
    ),
    error = identity
  ),
  "ngeo_error_geometry"
)
freesurfer <- read_ngeo_freesurfer(
  fixture("freesurfer-tiny-mgh"),
  base = "volume",
  affine = diag(4),
  checksum = TRUE
)
freesurfer_output <- write_ngeo_freesurfer(
  freesurfer, file.path(root, "reference.mgh")
)
freesurfer_restored <- read_ngeo_freesurfer(
  freesurfer_output$data,
  base = "volume",
  checksum = FALSE
)
freesurfer_valid <- missing_affine_rejected &&
  malformed_surface_rejected &&
  isTRUE(all.equal(
    freesurfer_restored$base$geometry$affine,
    freesurfer$base$geometry$affine,
    tolerance = 1e-5
  )) &&
  isTRUE(all.equal(
    freesurfer_restored$values,
    freesurfer$values,
    tolerance = 1e-5
  ))
assert(freesurfer_valid, "FreeSurfer reference validation differs.")

result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(utils::packageVersion("neurogeo")),
  validation = "passed",
  fixtures = list(
    count = nrow(manifest),
    verified = all(manifest$verified),
    licenses = sort(unique(manifest$license))
  ),
  formats = list(
    nifti = nifti_valid,
    gifti = gifti_valid,
    cifti = cifti_valid,
    freesurfer = freesurfer_valid
  ),
  external_neuroimaging_binaries = FALSE,
  elapsed_seconds = proc.time()[["elapsed"]] - started,
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
