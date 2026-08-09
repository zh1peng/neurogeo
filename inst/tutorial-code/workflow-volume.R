## ---- workflow-volume ----
library(neurogeo)
stopifnot(requireNamespace("RNifti", quietly = TRUE))
volume_path <- system.file(
  "extdata", "golden", "tiny.nii.gz", package = "neurogeo"
)
volume <- read_ngeo_nifti(volume_path, checksum = TRUE)
ngeo_base_type(volume)
ngeo_base_elements(volume)
ngeo_layers(volume)
ngeo_measures(volume)
volume$base$coordinate_space
ngeo_qc(volume)$summary

volume_weights <- ngeo_spatial_weights(
  volume, method = "voxel_contiguity", connectivity = 6, style = "W"
)
volume_result <- ngeo_moran(volume, volume_weights, layer = 1)
ngeo_inference_contract(volume_result)

volume_plot <- tempfile(fileext = ".pdf")
grDevices::pdf(volume_plot)
plot(volume, layer = 1)
grDevices::dev.off()

volume_output <- tempfile(fileext = ".nii.gz")
volume_manifest <- write_ngeo_nifti(volume, volume_output)
volume_roundtrip <- read_ngeo_nifti(volume_manifest$data, checksum = TRUE)
stopifnot(identical(dim(ngeo_values(volume_roundtrip)), dim(ngeo_values(volume))))
ngeo_history(volume_roundtrip)
unlink(c(volume_plot, unlist(volume_manifest)), force = TRUE)
