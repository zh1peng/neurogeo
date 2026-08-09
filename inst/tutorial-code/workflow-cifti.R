## ---- workflow-cifti ----
library(neurogeo)
stopifnot(requireNamespace("cifti", quietly = TRUE))
cifti_path <- system.file(
  "extdata", "golden", "tiny.dscalar.nii", package = "neurogeo"
)
cifti_surface_path <- system.file(
  "extdata", "golden", "tetra.surf.gii", package = "neurogeo"
)
dense <- read_ngeo_cifti(
  cifti_path,
  surfaces = c(left = cifti_surface_path, right = cifti_surface_path),
  checksum = TRUE
)
ngeo_base_type(dense)
ngeo_base_elements(dense)
ngeo_layers(dense)
ngeo_measures(dense)
ngeo_capabilities(dense)
ngeo_qc(dense)$summary

dense_weights <- ngeo_spatial_weights(
  dense, method = "component_contiguity", style = "W"
)
dense_result <- ngeo_moran(
  dense, dense_weights, layer = 1, zero_policy = TRUE
)
ngeo_inference_contract(dense_result)

dense_plot <- tempfile(fileext = ".pdf")
grDevices::pdf(dense_plot)
graphics::matplot(ngeo_values(dense), type = "l", xlab = "grayordinate")
grDevices::dev.off()

cifti_output <- tempfile(fileext = ".dscalar.nii")
write_ngeo_cifti(dense, cifti_output, type = "dscalar")
dense_roundtrip <- read_ngeo_cifti(cifti_output, checksum = TRUE)
stopifnot(identical(dim(ngeo_values(dense_roundtrip)), dim(ngeo_values(dense))))
ngeo_history(dense_roundtrip)
unlink(c(dense_plot, cifti_output), force = TRUE)
