## ---- workflow-surface ----
library(neurogeo)
stopifnot(
  requireNamespace("gifti", quietly = TRUE),
  requireNamespace("freesurferformats", quietly = TRUE)
)
surface_path <- system.file(
  "extdata", "golden", "tetra.surf.gii", package = "neurogeo"
)
geometry <- read_ngeo_gifti(surface_path, checksum = TRUE)
surface_coordinate_sets <- ngeo_spatial_base(geometry)$geometry$coordinates
surface_coordinates <- surface_coordinate_sets[[1L]]
surface_faces <- ngeo_spatial_base(geometry)$geometry$faces
surface <- ngeo_surface(
  surface_coordinates,
  surface_faces,
  values = cbind(signal = rowSums(surface_coordinates^2)),
  measures = ngeo_measure(
    measure_id = "surface_signal", name = "surface signal",
    support_behavior = "intensive", unit = "a.u."
  ),
  coordinate_space = ngeo_spatial_base(geometry)$coordinate_space
)
ngeo_validate(surface, "strict")
ngeo_qc(surface)$summary
surface_weights <- ngeo_spatial_weights(
  surface, method = "mesh_contiguity", style = "W"
)
surface_result <- ngeo_moran(surface, surface_weights, "signal")
ngeo_inference_contract(surface_result)

surface <- ngeo_set_chart(
  surface, surface_coordinates[, 1:2, drop = FALSE], name = "tutorial"
)
surface_plot <- tempfile(fileext = ".pdf")
grDevices::pdf(surface_plot)
plot(surface, layer = "signal")
grDevices::dev.off()

surface_output <- tempfile(fileext = ".surf.gii")
surface_manifest <- write_ngeo_gifti(surface, surface_output)
surface_roundtrip <- read_ngeo_gifti(surface_manifest$geometry, checksum = TRUE)
ngeo_history(surface_roundtrip)
unlink(c(surface_plot, unlist(surface_manifest)), force = TRUE)
