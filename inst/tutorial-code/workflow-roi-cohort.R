## ---- workflow-roi-cohort ----
library(neurogeo)
cohort_path <- system.file(
  "extdata", "golden", "tiny-roi-cohort.csv", package = "neurogeo"
)
cohort <- utils::read.csv(cohort_path, stringsAsFactors = FALSE)
roi_columns <- grep("^roi_", names(cohort), value = TRUE)
roi_values <- t(as.matrix(cohort[roi_columns]))
colnames(roi_values) <- cohort$subject_id
roi_layers <- data.frame(
  layer_id = cohort$subject_id,
  name = cohort$subject_id,
  measure_id = "cortical_thickness",
  subject_id = cohort$subject_id,
  group = cohort$group,
  stringsAsFactors = FALSE
)
roi_centroids <- cbind(x = seq_along(roi_columns), y = 0)
roi_adjacency <- abs(outer(seq_along(roi_columns), seq_along(roi_columns), "-")) == 1
roi <- ngeo_parcellation(
  data.frame(region_id = roi_columns, name = roi_columns),
  values = roi_values,
  centroid = roi_centroids,
  support_size = rep(1, length(roi_columns)),
  adjacency = roi_adjacency,
  layers = roi_layers,
  measures = ngeo_measure(
    measure_id = "cortical_thickness", name = "cortical thickness",
    support_behavior = "intensive", unit = "mm"
  ),
  coordinate_space = ngeo_coordinate_space(
    "tutorial-roi-layout", kind = "unknown", unit = "unknown"
  )
)
ngeo_validate(roi, "strict")
ngeo_qc(roi)$summary

group_mean <- sapply(c("control", "case"), function(group) {
  rowMeans(ngeo_values(roi)[, roi_layers$group == group, drop = FALSE])
})
group_difference <- ngeo_parcellation(
  ngeo_base_elements(roi),
  values = cbind(case_minus_control = group_mean[, "case"] - group_mean[, "control"]),
  centroid = roi_centroids,
  support_size = rep(1, length(roi_columns)),
  adjacency = roi_adjacency,
  measures = ngeo_measure(support_behavior = "intensive", unit = "mm"),
  coordinate_space = ngeo_spatial_base(roi)$coordinate_space
)
roi_weights <- ngeo_spatial_weights(
  group_difference, method = "region_contiguity", style = "W"
)
roi_result <- ngeo_moran(group_difference, roi_weights, "case_minus_control")
ngeo_inference_contract(roi_result)

roi_plot <- tempfile(fileext = ".pdf")
grDevices::pdf(roi_plot)
plot(group_difference, layer = "case_minus_control")
grDevices::dev.off()
history_output <- tempfile(fileext = ".json")
ngeo_export_history(group_difference, history_output, redact = "paths")
stopifnot(file.exists(history_output))
unlink(c(roi_plot, history_output), force = TRUE)
