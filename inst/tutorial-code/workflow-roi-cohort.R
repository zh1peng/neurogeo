## ---- workflow-roi-cohort ----
library(neurogeo)
source(system.file(
  "tutorial-code", "brain-case-study.R", package = "neurogeo",
  mustWork = TRUE
))
dk <- ngeo_tutorial_dk_case_control(n_per_group = 100, seed = 20260810)
roi <- dk$cohort
ngeo_validate(roi, "strict")
ngeo_qc(roi)$summary
dim(ngeo_values(roi))
head(ngeo_layers(roi)[c("subject_id", "group", "age", "sex", "site")])

group_difference <- dk$difference
roi_weights <- ngeo_spatial_weights(
  group_difference, method = "region_contiguity", style = "W"
)
roi_result <- ngeo_moran(
  group_difference, roi_weights, "SCZ_minus_HC",
  permutations = 499, seed = 2026
)
ngeo_inference_contract(roi_result)
ngeo_tutorial_plot_dk(
  group_difference,
  title = "SCZ minus HC cortical thickness (mm)", midpoint = 0
)

history_output <- tempfile(fileext = ".json")
ngeo_export_history(group_difference, history_output, redact = "paths")
stopifnot(file.exists(history_output))
unlink(history_output, force = TRUE)
