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
contrast_values <- setNames(
  ngeo_values(group_difference)[, "SCZ_minus_HC"],
  ngeo_base_elements(group_difference)$region_id
)
contrast_limit <- max(abs(contrast_values), na.rm = TRUE)
contrast_map <- ngeo_cortical_map(
  dk$surface,
  values = contrast_values,
  chart = "flat",
  atlas = dk$atlas,
  underlay = dk$underlay,
  underlay_palette = "Grays",
  overlay_alpha = 0.82,
  palette = "Blue-Red 3",
  limits = c(-contrast_limit, contrast_limit),
  na_color = NA_character_,
  atlas_coverage = "auto"
)
plot(
  contrast_map,
  main = "SCZ minus HC cortical thickness (mm)",
  boundary_color = grDevices::adjustcolor("white", 0.55),
  boundary_lwd = 0.25,
  outline_lwd = 1.1
)

history_output <- tempfile(fileext = ".json")
ngeo_export_history(group_difference, history_output, redact = "paths")
stopifnot(file.exists(history_output))
unlink(history_output, force = TRUE)
