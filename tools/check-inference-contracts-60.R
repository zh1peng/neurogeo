path <- file.path("inst", "spec", "inference-contracts-6.0.csv")
if (!file.exists(path)) stop("Missing stable inference-contract registry.")
registry <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
required <- c(
  "result_class", "estimand", "sampling_unit", "null_model", "metric",
  "support", "uncertainty_target"
)
if (!identical(names(registry), required) || !nrow(registry) ||
    anyDuplicated(registry$result_class) ||
    any(vapply(registry, function(value) anyNA(value) || any(!nzchar(value)),
               logical(1)))) {
  stop("Stable inference-contract registry is incomplete or duplicated.")
}
lifecycle <- utils::read.csv(
  file.path("inst", "spec", "api-lifecycle-6.0.csv"),
  stringsAsFactors = FALSE
)
stable_classes <- lifecycle$symbol[
  lifecycle$type == "class" & lifecycle$lifecycle == "stable"
]
missing_class <- setdiff(registry$result_class, stable_classes)
if (length(missing_class)) {
  stop(
    "Inference registry contains non-stable or unknown classes: ",
    paste(missing_class, collapse = ", ")
  )
}
required_61 <- c(
  "ngeo_brain_landscape",
  "ngeo_brain_point_process",
  "ngeo_local_layer_coupling",
  "ngeo_maup_sensitivity",
  "ngeo_nonseparable_hotspots",
  "ngeo_resistance_distance",
  "ngeo_wavelet_coupling"
)
missing_61 <- setdiff(required_61, registry$result_class)
if (length(missing_61)) {
  stop(
    "Stable 6.1 scientific result classes lack inference contracts: ",
    paste(missing_61, collapse = ", ")
  )
}
cat("Inference contracts:", nrow(registry), "stable scientific result classes.\n")
