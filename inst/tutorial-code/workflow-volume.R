## ---- workflow-volume ----
library(neurogeo)
stopifnot(requireNamespace("RNifti", quietly = TRUE))

# A brain-shaped 5 HC + 5 SCZ teaching volume. The voxel geometry and affine
# are explicit; only the subject values are simulated.
volume_dim <- c(48L, 56L, 40L)
voxel_index <- arrayInd(seq_len(prod(volume_dim)), .dim=volume_dim)
normalized <- cbind(
  x=(voxel_index[, 1L] - mean(seq_len(volume_dim[[1L]]))) / 22,
  y=(voxel_index[, 2L] - mean(seq_len(volume_dim[[2L]]))) / 25,
  z=(voxel_index[, 3L] - mean(seq_len(volume_dim[[3L]]))) / 18
)
radius <- rowSums(normalized^2)
brain_mask <- radius < 1 &
  !(abs(normalized[, 1L]) < 0.055 & normalized[, 3L] > -0.2)
brain_mask <- array(brain_mask, dim=volume_dim)
inside <- which(as.vector(brain_mask))
brain_coordinates <- normalized[inside, , drop=FALSE]
baseline <- 95 - 24 * radius[inside] +
  5 * cos(3 * brain_coordinates[, 2L]) * cos(2 * brain_coordinates[, 3L])
scz_effect <- -7 * exp(
  -((abs(brain_coordinates[, 1L]) - 0.48)^2 / 0.08 +
      (brain_coordinates[, 2L] - 0.12)^2 / 0.16 +
      (brain_coordinates[, 3L] + 0.12)^2 / 0.12)
)
set.seed(4101)
volume_group <- factor(rep(c("HC", "SCZ"), each=5), levels=c("HC", "SCZ"))
volume_subject <- sprintf("vol-sub-%02d", 1:10)
volume_values <- vapply(seq_along(volume_subject), function(subject) {
  smooth_subject <- rnorm(1, sd=2) +
    rnorm(1, sd=1.5) * brain_coordinates[, 2L] +
    rnorm(1, sd=1.2) * brain_coordinates[, 3L]
  baseline + smooth_subject + rnorm(length(inside), sd=1.5) +
    if (volume_group[[subject]] == "SCZ") scz_effect else 0
}, numeric(length(inside)))
colnames(volume_values) <- volume_subject
volume <- ngeo_volume(
  values=volume_values,
  dim=volume_dim,
  affine=diag(c(2, 2, 2, 1)),
  mask=brain_mask,
  layers=data.frame(
    layer_id=volume_subject,
    name=volume_subject,
    measure_id="gray_matter_proxy",
    subject_id=volume_subject,
    group=volume_group
  ),
  measures=ngeo_measure(
    measure_id="gray_matter_proxy",
    name="simulated gray-matter intensity",
    support_behavior="intensive", unit="a.u."
  ),
  coordinate_space=ngeo_coordinate_space(
    "simulated-brain-volume", kind="volume", unit="mm"
  )
)
group_mean <- sapply(levels(volume_group), function(group) {
  rowMeans(volume_values[, volume_group == group, drop=FALSE])
})
volume_difference <- ngeo_volume(
  values=cbind(SCZ_minus_HC=group_mean[, "SCZ"] - group_mean[, "HC"]),
  dim=volume_dim,
  affine=diag(c(2, 2, 2, 1)),
  mask=brain_mask,
  measures=ngeo_measure(
    name="SCZ minus HC gray-matter intensity",
    support_behavior="intensive", unit="a.u."
  ),
  coordinate_space=ngeo_spatial_base(volume)$coordinate_space
)

ngeo_validate(volume, "strict")
ngeo_qc(volume)$summary
dim(ngeo_values(volume))
table(ngeo_layers(volume)$group)

volume_weights <- ngeo_spatial_weights(
  volume_difference, method="voxel_contiguity", connectivity=6, style="W"
)
volume_result <- ngeo_moran(
  volume_difference, volume_weights, layer=1,
  permutations=19, seed=4102
)
ngeo_inference_contract(volume_result)

# This plot is intentionally emitted into the tutorial, not hidden in a
# temporary PDF.
plot(
  volume_difference, layer=1,
  slice=round(volume_dim[[3L]] / 2), palette="Blue-Red 3"
)

volume_output <- tempfile(fileext=".nii.gz")
volume_manifest <- write_ngeo_nifti(volume_difference, volume_output)
volume_roundtrip <- read_ngeo_nifti(
  volume_manifest$data,
  mask=brain_mask,
  layers=ngeo_layers(volume_difference),
  measures=ngeo_measures(volume_difference),
  checksum=TRUE
)
stopifnot(
  identical(dim(ngeo_values(volume_roundtrip)),
            dim(ngeo_values(volume_difference)))
)
ngeo_history(volume_roundtrip)
unlink(unlist(volume_manifest), force=TRUE)
