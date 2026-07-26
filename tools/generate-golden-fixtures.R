output_dir <- file.path("inst", "extdata", "golden")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

coordinates <- rbind(
  c(0, 0, 0),
  c(1, 0, 0),
  c(0.5, sqrt(3) / 2, 0),
  c(0.5, sqrt(3) / 6, sqrt(2 / 3))
)
faces <- rbind(
  c(1L, 2L, 3L),
  c(1L, 2L, 4L),
  c(2L, 3L, 4L),
  c(3L, 1L, 4L)
)
metric <- c(1.5, 2.5, 3.5, 4.5)

freesurferformats::write.fs.surface(
  file.path(output_dir, "tetra.surface"),
  coordinates,
  faces
)
freesurferformats::write.fs.curv(
  file.path(output_dir, "tetra.curv"),
  metric
)
freesurferformats::write.fs.surface.gii(
  file.path(output_dir, "tetra.surf.gii"),
  coordinates,
  faces
)
freesurferformats::write.fs.morph.gii(
  file.path(output_dir, "tetra.shape.gii"),
  metric
)

colortable <- data.frame(
  struct_name = c("background", "region"),
  r = c(0L, 220L),
  g = c(0L, 20L),
  b = c(0L, 60L),
  a = c(0L, 0L),
  struct_index = c(0L, 1L),
  stringsAsFactors = FALSE
)
annot_path <- file.path(output_dir, "tetra.annot")
freesurferformats::write.fs.annot(
  annot_path,
  num_vertices = 4L,
  colortable = colortable,
  labels_as_indices_into_colortable = c(1L, 2L, 2L, 1L)
)
annot <- freesurferformats::read.fs.annot(annot_path)
freesurferformats::write.fs.annot.gii(
  file.path(output_dir, "tetra.label.gii"),
  annot
)

volume_values <- array(seq_len(16), dim = c(2, 2, 2, 2))
affine <- diag(c(2, 3, 4, 1))
affine[1:3, 4] <- c(10, 20, 30)
freesurferformats::write.fs.mgh(
  file.path(output_dir, "tiny.mgh"),
  volume_values,
  vox2ras_matrix = affine
)

image <- RNifti::asNifti(volume_values)
transform <- affine
attr(transform, "code") <- 2L
RNifti::sform(image) <- transform
RNifti::qform(image) <- transform
RNifti::writeNifti(
  image,
  file.path(output_dir, "tiny.nii.gz"),
  datatype = "int32"
)

metadata <- list(
  RepetitionTime = 2,
  SpatialReference = "synthetic",
  Description = "Synthetic neurogeo golden fixture"
)
jsonlite::write_json(
  metadata,
  file.path(output_dir, "tiny.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

write_fixed_string <- function(connection, value, size) {
  bytes <- charToRaw(enc2utf8(value))
  if (length(bytes) > size) {
    stop("Fixed string is too long.")
  }
  writeBin(c(bytes, raw(size - length(bytes))), connection)
}

write_int64 <- function(connection, values) {
  for (value in values) {
    if (value < 0 || value >= 2^31) {
      stop("Fixture int64 writer only supports small non-negative values.")
    }
    writeBin(
      c(as.integer(value), 0L),
      connection,
      size = 4L,
      endian = "little"
    )
  }
}

write_tiny_cifti <- function(path,
                             values,
                             mapping_xml,
                             intent_code,
                             intent_name) {
  values <- as.matrix(values)
  n_grayordinate <- nrow(values)
  n_map <- ncol(values)
  xml_raw <- charToRaw(enc2utf8(mapping_xml))
  extension_size <- ceiling((8L + length(xml_raw)) / 16L) * 16L
  vox_offset <- 544L + extension_size

  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)

  writeBin(540L, connection, size = 4L, endian = "little")
  write_fixed_string(connection, "n+2\r\n\032\n", 8L)
  writeBin(16L, connection, size = 2L, endian = "little")
  writeBin(32L, connection, size = 2L, endian = "little")
  write_int64(
    connection,
    c(6L, 1L, 1L, 1L, 1L, n_map, n_grayordinate, 1L)
  )
  writeBin(rep(0, 3L), connection, size = 8L, endian = "little")
  writeBin(c(0, rep(1, 7L)), connection, size = 8L, endian = "little")
  write_int64(connection, vox_offset)
  writeBin(rep(0, 6L), connection, size = 8L, endian = "little")
  write_int64(connection, c(0L, 0L))
  write_fixed_string(connection, "Synthetic neurogeo CIFTI fixture", 80L)
  write_fixed_string(connection, "", 24L)
  writeBin(c(0L, 0L), connection, size = 4L, endian = "little")
  writeBin(rep(0, 6L), connection, size = 8L, endian = "little")
  writeBin(rep(0, 12L), connection, size = 8L, endian = "little")
  writeBin(
    c(0L, 0L, as.integer(intent_code)),
    connection,
    size = 4L,
    endian = "little"
  )
  write_fixed_string(connection, intent_name, 16L)
  writeBin(raw(16L), connection)
  stopifnot(seek(connection) == 540L)

  writeBin(as.raw(c(1L, 0L, 0L, 0L)), connection)
  writeBin(
    as.integer(extension_size),
    connection,
    size = 4L,
    endian = "little"
  )
  writeBin(32L, connection, size = 4L, endian = "little")
  writeBin(
    c(xml_raw, raw(extension_size - 8L - length(xml_raw))),
    connection
  )
  stopifnot(seek(connection) == vox_offset)

  data_order <- as.numeric(t(values))
  writeBin(data_order, connection, size = 4L, endian = "little")
}

brain_models_xml <- paste0(
  '<MatrixIndicesMap AppliesToMatrixDimension="1" ',
  'IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS">',
  '<BrainModel IndexOffset="0" IndexCount="2" ',
  'ModelType="CIFTI_MODEL_TYPE_SURFACE" ',
  'BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" ',
  'SurfaceNumberOfVertices="4"><VertexIndices>0 2</VertexIndices>',
  '</BrainModel>',
  '<BrainModel IndexOffset="2" IndexCount="2" ',
  'ModelType="CIFTI_MODEL_TYPE_SURFACE" ',
  'BrainStructure="CIFTI_STRUCTURE_CORTEX_RIGHT" ',
  'SurfaceNumberOfVertices="4"><VertexIndices>1 3</VertexIndices>',
  '</BrainModel>',
  '<Volume VolumeDimensions="2,2,2">',
  '<TransformationMatrixVoxelIndicesIJKtoXYZ MeterExponent="-3">',
  '2 0 0 10\n0 3 0 20\n0 0 4 30\n0 0 0 1',
  '</TransformationMatrixVoxelIndicesIJKtoXYZ></Volume>',
  '<BrainModel IndexOffset="4" IndexCount="2" ',
  'ModelType="CIFTI_MODEL_TYPE_VOXELS" ',
  'BrainStructure="CIFTI_STRUCTURE_THALAMUS_LEFT">',
  '<VoxelIndicesIJK>0 0 0\n1 1 1</VoxelIndicesIJK>',
  '</BrainModel>',
  '</MatrixIndicesMap>'
)

dscalar_maps_xml <- paste0(
  '<MatrixIndicesMap AppliesToMatrixDimension="0" ',
  'IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS">',
  '<NamedMap><MapName>effect</MapName></NamedMap>',
  '<NamedMap><MapName>standard_error</MapName></NamedMap>',
  '</MatrixIndicesMap>'
)
dscalar_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<CIFTI Version="2"><Matrix>',
  dscalar_maps_xml,
  brain_models_xml,
  '</Matrix></CIFTI>'
)
write_tiny_cifti(
  file.path(output_dir, "tiny.dscalar.nii"),
  cbind(effect = 1:6, standard_error = seq(0.1, 0.6, by = 0.1)),
  dscalar_xml,
  intent_code = 3006L,
  intent_name = "ConnDenseScalar"
)

dlabel_maps_xml <- paste0(
  '<MatrixIndicesMap AppliesToMatrixDimension="0" ',
  'IndicesMapToDataType="CIFTI_INDEX_TYPE_SCALARS">',
  '<NamedMap><MapName>atlas</MapName><LabelTable>',
  '<Label Key="0" Red="0" Green="0" Blue="0" Alpha="0">background</Label>',
  '<Label Key="1" Red="1" Green="0" Blue="0" Alpha="1">region</Label>',
  '</LabelTable></NamedMap></MatrixIndicesMap>'
)
dlabel_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<CIFTI Version="2"><Matrix>',
  dlabel_maps_xml,
  brain_models_xml,
  '</Matrix></CIFTI>'
)
write_tiny_cifti(
  file.path(output_dir, "tiny.dlabel.nii"),
  matrix(c(0, 1, 1, 0, 1, 0), ncol = 1L),
  dlabel_xml,
  intent_code = 3007L,
  intent_name = "ConnDenseLabel"
)

dtseries_maps_xml <- paste0(
  '<MatrixIndicesMap AppliesToMatrixDimension="0" ',
  'IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES" ',
  'NumberOfSeriesPoints="3" SeriesStart="0" SeriesStep="0.8" ',
  'SeriesUnit="SECOND"/>'
)
dtseries_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<CIFTI Version="2"><Matrix>',
  dtseries_maps_xml,
  brain_models_xml,
  '</Matrix></CIFTI>'
)
write_tiny_cifti(
  file.path(output_dir, "tiny.dtseries.nii"),
  matrix(seq_len(18), nrow = 6L, ncol = 3L),
  dtseries_xml,
  intent_code = 3002L,
  intent_name = "ConnDenseSeries"
)
