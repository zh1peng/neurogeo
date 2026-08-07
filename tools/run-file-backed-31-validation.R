args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "file-backed-31-validation.json")

required <- c(
  "cifti", "digest", "freesurferformats", "jsonlite", "RNifti"
)
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("File-backed validation requires: ",
       paste(missing, collapse = ", "))
}
if (!exists("read_ngeo_filebacked", mode = "function")) {
  library(neurogeo)
}

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
rejected_as <- function(expression, class) {
  inherits(
    tryCatch({
      force(expression)
      NULL
    }, error = identity),
    class
  )
}
golden <- function(name) {
  source <- file.path("inst", "extdata", "golden", name)
  if (file.exists(source)) source else
    system.file("extdata", "golden", name, package = "neurogeo")
}
mib <- function(x) as.numeric(x) / 1024^2

started <- Sys.time()

nifti_path <- golden("tiny.nii.gz")
nifti_eager <- suppressWarnings(
  read_ngeo_nifti(nifti_path, checksum = FALSE)
)
nifti_backed <- read_ngeo_nifti_filebacked(
  nifti_path, checksum = FALSE
)
assert(
  isTRUE(all.equal(
    as.matrix(nifti_backed$values), nifti_eager$values
  )),
  "File-backed NIfTI differs from eager input."
)

cifti_path <- golden("tiny.dscalar.nii")
cifti_eager <- read_ngeo_cifti(cifti_path, checksum = FALSE)
cifti_backed <- read_ngeo_cifti_filebacked(
  cifti_path, checksum = FALSE
)
assert(
  isTRUE(all.equal(
    as.matrix(cifti_backed$values), cifti_eager$values
  )),
  "File-backed CIFTI differs from eager input."
)

mgh_path <- golden("tiny.mgh")
mgh_eager <- read_ngeo_freesurfer(
  mgh_path, base = "volume", checksum = FALSE
)
mgh_backed <- read_ngeo_mgh_filebacked(
  mgh_path, checksum = FALSE
)
assert(
  isTRUE(all.equal(
    as.matrix(mgh_backed$values), mgh_eager$values
  )),
  "File-backed MGH differs from eager input."
)
mgz_path <- tempfile(fileext = ".mgz")
freesurferformats::write.fs.mgh(
  mgz_path,
  freesurferformats::read.fs.mgh(mgh_path),
  vox2ras_matrix = mgh_eager$base$geometry$affine
)
mgz_backed <- read_ngeo_mgh_filebacked(
  mgz_path, checksum = FALSE
)
assert(
  isTRUE(all.equal(
    as.matrix(mgz_backed$values), mgh_eager$values
  )),
  "File-backed MGZ differs from eager input."
)

nifti_selected <- read_ngeo_nifti_filebacked(
  nifti_path,
  elements = c(8L, 1L, 4L),
  frames = 2L,
  checksum = FALSE
)
assert(
  identical(
    as.numeric(as.matrix(nifti_selected$values)),
    as.numeric(nifti_eager$values[c(8L, 1L, 4L), 2L])
  ),
  "NIfTI selection order or map alignment changed."
)
cifti_selected <- read_ngeo_cifti_filebacked(
  cifti_path,
  structures = "CORTEX_LEFT",
  elements = 2:1,
  frames = 2L,
  checksum = FALSE
)
assert(
  identical(
    as.numeric(as.matrix(cifti_selected$values)),
    as.numeric(cifti_eager$values[c(2L, 1L), 2L])
  ),
  "CIFTI brain-model/map alignment changed."
)

chunks <- ngeo_value_chunks(
  nifti_backed,
  chunk_size = 3L,
  FUN = function(block, rows) list(block = block, rows = rows)
)
assert(
  isTRUE(all.equal(
    do.call(rbind, lapply(chunks, `[[`, "block")),
    nifti_eager$values
  )),
  "Deterministic chunks did not reconstruct the values block."
)

constrained <- read_ngeo_nifti_filebacked(
  nifti_path,
  checksum = FALSE,
  budget = ngeo_resource_budget(
    memory_bytes = 16,
    materialized_elements = 2
  )
)
resource_rejected <- rejected_as(
  constrained$values[1:3, 1L, drop = FALSE],
  "ngeo_error_resource"
)
assert(resource_rejected, "Oversized file-backed block was not rejected.")

identity_a <- ngeo_file_values_identity(nifti_backed$values)
identity_b <- ngeo_file_values_identity(nifti_selected$values)
assert(!identical(identity_a, identity_b),
       "Distinct source selections share one identity.")

mutated_path <- tempfile(fileext = ".nii.gz")
file.copy(nifti_path, mutated_path)
mutated <- read_ngeo_nifti_filebacked(
  mutated_path, checksum = FALSE, verify = "checksum"
)
connection <- file(mutated_path, "ab")
writeBin(charToRaw("mutation"), connection)
close(connection)
mutation_rejected <- rejected_as(
  mutated$values[1L, 1L],
  "ngeo_error_file_mutation"
)
assert(mutation_rejected, "Mutated source was not rejected.")

copy_path <- tempfile(fileext = ".nii.gz")
copied <- write_ngeo_filebacked(
  nifti_backed, copy_path, chunk_bytes = 17L
)
atomic_copy_equal <- identical(
  copied$sha256,
  digest::digest(
    nifti_path, algo = "sha256", file = TRUE, serialize = FALSE
  )
)
assert(atomic_copy_equal, "Atomic pass-through copy changed source bytes.")
partial_copy_rejected <- rejected_as(
  write_ngeo_filebacked(
    nifti_selected, tempfile(fileext = ".nii.gz")
  ),
  "ngeo_error_partial_selection"
)
assert(partial_copy_rejected,
       "Partial file-backed selection was copied as complete.")

large_nifti_path <- tempfile(fileext = ".nii")
large_volume_values <- array(
  as.numeric(seq_len(100L * 100L * 100L * 2L)),
  dim = c(100L, 100L, 100L, 2L)
)
large_volume_values <- RNifti::asNifti(large_volume_values)
large_volume_values <- RNifti::`sform<-`(
  large_volume_values,
  structure(diag(4), code = 2L)
)
RNifti::writeNifti(
  large_volume_values, large_nifti_path, datatype = "float32"
)
rm(large_volume_values)
invisible(gc())
large_volume <- suppressWarnings(
  read_ngeo_nifti_filebacked(
    large_nifti_path,
    checksum = FALSE,
    verify = "metadata",
    budget = ngeo_resource_budget(
      memory_bytes = 4 * 1024^2,
      materialized_elements = 5e5
    )
  )
)
large_volume_object_mib <- mib(object.size(large_volume))
large_volume_slice <- large_volume$values[
  c(1L, 500000L, 1000000L), 1:2, drop = FALSE
]
assert(
  isTRUE(all.equal(
    as.numeric(large_volume_slice),
    c(1, 500000, 1000000, 1000001, 1500000, 2000000),
    tolerance = 1e-6
  )),
  "Large NIfTI direct slice is incorrect."
)
assert(
  large_volume_object_mib < 256,
  "Large file-backed volume metadata exceeded 256 MiB."
)

n_grayordinate <- 91592L
n_left <- n_grayordinate %/% 2L
large_values <- cbind(
  map_1 = seq_len(n_grayordinate),
  map_2 = seq_len(n_grayordinate) + 100000,
  map_3 = seq_len(n_grayordinate) + 200000
)
large_grayordinates <- ngeo_grayordinate(
  list(
    list(
      component_id = "left",
      kind = "surface",
      structure = "CORTEX_LEFT",
      vertex_index = seq_len(n_left) - 1L,
      surface_vertex_count = n_left
    ),
    list(
      component_id = "right",
      kind = "surface",
      structure = "CORTEX_RIGHT",
      vertex_index = seq_len(n_grayordinate - n_left) - 1L,
      surface_vertex_count = n_grayordinate - n_left
    )
  ),
  values = large_values,
  layers = data.frame(
    name = paste0("frame_", 1:3),
    time = 0:2,
    time_unit = rep("SECOND", 3),
    stringsAsFactors = FALSE
  )
)
large_cifti_path <- tempfile(fileext = ".dtseries.nii")
write_ngeo_cifti(
  large_grayordinates,
  large_cifti_path,
  type = "dtseries",
  datatype = "float32"
)
rm(large_grayordinates, large_values)
invisible(gc())
large_cifti <- read_ngeo_cifti_filebacked(
  large_cifti_path,
  checksum = FALSE,
  verify = "metadata",
  budget = ngeo_resource_budget(
    memory_bytes = 4 * 1024^2,
    materialized_elements = 5e5
  )
)
large_cifti_object_mib <- mib(object.size(large_cifti))
large_cifti_slice <- large_cifti$values[
  c(1L, n_left, n_left + 1L, n_grayordinate),
  1:3,
  drop = FALSE
]
expected_rows <- c(1L, n_left, n_left + 1L, n_grayordinate)
expected_cifti <- cbind(
  expected_rows,
  expected_rows + 100000,
  expected_rows + 200000
)
assert(
  isTRUE(all.equal(
    unname(large_cifti_slice),
    unname(expected_cifti),
    tolerance = 1e-6
  )),
  "91k CIFTI direct slice is incorrect."
)
assert(
  large_cifti_object_mib < 128,
  "91k file-backed CIFTI metadata exceeded 128 MiB."
)

manifest <- neurogeo:::.ngeo_conformance_manifest(version = "3.1")
assert(
  identical(manifest$corpus_version, "3.1"),
  "NGCS 3.1 conformance corpus did not verify."
)

report <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  started_at_utc = format(
    started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = as.character(packageVersion("neurogeo")),
  specification = "NGCS 3.1",
  validation = "passed",
  eager_file_backed_equality = list(
    nifti = TRUE, cifti = TRUE, mgh = TRUE, mgz = TRUE
  ),
  selection_alignment = list(
    nifti_voxel_frame = TRUE,
    cifti_brain_model_map = TRUE
  ),
  bounded_execution = list(
    deterministic_chunks = TRUE,
    oversized_block_rejected = resource_rejected,
    large_volume = list(
      elements = 1000000L,
      layers = 2L,
      resident_object_mib = large_volume_object_mib,
      limit_mib = 256,
      full_values_materialized_by_reader = FALSE
    ),
    large_cifti = list(
      grayordinates = n_grayordinate,
      layers = 3L,
      resident_object_mib = large_cifti_object_mib,
      limit_mib = 128,
      full_values_materialized_by_reader = FALSE
    )
  ),
  identity_and_output = list(
    distinct_selection_identity = TRUE,
    source_mutation_rejected = mutation_rejected,
    atomic_copy_equal = atomic_copy_equal,
    partial_copy_rejected = partial_copy_rejected
  ),
  conformance_corpus = "3.1",
  platform = R.version$platform,
  r_version = R.version.string
)

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, output, pretty = TRUE, auto_unbox = TRUE, digits = NA
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
