.ngeo_nifti_transforms <- function(image, header) {
  qform <- if (header$qform_code > 0L) {
    matrix(
      as.numeric(RNifti::xform(image, useQuaternionFirst = TRUE)),
      nrow = 4L
    )
  } else {
    NULL
  }
  sform <- if (header$sform_code > 0L) {
    matrix(
      as.numeric(RNifti::xform(image, useQuaternionFirst = FALSE)),
      nrow = 4L
    )
  } else {
    NULL
  }
  list(
    qform = qform,
    qform_code = header$qform_code,
    sform = sform,
    sform_code = header$sform_code
  )
}

.ngeo_nifti_affine <- function(transforms,
                               affine = c("auto", "sform", "qform")) {
  if (is.matrix(affine)) {
    if (!identical(dim(affine), c(4L, 4L))) {
      .ngeo_abort(
        "Explicit NIfTI `affine` must be 4 by 4.",
        "ngeo_error_geometry"
      )
    }
    return(list(matrix = affine, source = "explicit"))
  }
  affine <- match.arg(affine)
  source <- affine
  if (identical(affine, "auto")) {
    source <- if (!is.null(transforms$sform)) "sform" else "qform"
  }
  matrix <- transforms[[source]]
  if (is.null(matrix)) {
    .ngeo_abort(
      sprintf("Requested NIfTI %s is not present.", source),
      "ngeo_error_transform"
    )
  }
  list(matrix = matrix, source = source)
}

.ngeo_nifti_mask <- function(mask, image, lattice_dim) {
  if (is.null(mask)) {
    return(NULL)
  }
  if (is.character(mask) && length(mask) == 1L) {
    if (identical(mask, "nonzero")) {
      values <- matrix(as.array(image), nrow = prod(lattice_dim))
      return(rowSums(
        !is.na(values) & values != 0
      ) > 0L)
    }
    mask_image <- .ngeo_backend_read(
      "NIfTI mask",
      mask,
      function() RNifti::readNifti(mask)
    )
    if (!identical(as.integer(dim(mask_image)[1:3]), lattice_dim)) {
      .ngeo_abort(
        "NIfTI mask dimensions do not match the image.",
        "ngeo_error_alignment"
      )
    }
    return(as.vector(as.array(mask_image)) != 0)
  }
  mask
}

#' Read NIfTI as an NGCS volume
#'
#' @param path NIfTI path.
#' @param mask Optional mask path, logical array/vector, or `"nonzero"`.
#' @param frames Optional frames passed to `RNifti::readNifti()`.
#' @param layers Optional layer metadata.
#' @param measures Optional measurement semantics.
#' @param coordinate_space Optional `ngeo_coordinate_space`.
#' @param affine Active affine choice.
#' @param load_data Whether to retain values.
#' @param strict Whether to run strict validation.
#' @param checksum Whether to record a SHA-256 source identity. A legacy MD5
#'   field is retained through 6.x for history compatibility.
#'
#' @return An `ngeo_volume` object.
#' @templateVar example_call read_ngeo_nifti("sub-01_T1w.nii.gz")
#' @template stable-io
#' @export
read_ngeo_nifti <- function(path,
                            mask = NULL,
                            frames = NULL,
                            layers = NULL,
                            measures = NULL,
                            coordinate_space = NULL,
                            affine = c("auto", "sform", "qform"),
                            load_data = TRUE,
                            strict = TRUE,
                            checksum = TRUE) {
  .ngeo_require("RNifti", "NIfTI reading")
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ngeo_abort("`path` must name an existing NIfTI file.", "ngeo_error_argument")
  }

  image <- .ngeo_backend_read(
    "NIfTI",
    path,
    function() RNifti::readNifti(
      path,
      internal = !isTRUE(load_data),
      volumes = frames,
      json = "ignore"
    )
  )
  header <- RNifti::niftiHeader(image, unused = TRUE)
  lattice_dim <- as.integer(header$dim[2:4])
  transforms <- .ngeo_nifti_transforms(image, header)
  active <- .ngeo_nifti_affine(transforms, affine)

  if (!is.null(transforms$qform) && !is.null(transforms$sform) &&
      !isTRUE(all.equal(
        transforms$qform,
        transforms$sform,
        tolerance = 1e-6,
        check.attributes = FALSE
      ))) {
    .ngeo_warn(
      "NIfTI qform and sform differ; both were preserved.",
      "ngeo_warning_transform_conflict"
    )
  }

  pixunits <- attr(header, "pixunits")
  unit <- if (length(pixunits)) pixunits[[1L]] else "mm"
  coordinate_space <- coordinate_space %||% ngeo_coordinate_space(
    "unknown",
    kind = "volume",
    unit = unit,
    source_metadata = list(
      qform_code = header$qform_code,
      sform_code = header$sform_code
    )
  )
  values <- if (isTRUE(load_data)) as.array(image) else NULL
  active_mask <- .ngeo_nifti_mask(mask, image, lattice_dim)
  image_dim <- as.integer(header$dim[2:(header$dim[[1L]] + 1L)])
  n_layer <- if (length(image_dim) <= 3L) {
    1L
  } else {
    prod(image_dim[-seq_len(3L)])
  }
  if (is.null(layers)) {
    source_frame <- if (is.null(frames)) {
      seq_len(n_layer) - 1L
    } else {
      frames
    }
    layers <- data.frame(
      name = paste0("frame_", seq_len(n_layer)),
      source_frame = source_frame,
      stringsAsFactors = FALSE
    )
  }

  x <- ngeo_volume(
    values = values,
    dim = lattice_dim,
    affine = active$matrix,
    mask = active_mask,
    layers = layers,
    measures = measures,
    coordinate_space = coordinate_space,
    index_base = "zero"
  )
  x$base$geometry$header_transforms <- c(
    transforms,
    list(active = active$source)
  )
  x$history$header_summary <- list(
    version = attr(header, "version"),
    dim = header$dim,
    pixdim = header$pixdim,
    datatype = header$datatype,
    intent_code = header$intent_code,
    intent_name = header$intent_name,
    descrip = header$descrip
  )
  sidecar <- .ngeo_bids_sidecar(path)
  if (!is.null(sidecar)) {
    x$history$bids_sidecar <- sidecar
  }
  paths <- c(path, if (is.character(mask) &&
    length(mask) == 1L &&
    file.exists(mask)) mask)
  x <- .ngeo_append_import_provenance(
    x,
    paths = paths,
    importer = "read_ngeo_nifti",
    metadata = list(
      active_affine = active$source,
      frames = frames,
      load_data = load_data
    ),
    checksum = checksum
  )
  if (isTRUE(strict)) {
    ngeo_validate(x, "strict")
  }
  x
}
