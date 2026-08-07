args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args) >= 1L) args[[1L]] else
  file.path("check-output", "real-multilayer-50-validation.json")
cache <- if (length(args) >= 2L) args[[2L]] else
  file.path(".tools", "reference-5.0")
required_packages <- c("digest", "gifti", "jsonlite", "Matrix", "RSpectra")
missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Real multilayer 5.0 validation requires: ", paste(missing, collapse = ", "))
}
suppressPackageStartupMessages(library(neurogeo))

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
manifest_path <- file.path("inst", "extdata", "reference-5.0", "manifest.csv")
manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                            check.names = FALSE)
fixture <- function(name) {
  row <- manifest[manifest$name == name, , drop = FALSE]
  assert(nrow(row) == 1L, paste("Unknown 5.0 fixture:", name))
  path <- file.path(cache, row$file)
  valid <- file.exists(path) &&
    identical(as.numeric(file.info(path)$size), as.numeric(row$size)) &&
    identical(
      digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE),
      row$sha256
    )
  assert(valid, paste("Missing or invalid 5.0 fixture:", name))
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
read_labels <- function(name) {
  as.integer(scan(fixture(name), what = numeric(), sep = ",", quiet = TRUE))
}

started <- proc.time()[["elapsed"]]
covariates <- utils::read.csv(fixture("enigma-covariates"),
                              stringsAsFactors = FALSE)
thickness <- utils::read.csv(fixture("enigma-cortical-thickness"),
                             stringsAsFactors = FALSE, check.names = FALSE)
area <- utils::read.csv(fixture("enigma-cortical-area"),
                        stringsAsFactors = FALSE, check.names = FALSE)
assert(identical(covariates$SubjID, thickness$SubjID) &&
       identical(covariates$SubjID, area$SubjID),
       "ENIGMA subject rows are not exactly aligned.")

left <- read_ngeo_gifti(fixture("enigma-conte69-left"), checksum = TRUE)
right <- read_ngeo_gifti(fixture("enigma-conte69-right"), checksum = TRUE)
left_coordinates <- left$base$geometry$coordinates[[left$base$geometry$active_coordinates]]
right_coordinates <- right$base$geometry$coordinates[[right$base$geometry$active_coordinates]]
left_n <- nrow(left_coordinates)
coordinates <- rbind(left_coordinates, right_coordinates)
faces <- rbind(left$base$geometry$faces, right$base$geometry$faces + left_n)
space <- ngeo_coordinate_space(
  space_id = "ENIGMA-Conte69-32k",
  kind = "surface",
  unit = "mm",
  template = "Conte69",
  density = "32k",
  source_metadata = list(
    project = "ENIGMA Toolbox",
    commit = unique(manifest$source_commit)
  )
)
geometry <- ngeo_surface(
  coordinates, faces, coordinate_space = space,
  index_base = "one", source_index_base = 0L
)
assert(nrow(geometry$base$elements) == 64984L &&
       nrow(geometry$base$geometry$faces) == 129960L,
       "The bilateral Conte69 base has unexpected dimensions.")

dk_numeric <- read_labels("enigma-aparc-conte69")
schaefer100 <- read_labels("enigma-schaefer100-conte69")
schaefer200 <- read_labels("enigma-schaefer200-conte69")
assert(length(dk_numeric) == nrow(coordinates) &&
       length(schaefer100) == nrow(coordinates) &&
       length(schaefer200) == nrow(coordinates),
       "A support label vector is not vertex aligned.")

# ENIGMA's DK table contains 34 left then 34 right regions. Codes 35 and 70
# are the two non-analysis labels; code 0 is medial wall.
dk_codes <- c(1:34, 36:69)
thickness_columns <- names(thickness)[2:69]
area_columns <- names(area)[2:69]
dk_names <- sub("_thickavg$", "", thickness_columns)
assert(length(dk_codes) == length(dk_names) &&
       identical(sub("_thickavg$", "", thickness_columns),
                 sub("_surfavg$", "", area_columns)),
       "DK thickness and area columns are not semantically aligned.")
dk_index <- match(dk_numeric, dk_codes)
analysed <- !is.na(dk_index)
vertex_support <- ngeo_support_size(geometry)
template_region_support <- vapply(seq_along(dk_codes), function(i) {
  sum(vertex_support[which(dk_index == i)])
}, numeric(1))
assert(all(is.finite(template_region_support)) &&
       all(template_region_support > 0),
       "Every DK region needs positive template support.")

subject_objects <- vector("list", nrow(covariates))
names(subject_objects) <- covariates$SubjID
metadata <- vector("list", nrow(covariates))
for (subject in seq_len(nrow(covariates))) {
  subject_thickness <- as.numeric(thickness[subject, thickness_columns])
  subject_area <- as.numeric(area[subject, area_columns])
  area_density <- subject_area / template_region_support
  vertex_values <- matrix(0, nrow(coordinates), 2L)
  vertex_values[analysed, 1L] <- subject_thickness[dk_index[analysed]]
  vertex_values[analysed, 2L] <- area_density[dk_index[analysed]]
  map_names <- paste0(
    covariates$SubjID[[subject]],
    c("__thickness", "__relative_area_density")
  )
  subject_objects[[subject]] <- ngeo_surface(
    coordinates, faces,
    values = vertex_values,
    layers = data.frame(name = map_names, stringsAsFactors = FALSE),
    measures = rbind(
      ngeo_measure(
        value_type = "continuous", support_behavior = "intensive", unit = "mm"
      ),
      ngeo_measure(
        value_type = "continuous", support_behavior = "intensive", unit = "1"
      )
    ),
    coordinate_space = space, index_base = "one", source_index_base = 0L
  )
  metadata[[subject]] <- data.frame(
    subject_id = rep(covariates$SubjID[[subject]], 2L),
    feature = c("thickness", "relative_area_density"),
    role = "observed",
    source_id = "ENIGMA-example",
    stringsAsFactors = FALSE
  )
}
metadata <- do.call(rbind, metadata)
rownames(metadata) <- NULL
bind_arguments <- c(
  subject_objects,
  list(metadata = metadata, conflicts = "prefix", storage = "delayed")
)
stack <- do.call(ngeo_bind_layers, bind_arguments)
assert(inherits(stack$values, "ngeo_delayed_values") && ncol(stack$values) == 40L,
       "The real subject stack did not retain delayed aligned values.")

dk_labels <- rep.int(NA_character_, length(dk_numeric))
dk_labels[analysed] <- dk_names[dk_index[analysed]]
support_labels <- list(
  DK68 = dk_labels,
  Schaefer100 = ifelse(schaefer100 == 0L, NA_character_,
                       sprintf("Schaefer100_%03d", schaefer100)),
  Schaefer200 = ifelse(schaefer200 == 0L, NA_character_,
                       sprintf("Schaefer200_%03d", schaefer200))
)

features <- list()
support_inventory <- list()
for (support_name in names(support_labels)) {
  partition <- ngeo_partition(stack, support_labels[[support_name]])
  regional <- ngeo_aggregate(stack, partition)
  index <- ngeo_validate_layers(
    regional,
    unit = "subject_id",
    layer = "feature",
    required_layers = c("thickness", "relative_area_density"),
    complete = "error"
  )
  spatial_weights <- ngeo_spatial_weights(
    regional, method = "region_contiguity", style = "B"
  )
  basis <- ngeo_spatial_basis(
    regional, spatial_weights = spatial_weights, n_modes = 8L,
    components = "separate"
  )
  current <- ngeo_layer_coupling(
    regional,
    index = index,
    pairs = data.frame(
      x = "thickness", y = "relative_area_density",
      stringsAsFactors = FALSE
    ),
    basis = basis,
    bands = list(modes_001_008 = 1:8),
    estimands = c("same_location", "spectral_coupling"),
    chunk_layers = 4L
  )
  features[[support_name]] <- current
  support_inventory[[support_name]] <- list(
    regions = nrow(regional$base$elements),
    components = length(basis$components),
    basis_hash = basis$basis_hash,
    support_hash = current$history$support_hash,
    endpoints = ncol(current$values),
    maximum_eigen_residual = basis$diagnostics$max_residual,
    missing_endpoints = current$diagnostics$missing_endpoints,
    scale_type = "rank_matched",
    change_of_support_model = "DK piecewise-constant lift on Conte69 vertices"
  )
}

design <- data.frame(
  unit_id = covariates$SubjID,
  diagnosis = factor(covariates$Dx, levels = c(0, 1),
                     labels = c("control", "epilepsy")),
  age = covariates$Age,
  sex = factor(covariates$Sex),
  stringsAsFactors = FALSE
)
subject_schedule <- ngeo_exchangeability(
  design$unit_id, scheme = "free", permutations = 199L, seed = 5000L
)
subject_fit <- ngeo_group_test(
  features = features$DK68,
  data = design,
  model = ~ diagnosis + age + sex,
  test = "diagnosis",
  exchangeability = subject_schedule,
  adjustment = "maxT"
)
family_schedule <- ngeo_exchangeability(
  design$unit_id, scheme = "free", permutations = 199L, seed = 5001L
)
family_fit <- ngeo_group_test(
  features = features,
  data = design,
  model = ~ diagnosis + age + sex,
  test = "diagnosis",
  exchangeability = family_schedule,
  adjustment = "maxT"
)

common_schedule <- identical(
  family_fit$support$schedule_hash, family_schedule$schedule_hash
) && isTRUE(family_fit$diagnostics$common_schedule_all_supports)
support_rows <- sum(vapply(support_inventory, `[[`, integer(1), "regions"))
pass <- nrow(design) == 20L &&
  identical(as.integer(table(design$diagnosis)), c(10L, 10L)) &&
  length(features) == 3L && support_rows == 368L &&
  all(vapply(support_inventory, function(value) {
    value$components == 2L && value$missing_endpoints == 0L &&
      value$maximum_eigen_residual < 1e-6
  }, logical(1))) && common_schedule &&
  nrow(subject_fit$tests) == ncol(features$DK68$values) &&
  nrow(family_fit$tests) == sum(vapply(
    features, function(value) ncol(value$values), integer(1)
  ))
if (!pass) {
  stop(
    "The real multilayer/support-family workflow failed its gates: ",
    paste(capture.output(str(list(
      groups = table(design$diagnosis),
      supports = support_inventory,
      common_schedule = common_schedule,
      subject_test_rows = nrow(subject_fit$tests),
      family_test_rows = nrow(family_fit$tests),
      expected_test_rows = sum(vapply(
        features, function(value) ncol(value$values), integer(1)
      ))
    ))), collapse = " "),
    call. = FALSE
  )
}

used_rows <- manifest[, c(
  "name", "source_project", "source_commit", "source_url",
  "license_record", "terms_url", "redistribution", "size", "sha256"
)]
report <- list(
  schema = "neurogeo/real-multilayer-50-validation",
  package_version = as.character(utils::packageVersion("neurogeo")),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  elapsed_seconds = unname(proc.time()[["elapsed"]] - started),
  source = used_rows,
  preprocessing_provenance = list(
    source = "ENIGMA Toolbox example data from one MICA-MNI site",
    cortical_thickness = "FreeSurfer vertex thickness averaged in DK parcels",
    cortical_area = "FreeSurfer triangle area totals in DK parcels",
    neurogeo_transform = paste(
      "area total divided by Conte69 DK template support, then lifted",
      "piecewise-constantly with thickness to the common surface"
    )
  ),
  sample = list(
    included = nrow(design),
    control = sum(design$diagnosis == "control"),
    epilepsy = sum(design$diagnosis == "epilepsy"),
    exclusions = list(count = 0L, subject_id = character(), reason = character()),
    independent_unit = "subject"
  ),
  layers = list(
    thickness = list(unit = "mm", semantics = "intensive"),
    relative_area_density = list(unit = "1", semantics = "intensive")
  ),
  supports = support_inventory,
  workflows = list(
    subject_level_multilayer = list(
      support = "DK68", model = "~ diagnosis + age + sex",
      test = "diagnosis", permutations = subject_schedule$permutations,
      schedule_hash = subject_schedule$schedule_hash,
      endpoints = nrow(subject_fit$tests),
      minimum_raw_p = min(subject_fit$tests$p_raw),
      minimum_maxT_p = min(subject_fit$tests$p_maxT),
      population_inference = TRUE
    ),
    support_family_replication = list(
      supports = names(features), model = "~ diagnosis + age + sex",
      test = "diagnosis", permutations = family_schedule$permutations,
      schedule_hash = family_schedule$schedule_hash,
      common_schedule = common_schedule,
      family_endpoints = nrow(family_fit$tests),
      minimum_raw_p = min(family_fit$tests$p_raw),
      minimum_maxT_p = min(family_fit$tests$p_maxT),
      population_inference = TRUE
    ),
    claim_scope = paste(
      "execution and numerical validation on the 20-subject ENIGMA example;",
      "not a powered clinical claim"
    )
  ),
  dependencies = lapply(
    c("neurogeo", required_packages),
    function(package) list(
      package = package,
      version = as.character(utils::packageVersion(package))
    )
  ),
  pass = pass
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE,
                     digits = 16, null = "null")
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
