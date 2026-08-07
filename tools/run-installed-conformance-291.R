args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("release", "maintenance-291-validation.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("Installed conformance requires jsonlite and digest.")
}
suppressPackageStartupMessages(library(neurogeo))
package_version <- as.character(utils::packageVersion("neurogeo"))
if (utils::compareVersion(package_version, "2.9.1") < 0L) {
  stop("Installed conformance requires neurogeo 2.9.1 or later.")
}

corpus_directory <- if (
  utils::compareVersion(package_version, "3.5.0") >= 0L
) "conformance-ngcs35" else if (
  utils::compareVersion(package_version, "3.4.0") >= 0L
) "conformance-ngcs34" else if (
  utils::compareVersion(package_version, "3.3.0") >= 0L
) "conformance-ngcs33" else if (
  utils::compareVersion(package_version, "3.2.0") >= 0L
) "conformance-ngcs32" else if (
  utils::compareVersion(package_version, "3.1.0") >= 0L
) "conformance-ngcs31" else if (
  utils::compareVersion(package_version, "3.0.0") >= 0L
) "conformance-ngcs30" else "conformance-ngcs29"
manifest_path <- system.file(
  "extdata", corpus_directory, "manifest.json",
  package = "neurogeo"
)
formats_path <- system.file(
  "spec", "supported-formats.md", package = "neurogeo"
)
required_specs <- c(
  "NGCS-2.9.md", "NGCS-2.9.1.md",
  "API-2.9.md", "API-2.9.1.md",
  "migration-2.9.md", "migration-2.9.1.md"
)
if (utils::compareVersion(package_version, "3.0.0") >= 0L) {
  required_specs <- c(
    required_specs, "NGCS-3.0.md", "API-3.0.md", "migration-3.0.md"
  )
}
if (utils::compareVersion(package_version, "3.1.0") >= 0L) {
  required_specs <- c(
    required_specs, "NGCS-3.1.md", "API-3.1.md", "migration-3.1.md"
  )
}
if (utils::compareVersion(package_version, "3.2.0") >= 0L) {
  required_specs <- c(
    required_specs, "NGCS-3.2.md", "API-3.2.md", "migration-3.2.md"
  )
}
if (utils::compareVersion(package_version, "3.3.0") >= 0L) {
  required_specs <- c(
    required_specs, "NGCS-3.3.md", "API-3.3.md", "migration-3.3.md"
  )
}
if (utils::compareVersion(package_version, "3.4.0") >= 0L) {
  required_specs <- c(
    required_specs, "NGCS-3.4.md", "API-3.4.md", "migration-3.4.md"
  )
}
if (utils::compareVersion(package_version, "3.5.0") >= 0L) {
  required_specs <- c(
    required_specs, "NGCS-3.5.md", "API-3.5.md", "migration-3.5.md"
  )
}
if (utils::compareVersion(package_version, "4.0.0") >= 0L) {
  required_specs <- c(
    required_specs, "API-4.0.md", "migration-4.0.md"
  )
}
if (utils::compareVersion(package_version, "4.1.0") >= 0L) {
  required_specs <- c(
    required_specs, "API-4.1.md", "migration-4.1.md"
  )
}
if (utils::compareVersion(package_version, "4.1.1") >= 0L) {
  required_specs <- c(
    required_specs, "API-4.1.1.md", "migration-4.1.1.md"
  )
}
if (utils::compareVersion(package_version, "4.2.0") >= 0L) {
  required_specs <- c(
    required_specs,
    "API-4.2.md",
    "migration-4.2.md",
    "scientific-validation-4.2.md"
  )
}
if (utils::compareVersion(package_version, "4.2.1") >= 0L) {
  required_specs <- c(
    required_specs,
    "API-4.2.1.md",
    "migration-4.2.1.md",
    "API-tiers-4.2.1.md"
  )
}
if (utils::compareVersion(package_version, "4.2.2") >= 0L) {
  required_specs <- c(
    required_specs,
    "API-4.2.2.md",
    "migration-4.2.2.md",
    "real-data-validation-4.2.2.md"
  )
}
if (utils::compareVersion(package_version, "4.3.0") >= 0L) {
  required_specs <- c(
    required_specs,
    "API-4.3.md",
    "migration-4.3.md",
    "cortical-cartography-4.3.md"
  )
}
if (utils::compareVersion(package_version, "4.3.1") >= 0L) {
  required_specs <- c(
    required_specs,
    "cortical-flatmap-4.3.1.md"
  )
}
if (utils::compareVersion(package_version, "4.4.0") >= 0L) {
  required_specs <- c(
    required_specs,
    "API-4.4.md",
    "migration-4.4.md"
  )
}
if (utils::compareVersion(package_version, "4.4.1") >= 0L) {
  required_specs <- c(
    required_specs,
    "API-4.4.1.md",
    "migration-4.4.1.md"
  )
}
if (utils::compareVersion(package_version, "4.4.2") >= 0L) {
  required_specs <- c(
    required_specs,
    "README.md",
    "API-4.4.2.md",
    "migration-4.4.2.md"
  )
}
for (version in c("4.5", "4.6", "4.7", "4.8", "4.9")) {
  if (utils::compareVersion(package_version, paste0(version, ".0")) >= 0L) {
    required_specs <- c(
      required_specs,
      paste0("NGCS-", version, ".md"),
      paste0("API-", version, ".md"),
      paste0("migration-", version, ".md")
    )
  }
}
if (utils::compareVersion(package_version, "5.0.0") >= 0L) {
  required_specs <- c(
    required_specs,
    "NGCS-5.0.md", "API-5.0.md", "migration-5.0.md",
    "validation-5.0.md"
  )
}
spec_paths <- system.file("spec", required_specs, package = "neurogeo")
if (!nzchar(manifest_path) || !file.exists(manifest_path) ||
    !nzchar(formats_path) || !file.exists(formats_path) ||
    any(!nzchar(spec_paths)) || any(!file.exists(spec_paths))) {
  stop("Installed NGCS resources are incomplete.")
}
manifest <- neurogeo:::.ngeo_conformance_manifest()
formats <- paste(readLines(formats_path, warn = FALSE), collapse = "\n")
reviewed_status <- if (
  utils::compareVersion(package_version, "4.4.2") >= 0L
) {
  "Status: reviewed for neurogeo 4.4.2"
} else if (
  utils::compareVersion(package_version, "4.3.1") >= 0L
) {
  "Status: reviewed for neurogeo 4.3.1"
} else if (
  utils::compareVersion(package_version, "4.3.0") >= 0L
) {
  "Status: reviewed for neurogeo 4.3.0"
} else if (
  utils::compareVersion(package_version, "4.2.2") >= 0L
) {
  "Status: reviewed for neurogeo 4.2.2"
} else if (utils::compareVersion(package_version, "4.2.1") >= 0L) {
  "Status: reviewed for neurogeo 4.2.1"
} else {
  "Status: reviewed for neurogeo 4.2.0"
}
required_format_text <- c(
  reviewed_status,
  "pure-R CIFTI-2 writer",
  "NGCS support map schema 2",
  "BIDS derivative data + JSON"
)
format_consistent <- all(vapply(
  required_format_text,
  grepl,
  logical(1),
  x = formats,
  fixed = TRUE
)) && !grepl("rejects CIFTI writing", formats, fixed = TRUE)
if (!format_consistent) {
  stop("Installed supported-format inventory is inconsistent.")
}

scientific_validation_consistent <- NA
if (utils::compareVersion(package_version, "4.2.0") >= 0L) {
  scientific_path <- system.file(
    "spec", "scientific-validation-4.2.md",
    package = "neurogeo"
  )
  scientific_text <- paste(
    readLines(scientific_path, warn = FALSE),
    collapse = "\n"
  )
  scientific_validation_consistent <- all(vapply(
    c(
      "spdep", "spatialreg", "gstat", "GWmodel",
      "type-I error", "The evidence does not establish"
    ),
    grepl,
    logical(1),
    x = scientific_text,
    fixed = TRUE
  ))
  if (!scientific_validation_consistent) {
    stop("Installed 4.2 scientific-validation contract is incomplete.")
  }
}

real_data_validation_consistent <- NA
if (utils::compareVersion(package_version, "4.2.2") >= 0L) {
  real_data_path <- system.file(
    "spec", "real-data-validation-4.2.2.md",
    package = "neurogeo"
  )
  real_data_text <- paste(
    readLines(real_data_path, warn = FALSE),
    collapse = "\n"
  )
  real_data_validation_consistent <- all(vapply(
    c(
      "NIfTI", "GIFTI/FreeSurfer", "dscalar/dlabel/dtseries",
      "download-only", "The evidence does not establish"
    ),
    grepl,
    logical(1),
    x = real_data_text,
    fixed = TRUE
  ))
  if (!real_data_validation_consistent) {
    stop("Installed 4.2.2 real-data validation contract is incomplete.")
  }
}

cartography_consistent <- NA
if (utils::compareVersion(package_version, "4.3.0") >= 0L) {
  cartography_path <- system.file(
    "spec", "cortical-cartography-4.3.md",
    package = "neurogeo"
  )
  cartography_text <- paste(
    readLines(cartography_path, warn = FALSE),
    collapse = "\n"
  )
  cartography_consistent <- all(vapply(
    c(
      "atlas-independent",
      "MUST NOT invent a cut",
      "is_metric_flattening = FALSE",
      "seam-crossing",
      "source domain hash",
      "does not perform surface reconstruction"
    ),
    grepl,
    logical(1),
    x = cartography_text,
    fixed = TRUE
  ))
  cartography_exports <- c(
    "ngeo_flatten_surface",
    "ngeo_project_surface",
    "ngeo_cortical_map",
    "ngeo_cortical_map_data",
    "ngeo_cortical_layout"
  )
  if (!cartography_consistent ||
      any(!cartography_exports %in% getNamespaceExports("neurogeo"))) {
    stop("Installed 4.3 cortical-cartography contract is incomplete.")
  }
}

flatmap_consistent <- NA
if (utils::compareVersion(package_version, "4.3.1") >= 0L) {
  flatmap_path <- system.file(
    "spec", "cortical-flatmap-4.3.1.md",
    package = "neurogeo"
  )
  flatmap_text <- paste(
    readLines(flatmap_path, warn = FALSE),
    collapse = "\n"
  )
  flatmap_consistent <- all(vapply(
    c(
      "face_subset",
      "anatomical underlay",
      "atlas boundaries",
      "does not infer a cut",
      "download-only HCP S1200 Conte69 32k"
    ),
    grepl,
    logical(1),
    x = flatmap_text,
    fixed = TRUE
  ))
  if (!flatmap_consistent) {
    stop("Installed 4.3.1 cortical-flatmap contract is incomplete.")
  }
}

qc_consistent <- NA
if (utils::compareVersion(package_version, "4.4.0") >= 0L) {
  qc_path <- system.file("spec", "API-4.4.md", package = "neurogeo")
  qc_text <- paste(readLines(qc_path, warn = FALSE), collapse = "\n")
  qc_consistent <- all(vapply(
    c(
      "ngeo_qc()",
      "not_evaluated",
      "does not replace",
      "max_value_cells"
    ),
    grepl,
    logical(1),
    x = qc_text,
    fixed = TRUE
  ))
  layout_formals <- names(formals(neurogeo::ngeo_cortical_layout))
  if (!qc_consistent ||
      !"ngeo_qc" %in% getNamespaceExports("neurogeo") ||
      !"shared_scale" %in% layout_formals) {
    stop("Installed 4.4 quality-control contract is incomplete.")
  }
}

ci_matrix_configured <- NA
ci_installed_gate <- NA
workflow_path <- file.path(
  ".github", "workflows", "R-CMD-check.yaml"
)
if (file.exists(workflow_path)) {
  workflow <- paste(
    readLines(workflow_path, warn = FALSE), collapse = "\n"
  )
  ci_matrix_configured <- all(vapply(
    c(
      "ubuntu-latest", "macos-latest", "windows-latest",
      "r: release", "r: devel", "r: oldrel-1"
    ),
    grepl,
    logical(1),
    x = workflow,
    fixed = TRUE
  ))
  ci_installed_gate <- grepl(
    "Rscript tools/run-installed-conformance-291.R",
    workflow,
    fixed = TRUE
  )
  if (!ci_matrix_configured || !ci_installed_gate) {
    stop("Cross-platform installed conformance CI is incomplete.")
  }
}

exports <- getNamespaceExports("neurogeo")
result <- list(
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  package_version = package_version,
  validation = "passed",
  installed = list(
    corpus_version = manifest$corpus_version,
    specification_count = length(manifest$specifications),
    fixture_count = length(manifest$fixtures),
    format_inventory_consistent = format_consistent,
    scientific_validation_consistent =
      scientific_validation_consistent,
    real_data_validation_consistent =
      real_data_validation_consistent,
    cartography_consistent = cartography_consistent,
    flatmap_consistent = flatmap_consistent,
    qc_consistent = qc_consistent,
    required_specs = required_specs,
    public_exports = length(exports),
    deprecated_exports = 0L
  ),
  source_audit = list(
    canonical_spec_source = "inst/spec",
    ci_matrix_configured = ci_matrix_configured,
    installed_conformance_gate = ci_installed_gate,
    remote_results_claimed = FALSE
  ),
  platform = R.version$platform,
  r_version = R.version.string
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
