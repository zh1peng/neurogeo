#' Read and verify the language-independent NGCS conformance corpus
#'
#' @param path Optional corpus manifest path.
#' @param version Bundled corpus version when `path` is `NULL`.
#' @return The verified manifest.
#' @export
ngeo_conformance_manifest <- function(
    path = NULL,
    version = c(
      "latest", "3.5", "3.4", "3.3", "3.2", "3.1", "3.0", "2.9"
    )) {
  .ngeo_require("jsonlite", "NGCS conformance corpus reading")
  version <- match.arg(version)
  if (is.null(path)) {
    if (identical(version, "latest")) version <- "3.5"
    directory <- switch(
      version,
      "3.5" = "conformance-ngcs35",
      "3.4" = "conformance-ngcs34",
      "3.3" = "conformance-ngcs33",
      "3.2" = "conformance-ngcs32",
      "3.1" = "conformance-ngcs31",
      "3.0" = "conformance-ngcs30",
      "2.9" = "conformance-ngcs29"
    )
    path <- system.file(
      "extdata", directory, "manifest.json",
      package = "neurogeo"
    )
    if (!nzchar(path)) {
      path <- file.path(
        "inst", "extdata", directory, "manifest.json"
      )
    }
  }
  if (!file.exists(path)) {
    .ngeo_abort("NGCS conformance manifest is missing.",
                "ngeo_error_io")
  }
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expected_29 <- c(
    "1.0", "1.1", "1.2", "1.3", "2.0", "2.1", "2.2",
    "2.3", "2.4", "2.5", "2.6", "2.7", "2.8", "2.9"
  )
  expected <- if (identical(manifest$corpus_version, "3.5")) {
    c(
      expected_29, "2.9.1", "3.0", "3.1", "3.2", "3.3", "3.4", "3.5"
    )
  } else if (identical(manifest$corpus_version, "3.4")) {
    c(
      expected_29, "2.9.1", "3.0", "3.1", "3.2", "3.3", "3.4"
    )
  } else if (identical(manifest$corpus_version, "3.3")) {
    c(expected_29, "2.9.1", "3.0", "3.1", "3.2", "3.3")
  } else if (identical(manifest$corpus_version, "3.2")) {
    c(expected_29, "2.9.1", "3.0", "3.1", "3.2")
  } else if (identical(manifest$corpus_version, "3.1")) {
    c(expected_29, "2.9.1", "3.0", "3.1")
  } else if (identical(manifest$corpus_version, "3.0")) {
    c(expected_29, "2.9.1", "3.0")
  } else {
    expected_29
  }
  expected_schema <- if (identical(manifest$corpus_version, "3.5")) {
    "NGCS-conformance-corpus-7"
  } else if (identical(manifest$corpus_version, "3.4")) {
    "NGCS-conformance-corpus-6"
  } else if (identical(manifest$corpus_version, "3.3")) {
    "NGCS-conformance-corpus-5"
  } else if (identical(manifest$corpus_version, "3.2")) {
    "NGCS-conformance-corpus-4"
  } else if (identical(manifest$corpus_version, "3.1")) {
    "NGCS-conformance-corpus-3"
  } else if (identical(manifest$corpus_version, "3.0")) {
    "NGCS-conformance-corpus-2"
  } else {
    "NGCS-conformance-corpus-1"
  }
  versions <- as.character(unlist(manifest$specifications))
  if (!identical(manifest$schema, expected_schema) ||
      !manifest$corpus_version %in%
        c("2.9", "3.0", "3.1", "3.2", "3.3", "3.4", "3.5") ||
      !identical(versions, expected) ||
      !isTRUE(manifest$language_independent) ||
      !is.list(manifest$fixtures) || !length(manifest$fixtures)) {
    .ngeo_abort("NGCS conformance manifest is invalid.",
                "ngeo_error_io")
  }
  directory <- dirname(path)
  for (fixture in manifest$fixtures) {
    fixture_path <- .ngeo_exchange_file(
      directory, fixture$path, "fixture path"
    )
    if (!file.exists(fixture_path) ||
        !identical(.ngeo_file_sha256(fixture_path), fixture$sha256)) {
      .ngeo_abort("NGCS conformance fixture checksum failed.",
                  "ngeo_error_io")
    }
    parsed <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)
    if (!identical(parsed$schema, fixture$schema)) {
      .ngeo_abort("NGCS conformance fixture schema differs.",
                  "ngeo_error_io")
    }
  }
  manifest
}

#' Report the 2.9 cross-platform compatibility matrix
#'
#' @return A compatibility data frame that distinguishes local evidence from
#' configured remote CI.
#' @export
ngeo_compatibility_matrix <- function() {
  data.frame(
    platform = c("Windows", "Linux", "macOS"),
    minimum_r = rep("4.2.0", 3L),
    pure_r_core = TRUE,
    external_neuroimaging_binary = FALSE,
    evidence = c(
      if (.Platform$OS.type == "windows") {
        "local release validation"
      } else {
        "configured CI"
      },
      if (Sys.info()[["sysname"]] == "Linux") {
        "local release validation"
      } else {
        "configured CI; remote result required"
      },
      if (Sys.info()[["sysname"]] == "Darwin") {
        "local release validation"
      } else {
        "configured CI; remote result required"
      }
    ),
    stringsAsFactors = FALSE
  )
}

#' Inventory the public 2.9 API for 3.0 planning
#'
#' @return A data frame of exported APIs and deprecation state.
#' @export
ngeo_api_inventory <- function() {
  lifecycle <- ngeo_api_lifecycle()
  data.frame(
    api = lifecycle$api,
    status_2_9 = ifelse(
      lifecycle$introduced == "<=2.9.1", "stable", "not_exported"
    ),
    planned_3_0_action = lifecycle$planned_action,
    deprecated_in_2_x = FALSE,
    stringsAsFactors = FALSE
  )
}
