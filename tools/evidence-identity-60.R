ngeo_sha256_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing evidence input: ", path, call. = FALSE)
  }
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

ngeo_source_commit <- function() {
  output <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L || length(output) != 1L ||
      !grepl("^[0-9a-f]{40}$", output)) {
    stop("Could not determine the source commit.", call. = FALSE)
  }
  output[[1L]]
}

ngeo_description_version <- function(path = "DESCRIPTION") {
  as.character(read.dcf(path, fields = "Version")[[1L]])
}

ngeo_dependency_identity <- function() {
  description <- read.dcf(
    "DESCRIPTION",
    fields = c("Depends", "Imports", "Suggests", "LinkingTo")
  )
  declared <- unlist(strsplit(
    paste(description[1L, ], collapse = ","),
    ",",
    fixed = TRUE
  ))
  declared <- trimws(sub("\\s*\\(.*$", "", declared))
  declared <- unique(c("R", declared[nzchar(declared)]))
  available <- utils::installed.packages()
  installed <- data.frame(
    Package = declared,
    Version = vapply(declared, function(package) {
      if (identical(package, "R")) {
        return(as.character(getRversion()))
      }
      if (!package %in% rownames(available)) return(NA_character_)
      available[package, "Version"]
    }, character(1)),
    stringsAsFactors = FALSE
  )
  installed <- installed[order(installed$Package), , drop = FALSE]
  text <- paste(installed$Package, installed$Version, sep = "==")
  list(
    sha256 = digest::digest(
      paste(text, collapse = "\n"),
      algo = "sha256",
      serialize = FALSE
    ),
    packages = as.list(stats::setNames(
      installed$Version,
      installed$Package
    ))
  )
}

ngeo_evidence_identity <- function(candidate_tar, fixtures) {
  if (!length(fixtures) || is.null(names(fixtures)) ||
      any(!nzchar(names(fixtures)))) {
    stop("Evidence fixtures must be a named non-empty character vector.",
         call. = FALSE)
  }
  fixture_records <- lapply(fixtures, function(path) {
    list(path = path, sha256 = ngeo_sha256_file(path))
  })
  list(
    package_version = ngeo_description_version(),
    source_commit = ngeo_source_commit(),
    candidate_tar = list(
      path = basename(candidate_tar),
      sha256 = ngeo_sha256_file(candidate_tar)
    ),
    dependencies = ngeo_dependency_identity(),
    fixtures = fixture_records
  )
}

ngeo_validate_evidence_identity <- function(report, expected, seeds = FALSE) {
  failures <- character()
  required <- c("schema", "suite", "candidate", "checks", "pass")
  missing <- setdiff(required, names(report))
  if (length(missing)) {
    failures <- c(failures, paste("missing fields:", paste(missing, collapse = ", ")))
  }
  if (!identical(report$schema, "neurogeo/evidence-report/1")) {
    failures <- c(failures, "schema mismatch")
  }
  candidate <- report$candidate
  if (!is.list(candidate)) {
    failures <- c(failures, "candidate identity missing")
  } else {
    comparisons <- list(
      package_version = identical(
        candidate$package_version, expected$package_version
      ),
      source_commit = identical(
        candidate$source_commit, expected$source_commit
      ),
      tar_sha256 = identical(
        candidate$candidate_tar$sha256, expected$candidate_tar$sha256
      ),
      dependency_sha256 = identical(
        candidate$dependencies$sha256, expected$dependencies$sha256
      ),
      fixtures = identical(candidate$fixtures, expected$fixtures)
    )
    failures <- c(failures, names(comparisons)[!unlist(comparisons)])
  }
  if (isTRUE(seeds) &&
      (!is.list(report$seeds) || !length(report$seeds))) {
    failures <- c(failures, "seed registry missing")
  }
  if (!isTRUE(report$pass)) {
    failures <- c(failures, "suite did not pass")
  }
  list(pass = !length(failures), failures = unique(failures))
}
