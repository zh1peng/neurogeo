args <- commandArgs(trailingOnly = TRUE)
if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
output <- if (length(args)) args[[1L]] else
  file.path("check-output", "release-scope-waivers-60.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE)) {
  stop("Release-scope waiver checking requires jsonlite and digest.")
}

path <- file.path("inst", "spec", "release-scope-waivers-6.0.json")
waivers <- jsonlite::read_json(path, simplifyVector = FALSE)
items <- waivers$waivers
ids <- vapply(items, function(item) item$waiver_id[[1L]], character(1))
expected <- list(
  `WVR-REV-001` = c("VAL-301", "VAL-308"),
  `WVR-UX-001` = c("TUT-101", "UX-301"),
  `WVR-PUB-001` = paste0("PUB-40", 1:7),
  `WVR-PR-001` = c("SEC-201", "REL-201")
)
item_by_id <- function(id) items[[match(id, ids)]]
checks <- list(
  schema = identical(
    waivers$schema[[1L]], "neurogeo/release-scope-waivers/1"
  ),
  package_version = identical(
    waivers$package_version[[1L]],
    read.dcf("DESCRIPTION", fields = "Version")[[1L]]
  ),
  explicit_owner_decision = identical(
    waivers$decision_status[[1L]], "owner-authorized-for-current-delivery"
  ) && nzchar(waivers$authorized_by[[1L]]) &&
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", waivers$authorized_on[[1L]]),
  exact_waiver_set = setequal(ids, names(expected)) && !anyDuplicated(ids),
  exact_affected_tasks = setequal(ids, names(expected)) && all(vapply(
    names(expected), function(id) {
      setequal(unlist(item_by_id(id)$affected_tasks), expected[[id]])
    }, logical(1)
  )),
  nonempty_boundaries = all(vapply(items, function(item) {
    nzchar(item$waived_requirement[[1L]]) &&
      nzchar(item$retained_boundary[[1L]]) &&
      isTRUE(item$does_not_assert_completion[[1L]])
  }, logical(1))),
  narrative_present = file.exists(file.path(
    "inst", "spec", "release-scope-waivers-6.0.md"
  ))
)
pass <- all(unlist(checks, use.names = FALSE))
result <- list(
  schema = "neurogeo/release-scope-waiver-check/1",
  package_version = waivers$package_version[[1L]],
  generated_at_utc = format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  waiver_sha256 = digest::digest(
    path, algo = "sha256", file = TRUE, serialize = FALSE
  ),
  active_waiver_ids = ids,
  checks = checks,
  pass = pass,
  evidence_boundary = waivers$evidence_boundary[[1L]]
)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  result, output, pretty = TRUE, auto_unbox = TRUE, null = "null"
)
cat(normalizePath(output, winslash = "/", mustWork = TRUE), "\n")
if (!pass) quit(status = 2L)
