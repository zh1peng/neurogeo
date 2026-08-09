if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
contract_path <- file.path(root, "inst", "spec", "condition-contracts-6.0.csv")
snapshot_path <- file.path(
  root, "inst", "spec", "condition-message-snapshots-6.0.csv"
)
stopifnot(file.exists(contract_path), file.exists(snapshot_path))

contracts <- utils::read.csv(
  contract_path, stringsAsFactors = FALSE, check.names = FALSE
)
snapshots <- utils::read.csv(
  snapshot_path, stringsAsFactors = FALSE, check.names = FALSE
)
required_contract <- c("scenario", "parent_class", "code", "field", "hint_contract")
required_snapshot <- c("scenario", "class", "code", "field", "message", "hint")
stopifnot(
  identical(names(contracts), required_contract),
  identical(names(snapshots), required_snapshot),
  !anyDuplicated(contracts$scenario),
  !anyDuplicated(snapshots$scenario),
  all(nzchar(as.matrix(contracts))),
  all(nzchar(as.matrix(snapshots))),
  all(contracts$scenario %in% snapshots$scenario),
  all(grepl("^ngeo_error", contracts$parent_class)),
  all(grepl("^NGEO_ERROR(?:_[A-Z0-9]+)*$", contracts$code))
)

forbidden <- "agent|milestone|phase[ -]?[0-9]|historical note|里程碑|智能体"
if (any(grepl(
  forbidden,
  paste(snapshots$message, snapshots$hint),
  ignore.case = TRUE,
  perl = TRUE
))) {
  stop("Condition snapshots contain an internal milestone or Agent note.")
}

r_files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
condition_source <- unlist(lapply(r_files, function(path) {
  expressions <- parse(path, keep.source = FALSE)
  found <- character()
  walk <- function(value) {
    if (is.call(value) && identical(value[[1L]], as.name(".ngeo_abort"))) {
      found <<- c(found, paste(deparse(value), collapse = " "))
    }
    if (is.recursive(value)) lapply(as.list(value), walk)
    invisible(NULL)
  }
  lapply(as.list(expressions), walk)
  found
}), use.names = FALSE)
if (any(grepl(forbidden, condition_source, ignore.case = TRUE, perl = TRUE))) {
  stop("A user-visible abort contains an internal milestone or Agent note.")
}

if (!requireNamespace("neurogeo", quietly = TRUE)) {
  stop("Install neurogeo before running the runtime condition contract gate.")
}
capture_error <- function(expression) tryCatch(expression, error = identity)
point <- neurogeo::ngeo_point(
  matrix(c(0, 0, 1, 0), ncol = 2L, byrow = TRUE),
  values = cbind(signal = c(1, 2))
)
observed <- list(
  missing_backend = capture_error(neurogeo:::.ngeo_require(
    "definitelyNotANeurogeoPackage", "condition contract audit"
  )),
  invalid_scalar_character = capture_error(neurogeo::ngeo_measure(
    unit = NA_character_
  )),
  missing_layer_metadata = capture_error(neurogeo::ngeo_layer_index(
    point, unit = "subject_id", feature = "feature"
  )),
  unknown_measure = capture_error(neurogeo::ngeo_update_measure(
    point, "absent", unit = "mm"
  )),
  failed_reader = capture_error(neurogeo:::.ngeo_backend_read(
    "NIfTI", "missing.nii", function() stop("synthetic reader failure")
  ))
)
observed <- do.call(rbind, lapply(names(observed), function(scenario) {
  condition <- observed[[scenario]]
  if (!inherits(condition, "ngeo_error")) {
    stop("Scenario did not raise ngeo_error: ", scenario)
  }
  data.frame(
    scenario = scenario,
    class = class(condition)[[1L]],
    code = condition$code,
    field = condition$field,
    message = conditionMessage(condition),
    hint = condition$hint,
    stringsAsFactors = FALSE
  )
}))
expected <- snapshots[match(observed$scenario, snapshots$scenario), , drop = FALSE]
rownames(expected) <- NULL
rownames(observed) <- NULL
if (!identical(observed, expected)) {
  print(all.equal(observed, expected))
  stop("Runtime condition messages differ from the reviewed 6.0 snapshots.")
}
cat(
  "Condition contracts:", nrow(contracts),
  "core scenarios and", length(condition_source), "abort calls audited.\n"
)
