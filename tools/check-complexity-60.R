baseline_path <- file.path(
  "inst", "validation", "function-complexity-ceilings-6.0.csv"
)
if (!file.exists(baseline_path)) stop("Missing function-complexity ceilings.")

observed_path <- tempfile(fileext = ".csv")
on.exit(unlink(observed_path), add = TRUE)
status <- system2(
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }),
  c("tools/audit-complexity-60.R", observed_path)
)
if (!identical(status, 0L)) stop("Could not audit function complexity.")

observed <- utils::read.csv(observed_path, stringsAsFactors = FALSE)
ceilings <- utils::read.csv(baseline_path, stringsAsFactors = FALSE)
required <- c(
  "source", "function_name", "line_ceiling", "branch_ceiling"
)
if (!identical(names(ceilings), required) ||
    anyDuplicated(paste(ceilings$source, ceilings$function_name))) {
  stop("Invalid function-complexity ceiling schema.")
}

key <- paste(observed$source, observed$function_name)
ceiling_key <- paste(ceilings$source, ceilings$function_name)
for (i in seq_len(nrow(ceilings))) {
  current <- match(ceiling_key[[i]], key)
  if (is.na(current)) next
  if (observed$line_count[[current]] > ceilings$line_ceiling[[i]] ||
      observed$branch_points[[current]] > ceilings$branch_ceiling[[i]]) {
    stop("High-complexity function exceeded its 6.0 ceiling: ",
         ceilings$function_name[[i]])
  }
}

high_risk <- observed$line_count >= 150L | observed$branch_points >= 30L
new_high_risk <- setdiff(key[high_risk], ceiling_key)
if (length(new_high_risk)) {
  stop("New unbounded high-complexity function: ", new_high_risk[[1L]])
}

cat(
  "Complexity ceilings hold for", nrow(ceilings),
  "high-risk 6.0 functions; no new high-risk function was introduced.\n"
)
