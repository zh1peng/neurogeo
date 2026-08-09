suppressPackageStartupMessages(library(neurogeo))

budget <- ngeo_resource_budget(
  memory_bytes = 1024,
  elapsed_seconds = 1,
  blocks = 1,
  materialized_elements = 1
)
stopifnot(
  utils::compareVersion(as.character(packageVersion("neurogeo")), "6.0.0") >= 0L,
  inherits(budget, "ngeo_resource_budget"),
  identical(budget$memory_bytes, 1024),
  identical(budget$blocks, 1)
)

cat("Imports-only smoke passed on", R.version.string, "\n")
