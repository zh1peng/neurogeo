manifest_path <- file.path(
  "inst", "spec", "documentation-manifest-6.0.csv"
)
manifest <- utils::read.csv(
  manifest_path, stringsAsFactors = FALSE, check.names = FALSE,
  fileEncoding = "UTF-8"
)

critical_ids <- c(
  "home", "guide", "installation", "tutorial-index", "module-index",
  "concepts-index", "options", "glossary", "getting-started",
  "format-workflows", "workflow-volume", "workflow-surface",
  "workflow-cifti", "workflow-roi-cohort", "core-concepts",
  "reading-data", "neighbors-and-weights", "parcellation-and-aggregation",
  "change-of-support", "spatial-modelling", "support-uncertainty",
  "support-aware-inference", "quality-control", "schema-validation",
  "reproducible-replay"
)

for (id in critical_ids) {
  rows <- manifest[
    manifest$document_id == id & manifest$locale %in% c("zh-CN", "en"),
    , drop = FALSE
  ]
  if (!identical(sort(rows$locale), c("en", "zh-CN"))) {
    stop("Critical documentation is not bilingual: ", id)
  }
  if (any(rows$translation_status != "paired") ||
      any(!nzchar(rows$counterpart_route)) ||
      any(rows$lifecycle != "stable")) {
    stop("Critical bilingual contract is incomplete: ", id)
  }
  counterpart <- rows$route[match(
    ifelse(rows$locale == "en", "zh-CN", "en"), rows$locale
  )]
  if (!identical(rows$counterpart_route, counterpart)) {
    stop("Counterpart routes are not reciprocal: ", id)
  }
}

source_only <- manifest$translation_status == "source-only"
if (any(nzchar(manifest$counterpart_route[source_only]))) {
  stop("Source-only documentation cannot declare a counterpart route.")
}
if (any(!manifest$translation_status %in%
        c("paired", "source-only", "not-applicable"))) {
  stop("Unknown documentation translation status.")
}

renderer <- paste(
  readLines("tools/render-vitepress.R", encoding = "UTF-8", warn = FALSE),
  collapse = "\n"
)
required_notices <- c(
  "本页目前没有经过审校的英文译文",
  "does not yet have a reviewed"
)
if (any(!vapply(required_notices, grepl, logical(1), x = renderer,
                fixed = TRUE))) {
  stop("The documentation renderer lacks a bilingual source-only notice.")
}

cat(
  "Localization contract:", length(critical_ids),
  "critical document IDs are paired;", sum(source_only),
  "source-language routes are explicitly marked.\n"
)
