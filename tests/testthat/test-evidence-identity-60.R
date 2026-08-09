evidence_helpers <- new.env(parent = globalenv())
evidence_helpers$`%||%` <- function(x, y) if (is.null(x)) y else x
sys.source(
  testthat::test_path("..", "..", "tools", "evidence-identity-60.R"),
  envir = evidence_helpers
)

test_that("evidence identity rejects stale, missing, and mismatched reports", {
  expected <- list(
    package_version = "6.0.0",
    source_commit = paste(rep("a", 40), collapse = ""),
    candidate_tar = list(path = "candidate.tar.gz", sha256 = paste(
      rep("b", 64), collapse = ""
    )),
    dependencies = list(sha256 = paste(rep("c", 64), collapse = "")),
    fixtures = list(corpus = list(path = "corpus.R", sha256 = paste(
      rep("d", 64), collapse = ""
    )))
  )
  valid <- list(
    schema = "neurogeo/evidence-report/1",
    suite = "science",
    candidate = expected,
    checks = list(regressions = TRUE),
    seeds = list(spin = 6002L),
    pass = TRUE
  )

  expect_true(evidence_helpers$ngeo_validate_evidence_identity(
    valid, expected, seeds = TRUE
  )$pass)

  old <- valid
  old$candidate$package_version <- "4.9.0"
  expect_false(evidence_helpers$ngeo_validate_evidence_identity(
    old, expected, seeds = TRUE
  )$pass)
  old$candidate$package_version <- "5.0.0"
  expect_false(evidence_helpers$ngeo_validate_evidence_identity(
    old, expected, seeds = TRUE
  )$pass)

  missing <- valid
  missing$candidate <- NULL
  expect_false(evidence_helpers$ngeo_validate_evidence_identity(
    missing, expected, seeds = TRUE
  )$pass)

  mismatch <- valid
  mismatch$candidate$candidate_tar$sha256 <- paste(rep("e", 64), collapse = "")
  expect_false(evidence_helpers$ngeo_validate_evidence_identity(
    mismatch, expected, seeds = TRUE
  )$pass)
})
