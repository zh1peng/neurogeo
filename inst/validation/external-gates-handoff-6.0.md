# External gates and handoff for 6.0

This document lists work that source-code changes cannot honestly close. The
canonical machine-readable state is `inst/spec/audit-task-status-6.0.csv` and
the owner-authorized current-delivery exceptions are recorded separately in
`inst/spec/release-scope-waivers-6.0.json`.

VAL-302 is no longer an external gate. Its complete frozen-grid run, 90 command
receipts, and offline evidence checker are committed under `inst/validation`.
FreeSurfer and Workbench were external comparators only and did not become
runtime dependencies.

## Required next decisions and evidence

1. If the UX waiver is lifted, complete `P1-formative-01` with at least five real neuroimaging users using
   `inst/validation/formative-usability-6.0.md`; enter raw outcomes in the
   existing CSV template and run the strict checker. Do not insert synthetic
   participants.
2. If the scientific-review waiver is lifted, obtain review of the issues in the VAL-301 design audit. Freeze
   a separately hashed amendment before generating any primary result. It must
   name the public procedure, statistic, generative null, tested hypothesis,
   multiplicity family, comparator, and null-versus-power cells.
3. Implement and run the complete 36-cell VAL-308 benchmark only after its
   registered dependencies close. Preserve all attempted fits and observed
   accuracy, convergence, peak-memory, time, and failure-rate values.
4. If the UX waiver is lifted, ask an external validator to rerun the key simulations from the immutable
   candidate. Complete UX-301 with 8--15 non-developer users using
   `inst/validation/ux301-protocol-6.0.md`; preserve every attempted row and
   run `Rscript tools/check-ux301-60.R --require-complete`.
5. Recheck protected `main` with
   `Rscript tools/check-github-governance-60.R`. The current merge has an
   owner-authorized approval waiver, but CI and the remaining protections are
   still mandatory. A future formal release still needs a signed prerelease,
   GitHub Release, and Zenodo record from the identical attested source archive.
6. If the Phase 4 waiver is lifted, start PUB-401--PUB-407 only after their machine dependencies close. Use
   `inst/validation/phase4-external-study-protocol-6.0.md`, copy the bundled
   evidence template to `release/phase4-evidence-6.0.json`, and retain the
   versioned schema with the archived receipt. These tasks require licensed
   operator sources, predefined discovery/replication cohorts, an independent
   laboratory, DOI-backed artifacts, a rewritten manuscript, and external
   go/no-go review. Negative findings remain results. The final handoff must
   pass `Rscript tools/check-phase4-readiness-60.R --require-complete`.

The current owner waivers narrow delivery scope; they do not prove the omitted
activities. Until the unwaived immutable-release evidence exists, 6.0 is a
remediated package with bounded VAL-302 external evidence, not a fully
externally validated scientific release or a Nature Communications-ready
submission.
