# External gates and handoff for 6.0

This document lists work that source-code changes cannot honestly close. The
canonical machine-readable state is `inst/spec/audit-task-status-6.0.csv`.

## Required next decisions and evidence

1. Complete `P1-formative-01` with at least five real neuroimaging users using
   `inst/validation/formative-usability-6.0.md`; enter raw outcomes in the
   existing CSV template and run the strict checker. Do not insert synthetic
   participants.
2. Obtain scientific review of the issues in the VAL-301 design audit. Freeze
   a separately hashed amendment before generating any primary result. It must
   name the public procedure, statistic, generative null, tested hypothesis,
   multiplicity family, comparator, and null-versus-power cells.
3. Run VAL-302 on a licensed environment with Connectome Workbench and
   FreeSurfer. Preserve executable versions, command lines, fixture hashes,
   stdout/stderr, and per-cell metrics. The prerequisite report alone is not
   parity evidence.
4. Implement and run the complete 36-cell VAL-308 benchmark only after its
   registered dependencies close. Preserve all attempted fits and observed
   accuracy, convergence, peak-memory, time, and failure-rate values.
5. Ask an external validator to rerun the key simulations from the immutable
   candidate. Complete UX-301 with 8--15 non-developer users using
   `inst/validation/ux301-protocol-6.0.md`; preserve every attempted row and
   run `Rscript tools/check-ux301-60.R --require-complete`.
6. Enable protected `main`, obtain independent reviews, then create the signed
   prerelease, GitHub Release, and Zenodo record from the identical attested
   source archive. Source files must not assert those external states early.
7. Start PUB-401--PUB-407 only after their machine dependencies close. Use
   `inst/validation/phase4-external-study-protocol-6.0.md`, copy the bundled
   evidence template to `release/phase4-evidence-6.0.json`, and retain the
   versioned schema with the archived receipt. These tasks require licensed
   operator sources, predefined discovery/replication cohorts, an independent
   laboratory, DOI-backed artifacts, a rewritten manuscript, and external
   go/no-go review. Negative findings remain results. The final handoff must
   pass `Rscript tools/check-phase4-readiness-60.R --require-complete`.

Until these items are complete, 6.0 is an internally remediated candidate, not
an externally validated scientific release or a Nature Communications-ready
submission.
