# Phase 4 external study and publication evidence protocol

Status: execution contract; blocked until the dependencies recorded in
`inst/spec/audit-task-status-6.0.csv` are complete. This packet is not evidence
that PUB-401--PUB-407 have been performed.

## Evidence identity and storage

Run all Phase 4 work against one immutable release candidate. Copy
`inst/validation/phase4-evidence-template-6.0.json` to
`release/phase4-evidence-6.0.json` and populate it only with real records.
Every record must bind the signed tag, 40-character source commit, P0
attestation SHA-256, public evidence location, reviewer, and review record.
The release receipt is intentionally ignored by git; archive it with the
external study materials and publication source data.

Validate preparation with:

```sh
Rscript tools/check-phase4-readiness-60.R
```

Use `--require-complete` only for the final handoff. A successful non-strict
check of the bundled empty template means only that the contract is readable.

## PUB-401: verified operator registry

Register, without reimplementing registration, the operators needed by the
frozen study. Coverage must include fsaverage, fsLR, MNI, a CIFTI density, and
at least one named atlas. Each record states source and target, direction,
version, SHA-256, upstream source and DOI, redistribution license, legal
metric, expected coverage, QC procedure and outcome, reviewer, and review
record. Bidirectional claims require separately verified directional records.

## PUB-402: discovery and external replication cohorts

Freeze endpoints, equivalence/robustness boundaries, missing-data rules,
exclusions, multiplicity, and analysis commit before accessing outcomes.
Include at least one discovery and one independently defined replication
cohort/site, with distinct sites under the preregistered independence rule.
Preserve positive, null, and failed results. At least one cohort must support
complete public reconstruction under its data-use terms. Record ethics or
documented exemption; never place participant data in this receipt.

## PUB-403: ablations and matched competitors

Report the seven frozen categories: area weighting, metric, single atlas
versus ensemble, iid versus spatial null, uncertainty model, guard on/off,
and a mature competitor. Competitor comparisons must share the same estimand,
input support, mask, metric, null, endpoint, and evaluation split. Record all
attempts, including failures, with a source-data hash.

## PUB-404: independent laboratory reconstruction

At least one laboratory with no development, review, or authorship role must
reconstruct a main figure using only the public release materials. Record the
expected and observed source-data hashes, public deviations and failures, and
the resolution in the final release. A developer-guided session does not
qualify.

## PUB-405 and PUB-406: open release and manuscript consistency

The receipt identifies DOI-pinned code, data, operators, and source data; a
locked environment or container; and one public command that rebuilds every
figure and table. Licenses, ORCIDs, and author contributions must be complete.
The manuscript record binds the submitted artifact and its SHA-256 and
confirms that claims, equations, statistics, limitations, stable/experimental
labels, Code/Data Availability, references, and source data agree with 6.x.

## PUB-407: external go/no-go review

Obtain two or three reviews from independent experts. Each review must answer
novelty, broad interest, technical validity, and independent-evidence
adequacy, cite a public or archived review record, and recommend `go`, `no-go`,
or `revise`. A complete `no-go` is a valid study outcome: choose a more
appropriate journal instead of expanding scope to manufacture novelty.

## Evidence boundary

The JSON Schema and checker validate completeness, identity, frozen category
coverage, and basic consistency. They cannot determine scientific validity,
independence, legal redistribution, ethical compliance, or journal fit.
Named external reviewers remain responsible for those judgments.
