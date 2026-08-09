# Phase 3 design-amendment review packet

Status: proposal only. No primary result may be generated from this packet
until the maintainer and an independent scientific reviewer approve a
separately hashed amendment record.

## VAL-301 recommendation

The proposed amendment binds each public procedure to a statistic, generative
null, hypothesis, multiplicity family, comparator, and failure behavior. It
uses only zero-autocorrelation, trend-absent cells for type-I calibration.
Nonzero autocorrelation and unremoved trends become power or
misspecification/stress cells and cannot be counted as type-I failures.

The proposed confirmatory family consists of global Moran total permutation
and local Moran conditional/total permutation with Holm adjustment across all
retained local endpoints in one map. Surface spin and the eigen-sign surrogate
remain experimental and outside C02. Their scientific nulls require separate
designs; passing algebraic invariants is not sufficient for promotion.

The reviewer must answer every `review_questions` item in
`phase3-amendment-proposals-6.0.json`. Approval must identify the commit,
reviewer, decision record, timestamp, final machine artifact, and its SHA-256.

## VAL-308 recommendation

The original Cartesian factor `subjects_or_supports` changes meaning among
exact, iterative, file-backed, and support-family paths. It also does not name
the workload or small exact oracle. The proposal therefore recommends a new
path-specific registry with stable axis names and metrics for each workload.
The already-passing seven large-scale cases remain useful C08 regression
evidence but cannot become primary VAL-308 cells retroactively.

If reviewers instead require the original 36-cell Cartesian layout, they must
define one common estimand and one meaning for every factor across all four
paths before execution. The implementation team must not infer those meanings
from the existing seven results.

`tools/check-phase3-amendment-proposals-60.R` verifies that this packet remains
unapproved, that no primary results are claimed, and that the original Phase 3
design hash has not changed. Its passing result is preparation evidence only.
