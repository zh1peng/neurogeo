# neurogeo 5.0 software validation

This document records the executable validation supplied for neurogeo 5.0.
Generated JSON reports are build artifacts, not hand-edited results. Each
report records package and dependency versions, seeds or hashes, and its own
pass criteria. Decisions about using these reports in a paper belong to the
paper project, not to this package.

| Evidence domain | Frozen source | Required machine report |
|---|---|---|
| analytic basis/projection | 4.5 tests and validation | `multilayer-45-full-validation.json` |
| coupling/reference comparison | 4.6 tests and validation | `coupling-46-full-validation.json` |
| subject null and recovery | 4.7 full calibration | `group-inference-47-full-validation.json` |
| common support-family null | 4.8 full calibration | `support-family-48-full-validation.json` |
| API and condition freeze | 5.0 audit | `freeze-50-audit.json` |
| real subject/support workflows | checksum-pinned ENIGMA example | `real-multilayer-50-validation.json` |
| resource envelope | 5.0 full performance mode | `multilayer-50-performance.json` |
| package regression | complete unit runner | `unit-50.json` |
| distribution | installed conformance and R check | conformance JSON and `00check.log` |

## Frozen calibration results

The predeclared 100-replicate 4.7 simulations yielded family-wise error 0.06
for the pure null, 0.06 with nuisance, 0.04 with site blocks, and sign-flip
type-I error 0.05. Sparse max-T and distributed sum-of-squares power were 0.98
and 0.95. The 4.8 correlated-support simulation yielded endpoint type-I error
0.036875 and full-family FWER 0.01. These values apply only to their declared
simulation designs; they are not universal operating guarantees.

## Real-data evidence

The download-only ENIGMA Toolbox example supplies 10 controls and 10 epilepsy
subjects with DK68 cortical thickness and area. neurogeo converts area totals
to template-support-relative density before a declared piecewise-constant lift
to Conte69. One DK68 analysis validates the subject path; a separate common
schedule validates the DK68, Schaefer100, and Schaefer200 family. This is an
execution and numerical-validation example, not a powered clinical result.

## Resource evidence

The full Windows reference run evaluates 32,400 by 64, 91,200 by 64, and
91,200 by 128 basis cases plus file-backed 100/1000-subject layer cases and
999/4999-transformation group cases. Absolute timings are descriptive and not
portable gates. Required gates are bounded allocation, sparse operators, no
dense element-pair matrix, no default endpoint null matrix, numerical basis
diagnostics, and finite endpoint results.

## Reproducibility and non-claims

`tools/run-release-evidence-50.R` verifies report hashes, complete-suite
status, installed conformance, a local `R CMD check` with `Status: OK`, website outputs,
cross-platform CI configuration, exact dependency versions, and the 5.0
documentation corpus. Passing this audit does not claim clinical validity,
causal interpretation, universal exchangeability, equal physical scale across
rank-matched atlases, or parcellation invariance.

The local `--as-cran` run is retained separately. On the reference Windows
machine it reports only environment/submission checks: unavailable `qpdf`, a
new-submission note, and unavailable network time verification. Package code,
installation, documentation, examples, tests, and vignette rebuilding pass;
managed CI continues to run `--as-cran` on Linux, macOS, and Windows.
