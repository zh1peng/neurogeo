# VAL-308 coverage audit

Status: partial core-performance evidence; the frozen VAL-308 release gate is
not satisfied.

`tools/run-full-performance.R` executes seven useful large-scale regression
tests: 164k surface topology/statistics, 91k grayordinate topology, 32k surface
diagnostics, 100k coordinate KNN, 100k-by-1k support change, 100k affine-grid
support construction, and 100k uncertain-support propagation. These tests gate
elapsed time, sparse-object size, topology, conservation, or finite analytic
uncertainty as appropriate. They support the narrower C08 claim that named
core sparse workflows remain within their declared budgets.

They do not implement the frozen `VAL-308` Cartesian design. In particular,
the report has no explicit `(elements, subjects_or_supports, path)` cell rows;
does not cover the exact, iterative, file-backed, and support-family paths at
all registered scales; and does not report per-cell relative error,
convergence, peak process memory, elapsed time, and failed-fit rate. Object
size is not peak process memory. Assigning the existing cases to factorial
cells after seeing their results would be an unregistered reinterpretation.

Before VAL-308 can close, implement a version-bound runner that emits all 36
cell identities and observed metrics, compares small cases with the registered
exact result, binds large cases to the Phase 0 baseline, and applies every
frozen gate without changing the design hash. Until then,
`tools/check-val308-coverage-60.R` must report
`release_gate_satisfied = false` and the project must not describe the seven
core tests as complete VAL-308 evidence.
