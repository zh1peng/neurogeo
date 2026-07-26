# Migrating from neurogeo 3.3 to 3.4

Exact 2.4 model calls are unchanged. Select iterative APIs explicitly and
construct one `ngeo_solver_control()` so tolerance, iteration, approximation,
seed, worker, non-convergence, and resource decisions are auditable.

Use exact-small log determinants to calibrate a workflow, then opt into
Hutchinson power series for larger domains and inspect both error
diagnostics. Iterative CAR requires explicit precision and symmetric sparse
weights. Batched GWR/kriging change execution grouping only.
