# Migration to neurogeo 4.4.1

No object migration is required from 4.4.0.

Exact CAR operations above 2,000 observations now stop before dense
allocation. Code that intentionally uses a larger exact problem must set
`options(neurogeo.max_exact_logdet = n)` to an explicit reviewed bound, or
move to a supported iterative method.

Generic `write_ngeo()` calls may now write standard CIFTI suffixes directly.
Existing calls to `write_ngeo_cifti()` remain valid.
