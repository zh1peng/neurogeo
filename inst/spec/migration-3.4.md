# Migrating from neurogeo 3.3 to 3.4

Version 3.4 is additive. `ngeo_spatial_regression()` and `ngeo_car()` retain
their exact-small behavior. Use their iterative counterparts only when an
explicit solver control and visible approximation/convergence report are
required.

For small SAR/SEM calibration, set `logdet = "exact"` and an
`exact_threshold` above the data dimension. For large domains, set
`logdet = "approximate"` and choose trace order/probes so both Monte Carlo
standard error and truncation bound are acceptable for the scientific use.
Parameters outside the declared norm-convergent range are rejected.

Use symmetric binary or otherwise explicitly symmetric weights for iterative
CAR. Precision selection is not implicit in this large-domain path: supply a
positive precision, then inspect the returned CG convergence evidence.

The batched GWR and kriging adapters accept indexed targets. They retain the
original method and order; batch size changes execution only, not estimates.
