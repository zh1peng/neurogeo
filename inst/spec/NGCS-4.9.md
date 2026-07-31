# NGCS 4.9 experimental-method boundary

NGCS 4.9 does not alter the stable domain, values, support, space, topology,
metric, measurement, transform, or provenance contracts. It defines the
boundary between the stable multilayer inference core and optional advanced
method evaluation.

The following invariants apply to all 4.9 experimental results:

1. ordered domain identity and selected map identity are retained;
2. spatial-map analysis has `population_inference = FALSE`;
3. data-driven ordination is not followed by ordinary confirmatory testing on
   the same observations;
4. pair-based methods use an explicit bounded sampling design and record the
   exact sample identity;
5. LMC assumptions and positive-semidefinite sill diagnostics are explicit;
6. MGWR does not expose nominal local p-values as inferential evidence;
7. no experiment silently allocates a full-cortex dense distance or pair
   matrix.

These experimental methods do not change the 5.0 stable API promotion gate.
