# Neuroimaging Geoinformatics Core Specification 2.8 addendum

Status: stable  
Version: 2.8  
Base specification: NGCS 2.7

NGCS 2.8 standardizes explicit coordinate-space registries and supplied
transform graphs. All NGCS 1.0-2.7 contracts remain.

A registered space identity MUST hash its complete normative signature:
space ID, kind, units, structure, template, density, and resolution. Matching
space names alone MUST NOT establish equivalence. An alias MUST target one
exact registered space hash and alias conflicts or ambiguous names MUST fail.

A space audit MUST compare kind, units, dimensionality, structure, template,
density, and resolution. Mismatches MUST remain visible even when a caller
supplies a transform connecting the spaces.

A transform graph contains only caller-supplied transforms. Every directed
edge MUST bind exact source and target space hashes, a transform hash, stable
edge ID, inversion eligibility, and lossy status. Graph and edge mutation
MUST be detected. The graph MUST NOT estimate registration, infer spatial
equivalence, repair metadata, resample, or interpolate.

Path search MUST be deterministic and directed. Only explicitly eligible,
non-lossy, supported affine edges MAY be inverted. If multiple shortest paths
exist, search MUST fail until the caller selects an exact edge-token sequence.
Cycles, ambiguous pairs, unit/structure mismatches, and density/resolution
differences MUST be rejected or explicitly diagnosed.

A path provenance record MUST include source and target space hashes, graph
hash, ordered edge IDs, edge hashes, reversal flags, lossy flags, and path
hash. Application requires explicit caller authorization and MUST reject any
lossy or unsupported non-affine path. Application changes geometry only and
does not change values, element order, topology, support, or measurement
semantics.

Conformance requires direct affine composition references, alias and
ambiguity adversarial fixtures, inversion/loss tests, graph mutation tests,
cycle and field-mismatch diagnostics, and exact per-edge provenance.
