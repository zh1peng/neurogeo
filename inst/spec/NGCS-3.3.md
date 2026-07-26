# Neuroimaging Geoinformatics Core Specification 3.3

Status: stable  
Version: 3.3  
Base specification: NGCS 3.2

NGCS 3.3 adds explicit temporal and spatiotemporal semantics without changing
the core dataset shape. A dataset retains exactly one spatial domain and one
element-by-map values block. Time coordinates align to maps; spatial geometry
MUST NOT be duplicated for each time coordinate.

An `ngcs/time-axis` MUST contain finite strictly increasing coordinates, one
canonical unit, instantaneous or explicit interval support, overlap policy,
tolerance, derived regularity, and an immutable SHA-256 identity. Interval
coordinates MUST lie within positive-width aligned intervals. A conforming
implementation MUST NOT infer time solely from map count.

Each time-aware numeric map MUST declare one of `instantaneous`,
`interval_mean`, `interval_total`, `rate`, or `categorical`. Instantaneous
semantics require instantaneous support. Interval means, totals, and rates
require interval support. Slicing MUST preserve the exact spatial domain,
strict time order, values alignment, semantics, and auditable provenance.

An `ngcs/temporal-weights` object MUST be a non-negative, zero-diagonal sparse
operator bound to one exact time-axis identity. An
`ngcs/spatiotemporal-weights` object MUST retain one domain-bound spatial
operator and one axis-bound temporal operator. Kronecker-sum or
Kronecker-product execution is normative in matrix-free separable form.
Materialization of a full space-time operator MUST require an explicit small
observation limit and resource budget.

Temporal and global spatiotemporal Moran statistics MUST use the declared
operators and identities. Temporal and joint space-time semivariograms MUST
declare a hard pair budget, report exact pair counts, and reject incomplete
bin coverage. Numeric statistics MUST reject categorical or non-finite input.

Longitudinal change, trend, and temporal contrasts MUST retain one spatial
domain and one aligned output values block. Means, sums, and integrals MUST
follow declared temporal measurement semantics and interval durations; no
default operation may silently reinterpret interval totals or instantaneous
values.
