# ADR-004: No implicit dense all-pairs distances

Status: accepted

## Decision

Topology and weights use sparse representations. Distance APIs operate on
explicit pairs, source-target sets, radii, or K-nearest neighbours.

## Consequences

A dense all-pairs distance matrix is available only through a future,
explicit small-data request with size safeguards. It is never a default
intermediate.

