# ADR-005: Measurement semantics govern aggregation

Status: accepted

## Decision

Every map records value type, spatial semantics, units, missing policy, and
default aggregation. Unknown semantics prohibit automatic aggregation.

## Consequences

Intensive, extensive, count, and categorical measurements cannot be treated
as interchangeable numeric columns. Conservation and weighted-mean tests
are normative.

