# ADR-002: One domain and one values block

Status: accepted

## Decision

Each dataset has exactly one active spatial domain and zero or one
`n_element × n_map` values block. Its first dimension is strictly aligned to
the domain element order.

## Consequences

Mixed support and arbitrary multi-assay containers require separate objects
and explicit mappings. Subsetting and reordering must update domain and
values together.

