# ADR-001: Controlled S3 object model

Status: accepted

## Decision

The R reference implementation uses validated S3 list classes. Constructors
create objects and validators enforce invariants. Mutable R6 references and
a package-wide S4 hierarchy are not used.

## Consequences

Objects remain inspectable and serialisable. `Matrix` S4 objects may appear
as internal sparse components without changing the package-level model.

