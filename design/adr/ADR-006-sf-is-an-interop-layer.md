# ADR-006: sf is an interoperability layer

Status: accepted

## Decision

Core objects do not inherit from `sf`. Conversion to `sf` is limited to
explicit two-dimensional computational charts or planar geometries.

## Consequences

Mesh faces, voxel affines, and hybrid domains retain compact native
representations. A flat chart cannot silently replace anatomical area,
topology, or cortical distance.

