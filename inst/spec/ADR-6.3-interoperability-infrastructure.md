# ADR: 6.3 interoperability infrastructure and package boundary

Status: accepted for implementation, 2026-08-20.

## Context

The NGCS 6.0 five-field dataset already provides an ordered spatial Base and
aligned measurement layers. Future downstream packages need stable ways to
consume one spatial field and bind empirical pairwise information without
depending on the normalized dataset internals or expanding neurogeo into a
domain-specific simulation framework.

## Decision

1. Retain the NGCS 6.0 `base / values / layers / measures / history` container
   and the five existing Base types.
2. Add `ngeo_relation` as an independent optional object. It binds empirical
   pairwise data to an ordered Base through `base_hash()` and the portable Base
   signature; it is never inserted into an `ngeo` top-level field.
3. Keep distance, adjacency, spatial weights, transforms, and support mappings
   as analysis objects rather than Relation data.
4. Add `ngeo_layer_view()` as the stable one-field extraction boundary while
   retaining normalized dataset storage.
5. Expose the existing manifest-schema-2 portable Base identity as
   `base_signature()` and `ngeo_base_signature()`. Keep `base_hash()` as the R
   implementation hash.
6. Define neurogeo as spatial representation plus spatial analysis. Dynamics,
   perturbation, simulation, calibration, prediction, and domain-specific
   computational-neuroscience models remain downstream responsibilities.
7. Approve the 6.3 lifecycle baseline of 256 exports, 102 S3 methods, 125
   public classes, and eight generics.

## Consequences

Existing `ngeo` objects and callers remain compatible. Downstream packages can
consume stable Base, Layer view, and Relation contracts without reading the
normalized `ngeo` list structure. Cross-language engines can use the portable
SHA-256 signature while R code retains the faster implementation hash. The
dependency direction remains downstream package to neurogeo.
