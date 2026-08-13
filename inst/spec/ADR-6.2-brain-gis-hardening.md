# ADR: harden portable identity and amend 6.2 brain-GIS inference

Status: accepted for implementation, 2026-08-13.

## Context

The 6.1 stable brain-GIS surface was additive to the frozen NGCS 6.0 data
model. Subsequent 6.2 work added three public workflow entry points and revised
the null or multiplicity behavior of local coupling, finite-domain point
processes, and nonseparable hotspots. A release audit found that the installed
6.x inference registry still described the older 6.1 behavior.

The same audit found that the portable surface signature read an obsolete
field name. Two surfaces with identical coordinate arrays but different active
coordinate selections could therefore have the same manifest and logical
identity even though metric operations used different geometry.

## Decision

1. Retain the NGCS 6.0 five-field container and five spatial-base types.
2. Promote the three stable 6.2 workflow entry points and one S3 method listed
   in `API-6.2.md`; the approved lifecycle counts are 251 exports, 100 S3
   methods, 123 public classes, and eight generics.
3. Require the lifecycle generator to consume an explicitly approved registry.
   A public symbol without an approved row is `unreviewed` operationally and
   makes regeneration fail; it is never inferred to be stable.
4. Amend the three scientific interpretation contracts described in
   `API-6.2.md`. The package records callable semantics and executable software
   validation, not study or manuscript status.
5. Replace portable object manifest schema 1 with schema 2 and logical NGCS
   object payload schema 1 with schema 2. Schema 2 binds the active surface
   coordinate name. Schema-1 object manifests are rejected; old cache and
   checkpoint identities are invalid under this contract.
6. Strengthen strict validation of already-normative coordinate-space,
   measurement, active-coordinate, and history invariants.

## Consequences

This is a breaking change only for portable manifest/cache identity, not for
the NGCS in-memory container or scientific data values. Consumers must rebuild
portable manifests and logical hashes. The implementation, Rd documentation,
API lifecycle and formals snapshots, inference registry, and
validation runners must pass together before release.

The release complexity gate remains binding. Public facade growth must be
removed through reviewed internal helpers rather than hidden by increasing the
frozen ceiling.
