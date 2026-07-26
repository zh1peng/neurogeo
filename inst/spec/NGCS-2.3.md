# Neuroimaging Geoinformatics Core Specification 2.3 addendum

Status: stable  
Version: 2.3  
Base specification: NGCS 2.2

## Scope

NGCS 2.3 standardizes inference over declared support-map families: one
common source-domain null across maps, family adjustment, fixed/random
cross-atlas consensus, declared multiscale hierarchies, and boundary
ensemble tests. All NGCS 1.0-2.2 contracts remain in force.

## Normative requirements

- One source null realization MUST be reused across every map in a tested
  family.
- Results MUST bind source-domain, support-map, seed, simulation-count,
  statistic, adjustment, and null-family identities.
- Every null MUST state whether it preserves spatial autocorrelation.
- Max-T MUST use row-wise maxima from one aligned common simulation matrix.
- Consensus MUST report inverse-variance inputs, uncertainty,
  heterogeneity, and leave-one-atlas-out influence.
- Multiscale labels MUST be unique, ordered, aligned, and caller-declared.
- Boundary tests MUST consume an ensemble with identical ordered source and
  target domains.
- Singular and underpowered fits MUST fail with classed conditions.
- Consensus, multiscale, and boundary results MUST NOT claim invariance.

## Conformance

Conformance requires independent max-T and consensus references, seeded
reproducibility, null family-error calibration, known-effect bias/coverage,
and ordered-domain mutation rejection.
