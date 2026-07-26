# Neuroimaging Geoinformatics Core Specification 2.3 addendum

Status: stable  
Version: 2.3  
Base specification: NGCS 2.2

## Scope

NGCS 2.3 standardizes inference over declared support-map families. It adds
common-source null simulations, cross-atlas consensus, multiscale inference,
boundary sensitivity tests, and family-wise adjustment. All NGCS 1.0-2.2
contracts remain in force.

## Common-support nulls

Every replicate MUST create one source-domain null realization and reuse it
across every support map in the tested family. The result MUST record the
ordered source-domain identity, every support-map identity, simulation count,
seed, tested statistic, adjustment, and null family.

A null MUST state whether it preserves spatial autocorrelation. Unconstrained
permutation MUST be identified as non-spatial. Moran spectral and surface-spin
nulls MAY claim spatial preservation only under their declared graph or
registration-coordinate contracts.

## Multiple testing

BH, BY, and Holm adjustment operate on a declared p-value family. Single-step
max-T adjustment MUST use the row-wise maximum of one common simulation
matrix. Two-sided max-T uses absolute observed and simulated statistics.
Implementations MUST reject simulation matrices that do not align with the
observed family.

## Cross-atlas consensus

Fixed-effects consensus uses inverse-variance weights. Random-effects
consensus uses a declared heterogeneity estimator; the reference
implementation uses DerSimonian-Laird. Results MUST report atlas estimates,
standard errors, normalized weights, confidence interval, Q, heterogeneity
degrees of freedom, I-squared, tau-squared, and leave-one-atlas-out estimates.

Consensus describes the supplied atlas family. It MUST NOT be described as
parcellation-invariant or as evidence of local agreement.

## Multiscale inference

Multiscale inference operates on a caller-declared ordered hierarchy of
complete support maps. Scale labels MUST be unique and aligned with the map
family. Results MUST retain scale-specific estimates and adjusted p-values,
adjacent changes, and an aggregate stability summary.

No hierarchy is inferred from labels or geometry. A multiscale result is a
support-sensitivity analysis, not a scale-invariance claim.

## Boundary inference

A boundary test requires a validated segmentation/operator ensemble whose
members share identical ordered source and target domains. One source null
realization MUST be reused across ensemble members. Results MUST bind the
ensemble hash and component map hashes and report observed effect dispersion,
the simulated dispersion distribution, and assignment sensitivity.

## Effect intervals and bootstrap

Atlas-specific model intervals MUST identify the fitted target sample size.
A common-source bootstrap MUST resample paired source values once per
replicate and apply the same draw to every atlas. Singular or underpowered
target fits MUST fail with a classed condition rather than returning a
silently unstable estimate.

## Conformance

Implementations MUST demonstrate:

1. max-T output against a direct common-simulation calculation;
2. fixed-effects consensus against an independent inverse-variance result;
3. seeded common-source simulation reproducibility;
4. declared family-error calibration under a null simulation;
5. known-effect bias and interval-coverage calibration;
6. ordered-domain rejection for invalid boundary ensembles.
