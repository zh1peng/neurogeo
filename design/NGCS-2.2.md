# Neuroimaging Geoinformatics Core Specification 2.2 addendum

Status: stable  
Version: 2.2  
Base specification: NGCS 2.1

## Scope

NGCS 2.2 standardizes domain-bound covariance, support-operator ensembles,
conditioning, and uncertainty propagation. It retains all NGCS 1.0–2.1
contracts.

## Covariance

A covariance object MUST bind one ordered domain identity and element ID
sequence. It MAY be represented as:

- non-negative diagonal variance;
- a symmetric positive-semidefinite matrix;
- a low-rank factor plus non-negative residual diagonal.

Implementations MUST reject covariance from a different ordered domain.
Large full-covariance construction or sampling MUST have an explicit
resource guard.

## Propagation

For a declared linear support transform `L` and source covariance `C`,
target covariance is:

```text
C_target = L C L'
```

Diagonal-only output MAY be computed without creating `C_target`.

For normalized intensive change of support, `L` is the Jacobian:

```text
L_ts = A_ts s_s / sum_j A_tj s_j
```

Independent operator-entry variance uses the NGCS 2.0 ratio derivative.
Its independence assumption MUST be recorded. Categorical covariance
requires a declared categorical probability model.

Monte Carlo propagation MUST record the covariance draw distribution,
operator selection rule, normalization, number of draws, and seed.

## Ensembles

An operator, registration, or segmentation ensemble contains two or more
support maps with identical ordered source and target domains. It MUST store
normalized non-negative weights, component map hashes, an ensemble hash,
kind, and provenance.

An ensemble represents declared alternatives. It MUST NOT imply independent,
calibrated, or equally plausible alternatives unless those claims are
separately supported.

## Conditioning

Sparse conditioning diagnostics MUST report source/target isolates and weak
support, norms, a bounded largest-singular-value estimate, and stable rank.
Exact numerical rank and condition number MAY be reported only under a
declared resource limit.

## Sensitivity

Ensemble sensitivity MUST separate between-operator variation from supplied
within-operator value uncertainty. Every result MUST bind the ensemble and
component hashes. Quantile intervals over alternatives are sensitivity
ranges, not confidence intervals unless a probability model is declared.

## Diagnostics

NGCS 2.2 diagnostics add entropy quantiles, effective target count,
structure-specific coverage when structure labels are supplied, total
operator variance, and conditioning. These calculations MUST remain sparse
for the full operator.

## Conformance

Implementations MUST demonstrate:

1. analytic diagonal and low-rank covariance against direct matrix results;
2. analytic normalized variance against seeded Monte Carlo simulation;
3. ensemble identity and mutation rejection;
4. bounded conditioning and 100k-source uncertain-operator diagnostics.
