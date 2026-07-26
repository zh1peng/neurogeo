# Neuroimaging Geoinformatics Core Specification 2.1 addendum

Status: stable  
Version: 2.1  
Base specification: NGCS 2.0  
Reference implementation: `neurogeo` 2.1 for R

## 1. Scope

NGCS 2.1 retains the complete NGCS 2.0 dataset and support-map contracts.
It standardizes how an implementation may construct and diagnose a sparse
support map from a relationship that is already known.

NGCS 2.1 is not a registration estimator, segmentation algorithm, surface
repair tool, raw MRI pipeline, or general resampling specification.

The key words MUST, MUST NOT, SHOULD, SHOULD NOT, and MAY are normative.

## 2. Builder contract

A support-map builder MUST return an NGCS 2.0 operator with orientation:

```text
target elements x source elements
```

The result MUST bind ordered source and target domain identities, remain
sparse, contain finite non-negative entries, declare its mapping type and
coverage, and record construction provenance.

A builder MUST NOT silently transpose or reorder an input, infer a
registration from coordinates or matching names, cross a declared brain
structure, replace missing target coverage with zero-valued observations,
convert partial coverage to complete without an explicit normalization
policy, or label an approximate geometric intersection as exact.

## 3. Known surface registration

Surface nearest and barycentric builders require coordinate sets that
already express one common registration. If source and target do not declare
the same known space, the caller MUST supply an identifier for the known
registration. That identifier is provenance, not evidence that registration
was estimated or validated.

Masked source elements become unmapped. Masked target vertices and triangles
MUST NOT receive contributions. Nearest mapping is crisp. Barycentric
mapping MUST use non-negative weights whose sum is one for each mapped
source element. Candidate-search acceleration, maximum projection distance,
degenerate triangles, and fallback behavior MUST be recorded.

## 4. Known affine voxel grids

Affine-grid builders express each source voxel centre in world coordinates
with the source affine and locate it with the inverse target affine. The
implementation MUST preserve each format's declared source index base.

Nearest mapping is crisp. Trilinear mapping is probabilistic and distributes
one source element among at most eight target grid centres. Contributions
removed by the target lattice or mask make coverage partial unless an
explicit normalization policy is used.

Exact voxel overlap MAY be limited to axis-aligned grids. An implementation
with that limit MUST reject rotation, shear, and axis permutation instead of
silently using a centre-based approximation. Overlap weights are fractions
of source voxel support and SHOULD conserve source volume under complete
coverage.

## 5. Atlas builders

Hard labels MUST align one-to-one with ordered source elements. Missing or
explicitly excluded labels are unmapped and produce partial coverage.

A source-element by region probability matrix MUST be transposed exactly
once into the normative target-by-source operator. Non-negative source
memberships with sums at most one are probabilistic; memberships whose sums
exceed one are overlapping. Complete probabilistic membership sums to one.

Region identity, order, source support, exclusions, and whether the target
template was constructed or supplied MUST be recorded.

## 6. Diagnostics

Diagnostics MUST be computable without materializing a dense operator and
MUST include source and target element counts, non-zero count and density,
source element and support coverage, target coverage, column-sum
conservation error, normalized membership entropy, and operator uncertainty.

For mapped source `s`, let `p_ts = A_ts / sum_t A_ts`. Membership entropy is:

```text
H_s = -sum_t p_ts log(p_ts)
```

Normalized entropy divides by `log(n_target)` and lies in `[0, 1]`.
Unmapped source elements have undefined entropy. Crisp assignments have
zero entropy.

## 7. Operator uncertainty and sensitivity

Independent entry variances MAY be sampled with a declared distribution,
truncation rule, sparsity policy, and normalization rule. A sampled operator
MUST be validated as an ordinary support map.

Comparing alternative operators requires a common ordered source domain.
Element-wise value sensitivity additionally requires a common ordered target
domain. Results MUST bind every operator identity and identify the reference
operator. Assignment sensitivity MUST compare target identities, not row
numbers alone.

## 8. Support-aware inference

An implementation MAY repeat one declared effect across multiple complete
support maps and report the coefficient family. It MUST call this
atlas-robust or support-sensitive analysis, not general local
parcellation-invariant inference.

A common-source permutation test MUST reuse the same source permutation
across every atlas in a replicate. It MUST state whether the null preserves
spatial autocorrelation. An unconstrained permutation MUST NOT be described
as a spatially constrained null. Multiple atlas-specific tests MUST expose
a multiple-testing policy.

## 9. Sparse exchange

Support maps MAY be exchanged as a Matrix Market operator plus JSON
metadata. The metadata MUST contain orientation, mapping type, coverage,
ordered IDs, domain identities, support, provenance, and an integrity hash.
Operator uncertainty MUST use a separately identified aligned sparse
matrix. This exchange is not a custom neuroimaging binary format.

## 10. Provenance

In addition to NGCS 2.0 requirements, construction provenance MUST record
the method and implementation version; registration identifier or affine
identities; selected coordinate sets and structure; mask, outside-grid,
exclusion, and normalization policies; search engine and bounded candidate
policy; tolerances; and mapped/unmapped counts.

## 11. Conformance

An NGCS 2.1 implementation MUST pass language-independent fixtures for:

1. nearest identity mapping on a registered surface;
2. a half-grid trilinear voxel allocation;
3. a probabilistic atlas with unequal source support.

It MUST also retain all NGCS 1.0 and NGCS 2.0 conformance behavior.
