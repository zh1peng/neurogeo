# Neuroimaging Geoinformatics Core Specification 2.0

Status: stable  
Version: 2.0  
Reference implementation: `neurogeo` 2.0 for R

## 1. Scope

NGCS 2.0 retains the complete NGCS 1.0 dataset contract and adds an explicit
change-of-support contract. It is language independent and is not a disk
format, registration engine, resampler, preprocessing pipeline, atlas
estimator, or inverse reconstruction method.

The key words MUST, MUST NOT, SHOULD, SHOULD NOT, and MAY are normative.
NGCS 2.0 implementations MUST satisfy NGCS 1.0 invariants and the support-map
invariants below.

## 2. Unchanged dataset contract

An NGCS dataset still contains:

```text
one spatial domain
+ one strictly aligned values block
+ explicit space
+ explicit topology
+ explicit metric eligibility
+ explicit measurement semantics
+ auditable provenance
```

The five base domains remain surface, volume, points, grayordinates, and
regions. Values on different domains or supports MUST NOT share a values
block. Element indexing, source indexing, capability claims, transforms,
labels, measurement semantics, and provenance retain their NGCS 1.0
definitions.

## 3. Support-map contract

An NGCS 2.0 support map represents one declared relationship between a source
domain and a target domain. It contains:

```text
type
direction = target_by_source
sparse non-negative operator A
source domain identity and ordered element IDs
target domain identity and ordered element IDs
coverage policy
optional source and target support sizes
optional operator-weight uncertainty
provenance
```

For `n_s` source elements and `n_t` target elements, `A` MUST have dimensions
`n_t × n_s`. Entry `A[t,s]` states the contribution of source support `s` to
target support `t`. Implementations MUST NOT silently transpose, reorder,
join, register, or resample this operator.

The source and target domain identities MUST be validated before application
or composition. Ordered element IDs MUST align exactly with operator columns
and rows. Operator entries MUST be finite and non-negative. Sparse storage is
the default; a language implementation MUST NOT require a dense
target-by-source matrix.

## 4. Mapping types

### 4.1 Crisp

A crisp column contains at most one nonzero, and every nonzero MUST equal
one. Under complete coverage every column contains exactly one unit entry.

### 4.2 Probabilistic

A probabilistic column contains non-negative membership fractions whose sum
MUST NOT exceed one. Under complete coverage each column MUST sum to one
within declared numerical tolerance.

### 4.3 Overlapping

An overlapping column MAY contribute to multiple targets and MAY sum above
one. Under complete coverage every source column MUST have positive total
membership. Overlap is not automatically a conservative allocation.

## 5. Coverage and conservation

Coverage is either `complete` or `partial`. Complete crisp and probabilistic
maps MUST have unit column sums. Complete overlapping maps MUST have positive
column sums.

For extensive values and counts, a valid conservative allocation operator
MUST have unit column sums. An overlapping operator with other column sums
MUST either:

1. be explicitly column-normalized before extensive allocation; or
2. be rejected.

Unmapped source support MUST be rejected unless the caller explicitly chooses
a drop policy. A drop policy MUST be recorded and MUST NOT claim global
conservation.

For every complete conservative change of support:

```text
sum(target extensive value) = sum(source extensive value)
```

within declared tolerance.

## 6. Measurement-aware change of support

Let `s` be positive source support sizes, `x` a source map, and `A` the
target-by-source operator.

### 6.1 Intensive

The target intensive value is:

```text
y_t = sum_s A[t,s] s_s x_s / sum_s A[t,s] s_s
```

Targets with zero denominator are undefined. Implementations MUST NOT replace
this operation with an unweighted mean unless all relevant supports are
declared equal.

### 6.2 Extensive and count

For a conservative allocation operator `C`:

```text
y = C x
```

Counts and extensive quantities use the same conservation rule. Whether a
non-integer probabilistic allocation remains a count interpretation MUST be
made explicit in measurement metadata.

### 6.3 Categorical

Categorical change of support uses an explicitly declared weighted decision
rule, such as support-weighted mode, plus a tie policy. It is not represented
as numeric averaging.

### 6.4 Unknown

Unknown measurement semantics MUST cause change of support to fail unless the
caller explicitly declares an interpretation. The interpretation and
resulting semantics MUST be recorded.

## 7. Composition

Support maps `A: S → M` and `B: M → T` may compose only when the ordered
intermediate domain identity of `A` exactly matches the source identity of
`B`.

```text
C = B A
```

The composed map MUST retain source and target identities, recompute its
mapping type and coverage, and record both input map identities. Composition
MUST NOT be used as evidence of an anatomical registration that was not
already encoded in the input maps.

## 8. Uncertainty

An implementation MAY store independent non-negative variances for operator
entries. Any uncertainty result MUST state its dependence assumptions.

For extensive linear allocation with independent source-value variance
`Var(x)`:

```text
Var(y_t) = sum_s C[t,s]^2 Var(x_s)
```

Independent operator-weight variance MAY add:

```text
sum_s Var(C[t,s]) x_s^2
```

For intensive support-normalized means, first-order propagation MUST include
the ratio derivative. With `D_t = sum_s A[t,s] s_s` and estimate `y_t`, the
operator derivative is:

```text
d y_t / d A[t,s] = s_s (x_s - y_t) / D_t
```

Categorical uncertainty MUST NOT be reported as a numeric variance without a
declared categorical probability model.

## 9. Cross-atlas operations

Two atlas maps may be compared only when they share the same ordered source
domain. Their support intersection is:

```text
O = A diag(s) B'
```

Jaccard and Dice overlap MUST use support sizes, not element counts, unless
unit support was explicitly declared.

Atlas-to-atlas value transfer is not a mathematical inverse of aggregation.
It MUST declare a reconstruction model. NGCS 2.0 defines a foundational
`piecewise_constant` model in which each source parcel is constant over its
support and target values are overlap-weighted. Implementations MUST expose
the transfer operator and SHOULD propagate supplied value uncertainty.

An implementation MUST NOT present parcel-to-element or parcel-to-parcel
reconstruction as assumption-free, unique, or exact when it is ill posed.

## 10. Parcellation-invariant inference

An inference may claim parcellation invariance only for a statistic whose
support algebra makes it invariant and only after validating complete domain
coverage and conservation.

NGCS 2.0 defines:

- the global support-weighted mean for intensive maps; and
- the global total for extensive maps and counts.

The estimate MUST agree between source support and every validated
parcellation within a declared tolerance. A shared source-domain bootstrap
MAY provide uncertainty that does not depend on one selected atlas.

Claims about local effects, regression coefficients, clusters, or parcel
contrasts are not automatically parcellation invariant.

## 11. Provenance

Support-map construction, normalization, composition, application,
uncertainty propagation, cross-atlas transfer, and invariant inference MUST
record:

- source and target domain identities;
- support-map identity;
- mapping type and coverage;
- measurement interpretation;
- allocation, unmapped, and normalization policy;
- reconstruction model, if any;
- uncertainty assumptions;
- software/specification version.

Redaction MAY hide source identifiers, but MUST state that redaction
occurred.

## 12. Resource invariants

Support operators SHOULD remain sparse through validation, composition,
overlap, allocation, and uncertainty propagation. Dense atlas-by-atlas
summaries MAY be created only when explicitly requested and bounded.
Implementations MUST retain resource guards for operations whose output can
become dense.

## 13. Conformance

An NGCS 2.0 implementation MUST pass language-independent crisp,
probabilistic, and overlapping fixtures. The fixtures test:

- orientation and dimensions;
- type and coverage invariants;
- source and target support;
- intensive support normalization;
- extensive conservation;
- explicit overlap normalization.

The reference fixture tolerance is declared in each fixture. Implementations
MAY use different object systems, names, or sparse libraries while preserving
the same semantics and results.
