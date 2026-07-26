# Neuroimaging Geoinformatics Core Specification 3.2

Status: stable  
Version: 3.2  
Base specification: NGCS 3.1

NGCS 3.2 connects an explicitly selected transform path to sparse
support-map construction and measurement-aware resampling. Registration
estimation, implicit resampling, and automatic path selection remain outside
the specification.

## Resampling plan

An `ngcs/resampling-plan` MUST bind the exact ordered source and target domain
hashes, exact transform-path identity, one domain-compatible method, named
method parameters, coverage/conservation/missing/unknown-semantics/
uncertainty policies, optional support and mapping variance, resource budget,
and immutable plan SHA-256.

The path endpoints MUST equal the source and target space hashes. Version 3.2
execution supports only supplied, non-lossy, affine-applicable paths.
Non-affine and lossy paths MUST fail explicitly. Plan construction MUST NOT
apply the path; map construction and execution MUST require a literal
`authorize = TRUE`.

## Support bridge

An authorized path changes source geometry only; it preserves ordered
elements, values, maps, topology, and measurement semantics. The bridge MUST
then delegate to the normative sparse builder:

- surfaces: nearest vertex or barycentric projection;
- volumes: nearest centre, trilinear allocation, or exact axis-aligned voxel
  overlap.

The stored operator remains target-by-source. Unsupported domain/method
combinations, incomplete coverage under an error policy, and resource
estimates beyond budget MUST raise classed conditions. No builder may infer a
missing registration.

## Policies and uncertainty

Coverage policy controls whether incomplete geometric contributions fail,
remain partial, or normalize covered contributions. Missing-support policy
separately controls wholly unmapped source elements. Intensive values MUST
use support-normalized means. Extensive/count values MUST use conservative
unit allocation unless column normalization is explicitly authorized.
Unknown semantics MUST fail unless the caller declares their interpretation.

Value uncertainty requires aligned non-negative source variance.
Value-and-mapping uncertainty additionally requires aligned non-negative
target-by-source weight variance. A plan MUST reject uncertainty inputs that
its policy did not authorize.

## Diagnostics, provenance, and output

Diagnostics MUST report path, plan, support-map, and joint hashes; sparse
size; mapped/unmapped source support; empty targets; column-sum range;
support totals; conservation; uncertainty state; and structured issues.

A result MUST contain exactly one target-domain dataset with one aligned
values block, one sparse support map, optional aligned variance, diagnostics,
and joint provenance. Provenance MUST state that registration was not
estimated and resampling was not implicit.

An optional output writer MAY atomically promote exactly one caller-defined
artifact. The writer contract does not create a new neuroimaging format or a
dataset orchestration layer.

## Conformance

Conformance requires identity and supplied-affine references for every
supported method, semantic conservation checks, uncertainty references,
authorization and adversarial path failures, plan mutation rejection,
resource refusal, atomic-output evidence, joint hash verification, and a
checksummed language-independent 3.2 corpus.
