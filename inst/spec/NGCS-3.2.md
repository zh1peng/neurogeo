# Neuroimaging Geoinformatics Core Specification 3.2

Status: stable  
Version: 3.2  
Base specification: NGCS 3.1

NGCS 3.2 connects an explicitly selected transform path to sparse
support-map construction and measurement-aware resampling. Registration
estimation, implicit resampling, and automatic path selection remain outside
the specification.

An `ngcs/resampling-plan` MUST bind exact ordered source and target domain
hashes, exact path identity, a domain-compatible nearest, linear,
barycentric, or overlap method, explicit coverage/conservation/missing/
measurement/uncertainty policies, resource budget, and immutable SHA-256.

Execution MUST require explicit authorization and MUST accept only supplied,
non-lossy affine-applicable paths. It MUST preserve source element and value
order while transforming geometry, then construct a target-by-source sparse
support map. Non-affine/lossy paths, incompatible methods, incomplete
required coverage, mutation, and budget excess MUST fail explicitly.

Intensive values use support-normalized means. Extensive/count values require
conservative allocation or explicit normalization. Value and mapping
uncertainty require their declared aligned non-negative variances.

Diagnostics and results MUST jointly bind plan, path, and support-map hashes,
report sparse coverage/conservation and uncertainty, and state that
registration was not estimated and resampling was not implicit. A result
retains one target domain and one aligned values block.
