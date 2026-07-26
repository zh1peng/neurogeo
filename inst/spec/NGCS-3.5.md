# Neuroimaging Geoinformatics Core Specification 3.5

Status: stable  
Version: 3.5  
Base specification: NGCS 3.4

NGCS 3.5 defines scientific logical identity, immutable provenance DAGs,
whitelist-only replay manifests, verified artifact manifests, and atomic
derivative batches.

Logical hashes MUST cover one ordered domain, one aligned values block,
maps, measurement semantics, labels, and file identity while excluding
incidental timestamps. Hashing MUST obey an explicit materialization budget.

Provenance edges MUST reference existing unique nodes, declare argument
roles, form an acyclic graph, and carry a canonical SHA-256 identity.
Replay MUST verify the recorded environment and every input, intermediate,
and final logical hash. Replay MUST NOT evaluate arbitrary embedded code.

Artifact paths MUST be root-relative. Size, SHA-256, role, and completeness
MUST be verified before use. A batch MUST declare `derivative_only` scope
and MUST become visible only after every artifact and manifest is complete.
Failed writers MUST leave no visible partial target batch.
