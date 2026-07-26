# Neuroimaging Geoinformatics Core Specification 3.0

Status: stable  
Version: 3.0  
Base specification: NGCS 2.9.1

NGCS 3.0 standardizes machine-verifiable object schemas, structured
validation, portable metadata manifests, and API lifecycle governance. It
retains the one-domain/one-values-block architecture and every normative
NGCS 1.0-2.9.1 scientific contract.

## Schema registry

Every core object family MUST have one stable schema identifier, version,
implementation class binding, lifecycle state, and non-empty invariant set.
Schema lookup MUST be deterministic by controlled S3 inheritance. Unknown
classes MUST fail with a classed schema condition.

The 3.0 registry covers the five base domains plus space, transform, weights,
partition, support map, block support map, support covariance, support
ensemble, delayed values, space registry, transform graph, execution plan,
and resource budget.

## Structured validation

Schema validation MUST delegate to the authoritative object-specific
validator. It MUST NOT create a weaker parallel definition. A validation
report MUST contain validity, schema identity and version, object class,
checked invariants, and ordered issues. Every issue MUST expose severity,
stable code, condition class, and message.

Invalid objects MAY be returned as reports for diagnostics or raised as a
classed `ngeo_error_schema_validation` carrying that same report. Repeated
validation of an unchanged object MUST produce identical issues.

## Portable object metadata manifest

An NGCS object manifest MUST be JSON-compatible and MUST declare its manifest
schema, NGCS specification, object schema and version, controlled S3 class,
normative metadata, and canonical SHA-256. Hashing MUST use canonical JSON
bytes and MUST NOT use R serialization.

For an `ngeo` object, the manifest MUST bind the ordered domain using a
canonical normative-field SHA-256, ordered element identities, exact space,
values storage/dimensions, ordered maps, and measurement semantics. It does
not copy or create a second values block.

Writers MUST promote the JSON atomically. Readers MUST validate the canonical
hash before returning a manifest. Optional object matching MUST reject a
manifest for a different object.

## Migration and API lifecycle

Migration dispatch MUST validate the source object and record source,
target, and schema identity without silently changing scientific semantics.

The lifecycle registry MUST derive from actual namespace exports and declare
introduced version, lifecycle state, replacement, and planned action. Every
2.x export is retained and stable in 3.0. Any future removal requires an
explicit deprecation and migration path.

## Conformance

Conformance requires registry coverage, valid and adversarial schema
validation, deterministic canonical manifests, JSON round-trip, mutation
rejection, 2.x API retention, and a checksummed language-independent NGCS
3.0 corpus.
