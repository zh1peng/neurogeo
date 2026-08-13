# Installed specifications

This directory is the canonical source for neurogeo contracts shipped with
the package. It contains current API, migration, format, and NGCS documents
used by installed conformance checks.

`API-6.0.md` defines the NGCS 6.x compatibility boundary. `API-6.1.md` and
`ADR-6.1-brain-gis-promotion.md` define the additive 6.1 brain-GIS analysis
surface. `API-6.2.md` and `ADR-6.2-brain-gis-hardening.md` define the 6.2
workflow, inference, lifecycle, and portable-identity amendments; the NGCS 6.0
container is unchanged. The files
`api-lifecycle-6.0.csv`, `api-contracts-6.0.json`, and
`inference-contracts-6.0.csv` retain their names as the canonical registries
for the complete 6.x series.

`design/` contains development plans, audits, risks, and frozen historical
rationale. Existing same-name historical files there may be retained for
context, but new or revised installed contracts must be authored only here.
