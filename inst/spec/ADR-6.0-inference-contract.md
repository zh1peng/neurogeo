# ADR: stable scientific inference contract

Status: accepted for implementation, 2026-08-09.

Every stable scientific result class listed in
`inference-contracts-6.0.csv` has one explicit interpretation contract with:

1. estimand;
2. sampling unit;
3. null model;
4. metric;
5. support;
6. uncertainty target.

`ngeo_inference_contract()` is the canonical interface. A descriptive result
must say that a null model or uncertainty target is not applicable rather than
inventing inferential meaning. Runtime fields such as a selected null model or
distance method override only the matching declarative default. Identity hashes
are carried separately and never substituted for scientific semantics.

The contract itself is a registered NGCS manifest object. Its `print()`,
`summary()`, and portable manifest use the same six stored strings. Experimental
result classes are excluded until their promotion evidence and contract are
accepted. Adding or promoting a scientific result requires updating this ADR,
the registry, examples, and the registry gate in the same change.

`ADR-6.1-brain-gis-promotion.md` is the accepted additive amendment for seven
stable brain-GIS scientific result classes. Their normative interpretation
rows are stored in the same canonical 6.x registry,
`inference-contracts-6.0.csv`.
