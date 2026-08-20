# NGCS 6.0 core data model

NGCS 6.0 defines one canonical in-memory dataset contract:

```text
ngeo
|- base
|- values
|- layers
|- measures
`- history
```

The base defines where values live. Value columns are described by ordered
layer rows, and each layer references exactly one de-duplicated measure through
`measure_id`. History records construction and subsequent operations.

## Base contract

A base contains a stable element table, type-specific geometry, one coordinate
space, optional topology, and optional aligned label resources. The five base
types are `point`, `surface`, `volume`, `parcellation`, and `grayordinate`.

`base_hash()` identifies the ordered spatial base and excludes `base$labels`.
Adding or merging label resources therefore does not make otherwise identical
spatial bases incompatible. Label content remains part of dataset-level
reproducibility manifests.

## Alignment invariants

1. `nrow(values)` equals the number of base elements.
2. `ncol(values)` equals the number of layer rows.
3. Layer IDs are non-empty and unique.
4. Every layer references one defined measure.
5. Element-aligned label values have one entry per base element.
6. Subsetting changes elements, values, geometry, and aligned labels together.
7. Cross-base operations require an explicit transform or support map.

## Analysis objects

Distance methods, spatial weights, transforms, support maps, temporal axes,
models, and inference results are analysis objects. They are not mandatory
fields of an `ngeo` dataset and must bind to the relevant base identity.

## Change of support

`aggregate_to()` is the normative change-of-support engine. Crisp partitions
are converted to sparse support maps before execution. Intensive, extensive,
count, and categorical measures follow their declared aggregation semantics;
unknown semantics require an explicit policy.

## Versioning

NGCS 6.0 is incompatible with the 5.x `domain / maps / provenance` container.
Portable manifests for current objects report schema version 6.0 and use
`base`, `layer`, and `history` terminology. The portable manifest envelope is
versioned independently from the object schema. Neurogeo 6.3 uses
`NGCS-object-manifest-2` and `NGCS-logical-object-2` so surface identity binds
the selected active coordinate set; schema-1 object manifests are not valid
cache or checkpoint identities under that contract.
