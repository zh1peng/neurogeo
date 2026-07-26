# neurogeo 1.0 API freeze

Status: frozen for 1.0

## Stable object contract

Every `ngeo` dataset has the top-level fields:

```text
domain
values
maps
measures
labels
provenance
```

The stable concrete dataset classes are `ngeo_surface`, `ngeo_volume`,
`ngeo_points`, `ngeo_grayordinates`, and `ngeo_regions`. Independent stable
objects are `ngeo_space`, `ngeo_transform`, `ngeo_weights`, and
`ngeo_partition`.

Constructors, readers, accessors, validation, topology, bounded distance,
weights, partition/aggregation, and foundational statistics exported in
`NAMESPACE` are public 1.x APIs. Undocumented functions beginning with `.`
are internal and may change within 1.x.

## Compatibility promises

- Existing valid 1.0 constructor calls remain valid throughout 1.x.
- Top-level dataset fields and five domain names remain stable throughout
  1.x.
- Stable element IDs, source index bases, domain hashes, measurement
  semantics, and provenance are not silently reinterpreted.
- New optional metadata columns or list fields may be added in minor
  releases.
- New methods may be added without changing existing defaults.
- A change that alters scientific support, topology, metric meaning, or
  default aggregation requires a major specification or package version.

## Deprecation policy

After 1.0, a deprecated public API remains functional for at least two minor
package releases. It must:

1. emit a classed deprecation warning;
2. name the replacement and migration path;
3. appear in `NEWS.md`;
4. retain tests during the deprecation window.

Immediate removal is reserved for security failures or behavior that cannot
be retained without silently producing scientifically invalid results.

## Change process

Major API changes require an issue, an ADR, an engineering review, and a
scientific-semantics review. Feature proposals must identify their domain,
support, topology, metric, measurement, transform, and provenance effects.
