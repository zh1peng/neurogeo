# Neuroimaging Geoinformatics Core Specification 1.0

Status: stable  
Version: 1.0  
Reference implementation: `neurogeo` 1.0 for R

## 1. Scope and conformance

The Neuroimaging Geoinformatics Core Specification (NGCS) defines
language-independent spatial semantics and invariants. It is not a disk
format, preprocessing pipeline, registration engine, viewer, or multi-assay
container.

The key words MUST, MUST NOT, SHOULD, SHOULD NOT, and MAY are normative.
An implementation conforms to NGCS 1.0 when it enforces this specification
and passes the published conformance fixtures within their declared
tolerances.

## 2. Dataset contract

An NGCS dataset contains exactly:

```text
one domain
+ one aligned values block
+ maps and measurement semantics
+ explicit space, topology, and metric eligibility
+ labels
+ auditable provenance
```

The following invariants are mandatory:

1. A dataset MUST have exactly one active spatial domain.
2. The first values dimension MUST align one-to-one and in order with domain
   elements. Implementations MUST NOT silently recycle, join, reorder,
   register, or resample values.
3. Every element MUST have a stable, unique `element_id`.
4. Source indices and their index base MUST remain distinct from
   implementation-local indices.
5. Every values map MUST have one maps record and one measurement-semantics
   record.
6. Values with different supports MUST NOT share a values block unless an
   explicit mapping has changed them to the same domain.
7. Unknown space or measurement semantics MUST remain explicitly unknown.
8. Operations that change element order, count, support, topology, or values
   interpretation MUST be recorded in provenance.

The values block is an `n_element` by `n_map` numeric, integer, logical, or
sparse matrix. It MAY be absent for geometry-only or metadata-only reads.
Arbitrary nested assays are outside NGCS 1.0.

## 3. Domain, geometry, and support

The domain is the ordered set of spatial support elements. Geometry locates
or shapes those elements; it is not itself a measurement. Support is the
spatial extent represented by an element or value.

NGCS 1.0 defines exactly five base domain types.

### 3.1 Surface

A surface domain represents vertices with triangular faces. It MUST contain
stable vertex elements, one or more named coordinate sets, faces, an active
coordinate set, coordinate roles, and a space.

Face indices MUST resolve to existing vertices. Coordinate metadata MUST
state dimension, role, units, and metric eligibility. Anatomical area and
distance MUST NOT silently use flattened, inflated, spherical, chart, or
other non-anatomical coordinates.

Vertex support MAY be barycentric area, with each triangle area divided
equally among its vertices. Implementations SHOULD diagnose degenerate or
duplicate faces, non-manifold edges, isolated vertices, and components.

### 3.2 Volume

A volume domain represents an active subset of a three-dimensional voxel
lattice. It MUST contain lattice dimensions, active voxel indices, a
voxel-to-world affine, and a space.

When both qform and sform exist, both MUST be preserved, the active transform
MUST be named, and a material conflict MUST produce a diagnostic. Voxel
support is the absolute determinant of the affine's 3 by 3 linear component.
A mask changes the domain; it is not merely an NA convention.

### 3.3 Points

A points domain contains finite coordinates, stable elements, and a space.
It MAY contain structure, uncertainty, or support metadata. It has no
implicit topology. K-nearest-neighbor and distance-band relations require
explicit construction and parameters.

### 3.4 Grayordinates

A grayordinates domain is one ordered composite domain containing named
surface and/or volume components. Global element order MUST match the source
brain-model mapping. Vertex indices, voxel IJK, structures, excluded
vertices, surface vertex counts, volume transforms, and source index bases
MUST be preserved.

Surface coordinates and faces are optional. Without matching surface
geometry, surface topology, geodesic, and area capabilities MUST be false.
Attached geometry MUST match structure, vertex count, density where known,
and index mapping.

Default topology is component-local and block diagonal. Cross-hemisphere,
surface-to-volume, and other cross-component edges MUST NOT be invented.

### 3.5 Regions

A regions domain represents parcels, ROIs, or clusters. It is either
membership-backed from one base domain or standalone with the available
support, centroid, and adjacency metadata.

A membership-backed region domain MUST retain the base-domain hash and
membership. It SHOULD refer to, rather than duplicate, full base geometry.

## 4. Element indexing

Every domain MUST expose an ordered element table with `element_id`. It
SHOULD preserve `source_index`, `source_index_base`, `structure`,
`component_id`, and inclusion state when applicable.

Importers MUST convert zero-based and one-based formats explicitly.
Subsetting or reordering MUST preserve stable IDs and update all aligned
structures. Matching row counts alone do not establish correspondence.

## 5. Space and transforms

A space description MUST include `space_id`, `kind`, and units. It MAY
include template, structure, density, resolution, and source metadata.
Equal names do not prove correspondence; applicable counts, mappings,
affines, masks, density, and structure MUST also agree.

A transform records a known mapping and MUST identify source and target
spaces, type, and direction. It SHOULD record method/software,
interpolation or resampling rule, parameters, source identity, and Jacobian
availability. NGCS 1.0 records transforms but does not estimate registration
or perform implicit resampling.

## 6. Topology and metric

Topology is direct connection. A metric is a named and parameterized
distance rule. They MUST remain distinguishable.

Standard topology comprises surface mesh edges, active-mask-limited voxel
6/18/26 connectivity, region relations, and component-local grayordinate
relations.

Standard metrics comprise Euclidean coordinates, world-space voxel
Euclidean distance, mesh-edge shortest paths, graph hops, and region
centroid distance. Mesh-edge shortest paths MUST be called
`edge_geodesic`; they MUST NOT be described as exact continuous-surface
geodesics.

Distance APIs MUST require explicit pairs, sources/targets, neighborhoods,
or another bounded request. They MUST NOT construct a full dense all-pairs
matrix by default.

## 7. Maps and measurement semantics

Each map MUST define:

- value type;
- spatial semantics: `intensive`, `extensive`, `count`, `categorical`, or
  `unknown`;
- units;
- missing-value policy;
- default aggregation rule.

Intensive values aggregate using support-weighted means. Extensive values
and counts aggregate by sum. Categorical values use an explicit mode/tie
policy or caller-supplied rule. Unknown semantics MUST NOT aggregate without
an explicit function.

Statistics requiring numeric variation MUST reject categorical maps and
zero-variance inputs. Missing-data and isolated-neighbor policies MUST be
explicit.

## 8. Partitions

A partition maps base elements to regions. It MUST contain a base-domain
hash, aligned membership, region table, explicit background/unlabeled
policy, overlap policy, and provenance.

NGCS 1.0 supports crisp, non-overlapping partitions. Probabilistic or
overlapping partitions are outside the stable 1.0 contract.

Regional aggregation MUST record the input domain, partition, rule per map,
excluded elements, missing-data handling, and output support sizes.
Extensive aggregation MUST conserve the included total.

## 9. Spatial weights

A weights object is independent from a dataset and MUST contain:

- source-domain hash;
- sparse weight matrix and, where applicable, raw weights;
- construction method and metric;
- parameters;
- symmetry and diagonal policy;
- normalization;
- isolate and component diagnostics.

Algorithms MUST reject weights whose domain hash differs from the dataset.
Weights SHOULD remain sparse throughout normal operations.

## 10. Provenance

Provenance MUST include specification version and implementation version.
Imported datasets SHOULD include source identity, optional checksum, reader
and backend, read time, relevant header/sidecar metadata, and operation log.

Derived outputs MUST record scientifically material parameters. Public
export MAY redact private paths or oversized metadata, but MUST NOT alter
domain identity or scientific operation history.

## 11. Capability model

Validity does not imply that every algorithm is available. Implementations
MUST expose capability diagnostics before computation. Stable capability
names include:

- 2D and 3D coordinates;
- surface topology;
- voxel affine;
- adjacency;
- surface area;
- voxel volume;
- geodesic;
- partition;
- labels;
- computational chart.

An algorithm MUST fail at its capability boundary with a precise diagnostic,
not later in an unrelated matrix operation.

## 12. Readers and interoperability

NGCS readers MUST preserve scientific alignment and mapping metadata.
NGCS 1.0 standard coverage includes NIfTI, GIFTI geometry/metric/label,
CIFTI dscalar/dlabel/dtseries, FreeSurfer surface/annot/curv/MGH/MGZ, and
native coordinates/faces/labels/values or array-plus-affine input.

Readers MUST NOT require FreeSurfer, FSL, or Connectome Workbench at runtime.
Interoperability conversions MAY lose NGCS metadata and MUST document that
loss. NGCS does not define a custom binary format.

## 13. Conformance suite

The normative fixture set contains:

1. a zero-indexed tetrahedral surface with expected edges and areas;
2. a masked affine volume with expected active indices, adjacency, and
   voxel support;
3. a hybrid grayordinate mapping with expected ordered components;
4. a crisp partition with expected region adjacency, support, and intensive
   and extensive aggregation.

Golden I/O and reference-statistic tests supplement, but do not replace,
these language-independent fixtures.

## 14. Versioning

The specification and reference implementation use independent semantic
versions. An object records its specification version. NGCS 1.x additions
MUST be backward compatible. A breaking semantic change requires NGCS 2.0.

Reference implementations MUST document migrations. After package 1.0,
deprecated public APIs remain available for at least two minor releases
unless removal is required for correctness or security.
