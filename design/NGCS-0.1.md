# Neuroimaging Geoinformatics Core Specification 0.1

Status: draft  
Version: 0.1  
Reference implementation: `neurogeo` for R

## 1. Purpose

NGCS defines language-independent semantics and invariants for
neuroimaging geoinformatics objects. It is not a disk format. Implementations
MAY use different in-memory representations, but MUST pass the same
conformance tests.

The key words MUST, MUST NOT, SHOULD, SHOULD NOT, and MAY are normative.

## 2. Dataset model

An NGCS dataset contains:

```text
dataset
├── domain
├── values
├── maps
├── measures
├── labels
└── provenance
```

The following invariants apply:

1. A dataset MUST have exactly one active spatial domain.
2. The first dimension of `values` MUST align one-to-one and in order with
   the domain elements.
3. Every element MUST have a stable, unique `element_id`.
4. Source indices MUST be stored separately from implementation-local
   indices, including the source index base.
5. Values with different spatial supports MUST NOT be combined without an
   explicit mapping.
6. Unknown coordinate space MUST be represented as `unknown`; it MUST NOT be
   silently interpreted as MNI, fsaverage, fsLR, or another named space.
7. Operations that change element count, order, or support MUST update
   provenance.

`values` is an `n_element × n_map` numeric, integer, logical, or sparse
matrix, or is absent for a geometry-only dataset. Arbitrary multi-assay
containers are outside NGCS 0.1.

## 3. Domain types

NGCS 0.1 defines five domain types.

### 3.1 Surface

A surface domain represents vertices and triangular faces. It MUST contain:

- stable element identifiers;
- one or more named coordinate sets;
- triangular faces;
- an active coordinate set;
- coordinate metadata;
- a space description.

Face indices MUST resolve to existing vertices. A coordinate set MUST state
its dimension, role, units, metric eligibility, and source. Flattened,
inflated, and spherical coordinates MUST NOT be used as anatomical area or
cortical-distance coordinates without an explicit request.

Multiple coordinate sets MAY share one topology. Implementations SHOULD
diagnose degenerate faces, duplicate faces, isolated vertices,
non-manifold edges, and connected components.

### 3.2 Volume

A volume domain represents an active subset of a three-dimensional voxel
lattice. It MUST contain:

- lattice dimensions;
- an active voxel-to-world 4 × 4 affine;
- IJK indices for every active element;
- a space description.

When qform and sform are both available, both MUST be preserved, the active
affine MUST be identified, and a material conflict MUST produce a
diagnostic. Voxel volume is the absolute determinant of the affine's
3 × 3 linear component.

### 3.3 Points

A points domain contains coordinates, stable element identifiers, and a
space description. It MAY contain structure, uncertainty, or support-radius
metadata. Points have no implicit topology. K-nearest-neighbour and
distance-band relations require explicit construction.

### 3.4 Grayordinates

A grayordinates domain is one ordered composite domain containing named
components such as left cortical vertices, right cortical vertices, and
subcortical voxels.

The global element order MUST match the source brain-model mapping. Vertex
indices, voxel IJK, structures, excluded vertices, and volume transforms
MUST be preserved.

Cortical coordinates and faces are optional. Without attached geometry,
the dataset remains valid but surface topology, geodesic, and surface-area
capabilities MUST be false. Attached surfaces MUST match the recorded
structure, vertex count, density, and index mapping.

Default grayordinate topology is block diagonal. Cross-hemisphere and
cortical-subcortical edges MUST NOT be added implicitly.

### 3.5 Regions

A regions domain represents parcels, ROIs, or clusters. It is either:

- membership-backed, with an explicit mapping from a base domain; or
- standalone, with region identifiers and available support, centroid, or
  adjacency metadata.

Membership SHOULD use an integer vector or sparse matrix and SHOULD NOT
duplicate the complete base geometry.

## 4. Element indexing

Every domain MUST provide an element table containing `element_id`.
It SHOULD also contain `source_index`, `source_index_base`, `structure`, and
`included`, and MAY contain `component_id`.

Importers and exporters MUST convert index bases explicitly. Reordering or
subsetting MUST NOT silently replace stable element identifiers.

## 5. Space

A space description contains:

- `space_id`;
- `kind`;
- `units`;
- optional template, structure, density, and resolution;
- source metadata.

Equal `space_id` values do not prove element-wise correspondence.
Implementations MUST additionally compare the applicable vertex count,
density, affine, mask, and mapping metadata.

## 6. Topology and metric

Topology describes direct connection. Metric describes a named,
parameterised distance rule. They are distinct concepts.

NGCS 0.1 topology includes mesh edges, active-mask-limited voxel
6/18/26-connectivity, region relations, and component-local grayordinate
relations.

NGCS 0.1 metrics include Euclidean coordinate distance, world-space voxel
Euclidean distance, mesh-edge shortest-path distance, graph hops, and region
centroid distance. Mesh-edge shortest paths MUST be named `edge_geodesic`
and MUST NOT be presented as exact continuous-surface geodesics.

Distance APIs SHOULD operate on requested pairs, sources and targets,
neighbourhood radii, or K nearest neighbours. A full dense all-pairs matrix
MUST NOT be constructed by default.

## 7. Measurement semantics

Every map MUST define or explicitly leave unknown:

- value type;
- spatial semantics;
- units;
- missing-value policy;
- default aggregation.

Intensive quantities normally aggregate with support-weighted means.
Extensive quantities and counts normally aggregate by sum. Categorical
values require a declared mode/tie policy or another explicit rule.
Unknown semantics MUST require the caller to provide an aggregation
function.

## 8. Partition and weights

A partition maps base-domain elements to regions. It MUST identify its base
domain, membership, background policy, overlap policy, regions, and
provenance. NGCS 0.1 requires crisp partitions; probabilistic or overlapping
membership is deferred.

A weights object is independent from the dataset and MUST record its source
domain identity, sparse matrix, construction method, metric and parameters,
symmetry, diagonal policy, normalisation, and component diagnostics.

## 9. Transform and provenance

A transform records a known mapping; NGCS 0.1 does not estimate
registration. It SHOULD record source and target spaces, type, direction,
method/software, interpolation or resampling rule, parameters, source
identity, and Jacobian availability.

Provenance MUST include the specification and implementation versions and
SHOULD include source identifiers, configurable checksums, importer
information, read time, a header summary, and an operation log. Public
outputs SHOULD support removal of private paths and oversized metadata.

## 10. Capability model

A valid dataset can lack information required by a particular algorithm.
Implementations MUST expose capabilities before computation. Standard
capabilities include 2D/3D coordinates, surface topology, voxel affine,
adjacency, surface area, voxel volume, geodesic, partition, labels, and
computational chart.

Algorithms MUST fail at their capability boundary with a precise diagnostic,
not later inside an unrelated matrix operation.

## 11. Conformance

An implementation conforms to NGCS 0.1 when it:

1. enforces the dataset and indexing invariants;
2. represents surface, volume, points, grayordinates, and regions without
   changing their scientific support;
3. preserves source mapping metadata;
4. applies topology, metric, and aggregation semantics explicitly;
5. passes the published surface, volume, grayordinate, and partition
   fixtures within declared numerical tolerances.

