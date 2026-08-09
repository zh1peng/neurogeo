---
title: "neurogeo: Semantically explicit geoinformatics for neuroimaging in R"
version: "2.0 preprint draft"
---

> **SUPERSEDED — NOT THE 6.0 MANUSCRIPT.** This historical NGCS 2.0 draft is
> retained only for provenance. It must not be cited as describing the current
> package, API, validation status, or scientific claims.

# Abstract

Neuroimaging measurements are commonly stored on cortical meshes, voxel
lattices, point sets, hybrid grayordinate mappings, and anatomical regions.
Analysis errors arise when values are separated from their spatial support,
coordinate context, topology, distance definition, aggregation semantics, or
transformation history. We present `neurogeo`, an R reference implementation
of the language-independent Neuroimaging Geoinformatics Core Specification
(NGCS) 2.0. Its central contract is one spatial domain and one strictly
aligned values block, accompanied by explicit space, topology, metric,
measurement semantics, and auditable provenance. The package reads standard
NIfTI, GIFTI, CIFTI, and FreeSurfer formats without requiring FSL,
FreeSurfer, or Connectome Workbench. Sparse topology and weights support
bounded distances, support-aware parcellation, spatial statistics, null
models, and local regression. NGCS 2.0 adds sparse crisp, probabilistic, and
overlapping support operators, conservation-aware change of support,
uncertainty propagation, cross-atlas analysis, and bounded
parcellation-invariant inference. Language-independent conformance fixtures,
golden round-trip I/O, simulation calibration, external workflows, reference
comparisons, and performance guards validate the implementation.

# Motivation

Cortical thickness at a mesh vertex, signal intensity in a voxel, a CIFTI
grayordinate, and a parcel summary may all appear as numeric rows, but those
rows do not represent interchangeable spatial units. A surface edge is not a
Euclidean-nearest-neighbor relation; an inflated coordinate is not an
anatomical metric; and a regional mean is not valid for an extensive
quantity such as surface area. Generic tables tend to leave these
distinctions implicit, while format-specific objects make cross-domain
methods difficult to express consistently.

NGCS treats domain and support semantics as prerequisites to algorithms.
The design intentionally excludes raw MRI preprocessing, automatic
registration or resampling, a new binary format, default whole-brain
all-pairs distances, tractography, connectomes, and a general multi-assay
container.

# Design

The controlled S3 implementation defines five domains: surface, volume,
points, grayordinates, and regions. Each exposes stable element identifiers
and source indexing. A dataset contains one aligned values matrix, maps,
measurement descriptions, labels, and provenance. Independent space,
transform, weights, partition, and support-map objects prevent scientific
relationships from being hidden in inheritance or oversized geometry
tables.

Capabilities distinguish validity from algorithm eligibility. A CIFTI
object without cortical surfaces remains a valid grayordinate dataset but
does not claim surface topology, area, or geodesic capability. Points remain
valid without implicit neighbors. Unknown coordinate spaces and
measurements are represented as unknown rather than guessed.

# Input and interoperability

NIfTI input uses `RNifti`, GIFTI uses `gifti`, CIFTI uses the pure-R `cifti`
reader, and FreeSurfer formats use `freesurferformats`. Readers retain
indexing, transforms, map order, label tables, sidecars, and source
provenance. CIFTI brain models preserve ordered cortical and subcortical
components and attach surfaces only after structure and vertex-count
validation. Writers round-trip NIfTI, GIFTI, and FreeSurfer formats through
R backends. Optional converters expose sparse weights to `spdep` and
`igraph`, while controlled charts provide bounded one-way `sf`
interoperability.

# Spatial operations

Mesh, voxel, grayordinate-component, and region adjacency are sparse.
Distances require explicit source-target requests and are guarded against
accidental dense allocation. Weights support contiguity, KNN, distance
bands, inverse distance, and Gaussian kernels with explicit normalization,
symmetry, isolate, and component diagnostics.

Crisp partitions bind membership to a base-domain hash. Aggregation uses
vertex area, voxel volume, or available support. Intensive measurements use
support-weighted means; extensive measurements and counts use sums;
categorical measurements use an explicit mode/tie policy; and unknown
semantics require a caller-supplied function. Outputs retain source,
partition, rule, missing-data policy, excluded-element count, and support.

NGCS 2.0 generalizes this operation with a sparse target-by-source support
operator bound to both ordered domain hashes. Crisp, probabilistic, and
overlapping mappings share one contract. Intensive maps use
support-normalized operator means. Extensive maps and counts require unit
column allocation or explicit overlap normalization, making conservation
testable. First-order uncertainty propagation covers source-value and
independent operator-weight variance while rejecting normalized uncertain
overlap without a covariance model.

Support intersections provide physical Dice/Jaccard atlas comparison.
Atlas-to-atlas values use a declared piecewise-constant reconstruction model,
return its sparse transfer operator, and propagate supplied variance. Global
support-weighted intensive means and extensive totals can be verified across
multiple parcellations with a shared source-domain bootstrap; local effects
are not automatically claimed to be invariant.

Foundational statistics consume a dataset and matching weights object.
Global Moran's I, Geary's C, and Local Moran statistics use sparse matrix
operations. Getis-Ord statistics and exact graph-lag correlograms share a
permutation and multiple-testing policy. Surface spin and bounded Moran
spectral randomization expose their geometry and topology assumptions.
OLS/SLX adapters and explicit-bandwidth kernel regression retain domain,
metric, support, missing-data, and isolate policies. Empirical variograms
enumerate bounded pairs without constructing an implicit full distance
matrix.

# Validation

NGCS 1.0 conformance fixtures specify a tetrahedral surface, masked affine
volume, hybrid grayordinate mapping, and crisp partition with expected
areas, topology, support, adjacency, and aggregation. Synthetic standard
files provide golden tests for every supported format. Global and local
autocorrelation values are compared with `spdep`.

Two external workflows use independently distributed backend-package data:
an RNifti volume with 114,555 active voxels and a CIFTI cortical map with
59,412 grayordinates plus matching 32k surfaces. Both proceed from standard
file input through sparse weights to Moran's I without external binaries.
NGCS 2.0 adds language-independent crisp, probabilistic, and overlapping
operator fixtures with expected intensive results and extensive
conservation. Simulation validation measures Moran permutation type-I error,
OLS coefficient bias, kernel exact-field recovery, and spectral
Moran/variance preservation. Synthetic 32k, 91k, 100k, and 164k workloads
verify sparse memory and runtime behavior, including a
100k-source/1k-target support operator.

# Limitations and future work

Surface distances use mesh-edge shortest paths rather than exact
continuous-surface geodesics. The package does not estimate bandwidths,
registration, or resampling; it does not implement kriging, SAR/CAR models,
a full viewer, or raw MRI preprocessing. Moran spectral nulls use a guarded
exact eigendecomposition. Cross-atlas reconstruction is limited to an
explicit piecewise-constant model, and operator uncertainty assumes
independence unless a method states otherwise.

The stable domain, support-map, weights, partition, and provenance interfaces
are intended to support later methods without changing the scientific
meaning of existing objects. A future language implementation can conform by
passing the same fixtures rather than copying the R class layout.

# Availability

The local 2.0 release candidate is MIT licensed. A public repository and
archive identifier will be added only after maintainers authorize
publication. The source release contains the specification, tests,
vignettes, synthetic fixture generator, and reproducibility records.
