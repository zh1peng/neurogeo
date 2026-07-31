# Multilayer inference decisions

Status: frozen for the 4.5 implementation stage

## Domain and indexing

The core object remains one ordered spatial domain and one element-by-map
values block. A map table supplies the logical independent-unit-by-layer
index. No cohort, tensor, R6, or multi-assay container is introduced.

The stable subject-level contract requires unique `unit x layer` entries.
Replicates are never averaged implicitly. Subject covariates are supplied in a
separate design table and aligned by exact unit identifier.

## Space, topology, metric, and support

Map binding requires identical ordered domain, element, space, and topology
identity. It performs no registration, resampling, or name-based matching.

The first stable operator is the graph Laplacian built from symmetric,
non-negative raw spatial weights. Row-standardized weights are not used as a
symmetric eigensystem. Components are decomposed separately. Surface charts
and flatmaps are presentation coordinates and cannot define this operator.

Projection uses one positive support vector throughout centering, inner
products, orthogonality, coefficients, energy, and coupling. Surface, volume,
region, and grayordinate support comes from the domain. Point domains use
identity support unless a later explicit contract supplies another measure.

## Basis

For stiffness `L`, support mass `A`, and component-local modes `U`, neurogeo
solves `L u = lambda A u` through the symmetric transformed operator
`A^(-1/2) L A^(-1/2)`. Returned non-constant modes satisfy
`t(U) A U = I`. Only partial component-local eigensystems may be computed for
large domains; a dense full-domain matrix is forbidden.

The basis uses no map values, outcomes, group labels, or clinical data.
Domain, operator, support, component, numerical diagnostics, and basis hashes
are retained. Bands may not split a declared near-degenerate eigenspace.

## Measurement semantics

Confirmatory numeric coupling accepts continuous intensive maps by default.
Extensive and count maps require an explicit scientifically meaningful
conversion or endpoint. Categorical maps define partitions, boundaries, or
strata and do not enter numeric Laplacian projection.

## Inference regimes

Spatial-map nulls and subject-level nulls are distinct. Reference-map results
have `inference_unit = spatial_map` and do not claim population inference.
Subject inference moves complete subject records, preserving every spatial
map and within-subject layer relationship. Vertices are never treated as
independent subjects.

One normalized exchangeability schedule is reused over every endpoint and
every declared support. Support-family dispersion is descriptive; it is not
automatically a random-effects variance or universal atlas uncertainty.

## Resources and provenance

Large basis construction is sparse or implicit. Projection is component-,
row-, and map-chunked. Group permutations stream exceedance counts and family
extrema. Every operation checks a declared resource budget before bounded
materialization and records deterministic ordering and exact identity hashes.

## Non-goals

The 4.5-5.0 stable path does not add preprocessing, registration estimation,
vertex-wise mass-univariate GLMs, a general ordination toolbox, a dense
all-pairs cortical variogram, co-kriging, longitudinal mixed models, graph
neural networks, or a new core container.
