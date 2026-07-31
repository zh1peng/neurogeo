# NGCS 5.0 multilayer inference profile

NGCS 5.0 freezes an auditable profile built on the existing invariant of one
spatial domain and one aligned values block. It does not add a core container.

## Required inference chain

1. Maps share the same ordered domain or are transformed explicitly before
   binding.
2. Map metadata uniquely identifies each independent unit and layer.
3. The spatial basis is derived from declared topology, metric, and support,
   never from the values tested in the same confirmatory analysis.
4. Projection uses the declared support inner product.
5. Coupling and single-layer energy remain distinct endpoints.
6. Population inference transforms whole independent units with one frozen
   exchangeability schedule.
7. A declared support family uses the same subjects, model, tested term,
   schedule, and full-family multiplicity correction.

## Three separate regimes

- **Reference-map:** spatial transformations describe a fixed set of maps;
  `population_inference` is false.
- **Subject-level:** rows are independent subjects and Freedman--Lane or a
  declared restricted schedule supplies the population null.
- **Support-family:** subject inference is repeated over a predeclared finite
  family with one common schedule. Dispersion across supports is descriptive
  and is not automatically sampling variance.

The regimes may not exchange p-values or null distributions.

## Scale claims

Physical-scale matching requires a declared relation between eigenvalues and
physical wavelength. Rank-matched bands are comparable by retained rank only.
Unmatched bands do not support cross-support scale claims.

## Computational invariants

- no dense element-by-element matrix on full cortical domains;
- no dense subject-by-element-by-layer tensor;
- no all-permutation-by-all-endpoint null matrix by default;
- basis blocks are stored by connected component;
- projection and group inference are chunked and deterministic;
- seeds, schedules, hashes, package versions, and data checksums are recorded.

## Claims and non-claims

The stable workflow can estimate where layer coupling occurs across retained
scales, test subject-group effects, distinguish coupling from marginal energy,
and report behavior over a declared support family. It does not establish
parcellation invariance, infer atlas sampling variance, provide registration or
resampling, replace raw MRI preprocessing, or turn an exploratory spatial-map
null into population inference.
