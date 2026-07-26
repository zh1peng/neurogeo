# NGCS 2.1 support-builder addendum

NGCS 2.1 retains NGCS 2.0 and standardizes sparse support-map construction
from already-known surface registrations, voxel affines, hard labels, and
probabilistic memberships.

Normative requirements:

1. Operators are sparse `target x source`, finite, non-negative, ordered,
   domain-bound, typed, and coverage-aware.
2. Builders do not estimate registration, transpose/reorder silently, cross
   declared structures, or turn partial coverage into complete coverage
   without an explicit policy.
3. Surface nearest maps are crisp; barycentric columns are non-negative and
   sum to one when mapped.
4. Affine voxel mapping preserves declared source index bases. Exact overlap
   may reject rotations and shear rather than approximate silently.
5. Hard labels align one-to-one with source elements. Probability input is
   source-by-region and becomes the normative target-by-source operator.
6. Sparse diagnostics report coverage, conservation error, entropy,
   sparsity, and uncertainty without densification.
7. Common-source permutations reuse one source permutation across atlases
   and state whether spatial autocorrelation is preserved.
8. Matrix Market plus JSON exchange preserves orientation, identities,
   ordered IDs, support, uncertainty, provenance, and integrity hash.

The complete normative text is maintained in `design/NGCS-2.1.md` in the
source repository.
