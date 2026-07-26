# Common mistakes

1. Do not treat matching space names as proof of element-wise alignment.
2. Do not use inflated, spherical, or flat surface coordinates for anatomical
   area or distance unless explicitly justified.
3. Do not aggregate cortical thickness with an unweighted mean when support
   areas differ.
4. Do not sum intensive measurements or average extensive measurements.
5. Do not guess that label `0` is background; declare background explicitly.
6. Do not attach a partition or weights object after changing the domain;
   domain hashes intentionally reject this.
7. Do not request dense all-pairs distances for whole-brain domains.
8. Do not interpret a geometry-only CIFTI cortical component as having
   surface topology until a matching surface is attached.
9. Do not expect readers to register, resample, preprocess, or repair data.
10. Do not run numeric autocorrelation statistics on categorical label maps.
11. Do not transpose a support operator: NGCS 2.0 is always target by source.
12. Do not apply an overlapping operator to extensive values without an
    explicit conservative allocation rule.
13. Do not describe atlas-to-atlas overlap transfer as an inverse
    reconstruction; declare the piecewise-constant model and uncertainty.
14. Do not claim local statistics are parcellation invariant merely because
    a global weighted mean or total is invariant.
15. Do not treat `registration = "name"` as registration estimation or
    validation; builders only consume an already-known relationship.
16. Do not pass a region-by-source probability matrix to an atlas builder;
    input is source-by-region and the stored operator is target-by-source.
17. Do not call a centre-based affine map exact voxel overlap.
18. Do not compare assignment row numbers across atlases; compare stable
    target identities.
19. Do not describe the default common-source permutation as a
    spatial-autocorrelation-preserving null.
20. Do not attach covariance after changing or reordering its source domain.
21. Do not call an alternative-operator range a confidence interval unless
    ensemble weights have a justified probability interpretation.
22. Do not request full target covariance or exact rank for an unbounded
    whole-brain operator; use diagonal variance and sparse conditioning.
23. Do not generate separate max-T null draws for each atlas; every row must
    come from one shared source-domain realization.
24. Do not describe fixed/random cross-atlas consensus as local
    parcellation invariance.
25. Do not infer a support-scale hierarchy from region names or counts;
    declare and audit its order.
26. Do not assume a block support map is bounded if an operation first
    materializes its complete logical operator.
27. Do not resume a checkpoint after changing its domain, operator, values,
    semantics, or operation parameters.
28. Do not use a cache key that omits measurement semantics or
    provenance-relevant parameters.
29. Do not publish directly to the final path when interruption could leave a
    partial artifact.
30. Do not pass source-domain covariance to a model after changing support;
    propagate and bind covariance to the exact target domain first.
31. Do not sum process, measurement, parameter, and support variance without
    stating an independence assumption.
32. Do not call a GWR bandwidth sensitivity range a confidence interval.
33. Do not call deterministic CAR smoothing a Bayesian posterior.
34. Do not interpret a within/between-support model ensemble as local
    parcellation invariance.
35. Do not treat a shared `space_id` as proof that two space descriptions are
    equivalent.
36. Do not add a transform edge that was inferred merely because a path is
    missing.
37. Do not choose between ambiguous shortest transform paths by insertion
    order; select the ordered edge tokens explicitly.
38. Do not invert a lossy or undeclared-invertible edge.
39. Do not apply a transform path without reviewing its edge hashes and
    explicitly authorizing geometry change.
40. Do not write CIFTI label maps with a floating-point datatype or attach
    NamedMap metadata to a series axis.
41. Do not construct BIDS derivative names by concatenating unordered
    entities or update data and sidecars as separate non-atomic operations.
42. Do not trust a support-map bundle whose chunk checksums or complete
    logical hash have not been verified.
43. Do not describe configured Linux/macOS CI as completed evidence when
    only the local platform was validated.
44. Do not implement a generic schema validator with weaker rules than the
    authoritative object-specific validator.
45. Do not hash R serialization when claiming a language-independent
    manifest identity.
46. Do not treat an object metadata manifest as a replacement values or
    multi-assay container.
47. Do not assume a file-backed object makes every downstream method bounded;
    the method must consume deterministic chunks instead of calling
    `as.matrix()` on the complete block.
48. Do not disable source verification and then reuse file-backed cache or
    checkpoint identities as if mutation were detectable.
49. Do not use the complete-source pass-through writer to encode a partial
    selection; use the format writer after intentional bounded materialization.
50. Do not create a resampling plan before selecting one exact supplied
    transform path; the plan never estimates registration or resolves
    ambiguity.
51. Do not treat `coverage = "normalize"` as authorization to normalize
    extensive measurements; geometric coverage and conservation are separate
    policies.
52. Do not execute a lossy or non-affine path through the 3.2 bridge merely
    because it exists in the transform graph.
