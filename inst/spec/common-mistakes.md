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
11. Do not assume a file-backed object makes every downstream method bounded;
    the method must consume deterministic chunks instead of calling
    `as.matrix()` on the complete block.
12. Do not disable source verification and then reuse file-backed cache or
    checkpoint identities as if mutation were detectable.
13. Do not use the complete-source pass-through writer to encode a partial
    selection; use the format writer after intentional bounded materialization.
14. Do not create a resampling plan before selecting one exact supplied
    transform path; the plan never estimates registration or resolves
    ambiguity.
15. Do not treat geometric coverage normalization as authorization to
    normalize extensive measurements.
16. Do not execute a lossy or non-affine path through the 3.2 bridge.
