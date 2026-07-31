# Support-family inference decisions for 4.8

Status: frozen before implementation

- Change of support, target topology/operator, target basis, and endpoint
  generation happen independently before `ngeo_group_test()`.
- A named list declares the complete support family and deterministic analysis
  order. Every member has the same ordered independent-unit rows.
- One 4.7 schedule is applied after endpoint columns are concatenated. No
  support-specific seed or regenerated schedule exists.
- Full-family max-T covers every endpoint/support column unless an explicit
  family partition is supplied.
- Stability summaries require an exact semantic key. Rank matching is labeled
  rank matching; unmatched scales are not aggregated.
- Direction agreement, dispersion, significance persistence, and
  leave-one-support-out influence are descriptive properties of the declared
  family. They are not a random-effects variance decomposition.
- Existing boundary diagnostics remain descriptive metadata. Element-level
  boundary permutations are not reused as subject-level inferential p-values.
- No automatic stable/unstable or parcellation-invariant classification is
  returned.
