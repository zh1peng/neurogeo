# Migration to neurogeo 4.8

Single-support 4.7 calls are unchanged. To analyse a declared support family,
pass a named list to the existing `ngeo_group_test(features = ...)` argument.
Do not create one schedule or one group test per atlas.

Every support member must have the same ordered unit identifiers and one
explicit support hash. Endpoint generation still happens separately after
each change-of-support, topology/operator, and basis step. The list order is
the analysis order and becomes part of the family hash.

The default max-T family now contains every endpoint at every listed support.
Support stability is summarized only for exact semantic keys. Rank-matched
bands remain labeled rank-matched; unmatched scales remain separate.

Support dispersion, boundary diagnostics, and leave-one-support-out influence
are descriptive. They are not added to sampling variance and do not imply an
automatic stable/unstable or parcellation-invariant result.
