# neurogeo 4.8 common-schedule support-family inference

Version 4.8.0 extends `ngeo_group_test()` to a named list of
`ngeo_subject_features`. It adds no public function or container class.

## Declared support family

Every list member has one non-empty support name, one explicit support hash,
the same ordered independent-unit identifiers, and ordinary endpoint
metadata. Analysis order is list order and is part of the family identity.

All support endpoint columns are combined before the 4.7 engine runs. One
exchangeability schedule row therefore transforms every endpoint at every
support. The default single-step max-T family contains the complete declared
endpoint-by-support family. A user partition is hashed and remains explicit.

## Semantic alignment

Support stability is computed only for exact semantic keys containing
estimand, layer pair, direction, component, scale type, band definition, and
audited transform. Rank-matched bands align by declared band/mode membership,
not by claiming a common physical wavelength. Physical bands retain numerical
boundaries. Unmatched scales remain support-specific and are not pooled.

## Sampling and support summaries

Per-support coefficients, raw p-values, full-family max-T p-values, and the
common omnibus are sampling evidence. Across aligned supports, descriptive
summaries include direction agreement, median, range, IQR, standard deviation,
maximum deviation, significance persistence, and leave-one-support-out
influence.

Support dispersion is not added to sampling variance. Atlases are not assumed
to be random draws. No stable/unstable label is created automatically. Claims
are limited to the named, hashed support family and never become an automatic
"parcellation-invariant" claim.

Existing boundary/support diagnostics may be carried forward descriptively,
but they do not become new subject-level p-values unless recomputed by the
common subject schedule.
