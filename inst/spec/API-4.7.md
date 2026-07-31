# neurogeo 4.7 subject-level permutation inference

Version 4.7.0 adds `ngeo_exchangeability()` and `ngeo_group_test()`. The
analysis unit is one complete independent-subject row in an existing
`ngeo_subject_features` matrix. Spatial elements are never permuted.

## Independent-unit contract

Unit identifiers are unique and align exactly across features, design data,
and exchangeability schedules. Repeated sessions must first be reduced to one
scientifically declared subject summary or paired contrast. A sign-flip test
expects one paired/one-sample contrast per independent row. Full PALM-style
family trees and longitudinal repeated-measures models are not implemented.

`complete_family` uses the same retained subjects for every predeclared
endpoint. Missing design covariates are an error. An exchangeability object
must be constructed for the final retained unit family.

## Exchangeability schedules

Stable schemes are free permutation, within-block permutation, sign flipping,
and a user-supplied schedule. Schedules store transformations in rows and
units in columns. The identity transformation is handled by the observed
statistic and is not stored among Monte Carlo rows. Duplicate transformations
are rejected or avoided. User schedules require exact unit column names.

Within-block schedules never move a row outside its declared block. The test
term must have identifiable within-block variation. Complex kinship and
exchangeability trees require an externally generated user schedule.

## Freedman--Lane engine

For transformed endpoint matrix `Y`, nuisance design `D`, and tested columns
`G`, the reduced model produces fitted values and residuals. Transformation
row `b` creates `Y_b = fitted_D + P_b residual_D`; the full model then yields
the same one-df t or multi-df partial F statistic. One transformation row is
used for every endpoint.

Shared QR decompositions and endpoint/permutation blocks replace endpoint-wise
`lm()` calls. Raw exceedance counts, family maxima, and omnibus nulls are
streamed. The full permutation-by-endpoint matrix is absent unless
`retain_null = TRUE` and passes the resource budget.

## Results and claims

Each endpoint reports the raw value scale, audited transform, coefficient for
one-df tests, descriptive parametric interval, test statistic, raw permutation
p-value, single-step max-T p-value, and partial R-squared. Omnibus max-absolute
statistic and sum-of-squares statistic reuse the same schedule.

Correlation-like endpoints may use an auditable Fisher-z transform. Spatial
element counts never enter its sampling standard error. Diagnostics retain
rank, residual degrees of freedom, leverage, group-wise variances, Monte
Carlo resolution, schedule hash, and the non-claim that arbitrary
heteroscedasticity is not solved.
