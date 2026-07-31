# Migration to neurogeo 4.7

Version 4.7 leaves every spatial object and 4.5/4.6 endpoint generator
unchanged. It adds subject-level inference after endpoint construction.

Create one `ngeo_exchangeability` for the exact final independent-unit order,
then pass it with one `ngeo_subject_features`, a separate design table, a
one-sided full-model formula, and one tested term to `ngeo_group_test()`.

Repeated sessions are not separate subjects. Reduce them to one subject row or
one paired contrast first. `sign_flip` expects one such contrast per row.
`complete_family` drops a subject if any endpoint is missing; construct the
schedule for that retained family. Missing covariates are always an error.

Automatic Fisher z is selected only from explicit endpoint metadata. Raw
values and the transform are retained. The default max-T correction and both
omnibus statistics use exactly the same schedule. Set `retain_null = TRUE`
only when the full permutation-by-endpoint matrix is required and fits the
declared resource budget.

List-of-support input is intentionally deferred to 4.8, where one common
schedule and full support-family correction can be enforced.
