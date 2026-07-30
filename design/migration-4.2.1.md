# Migration to neurogeo 4.2.1

No object, function-call, estimator, or result migration is required from
4.2.0.

Users can now discover the package website and issue tracker from
`packageDescription("neurogeo")`, obtain the installed citation with
`citation("neurogeo")`, and navigate the API using the published core,
advanced, and exchange/governance tiers.

`ngeo_object_manifest()` now succeeds for valid `ngeo_transform` objects.
This repairs audit metadata generation and does not alter the transform
object or its application.
