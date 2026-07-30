# Migration to neurogeo 4.2.1

No API, object, estimator, or result migration is required from 4.2.0.
Version 4.2.1 adds package discovery, citation, examples, API navigation, and
coverage governance only.

`ngeo_object_manifest()` now succeeds for valid `ngeo_transform` objects.
This repairs audit metadata generation and does not alter the transform
object or its application.
