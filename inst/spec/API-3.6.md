# neurogeo API 3.6

Version 3.6 is the compatibility transition to the smaller 4.0 public API.
The NGCS 3.5 scientific contracts remain unchanged.

Deprecated implementation APIs include metric objects, delayed/block wrapper
classes, generic execution/cache helpers, separate batched model wrappers,
and schema/tooling introspection. Use controlled metric names,
`ngeo_change_support()`, `ngeo_validate()`, the base model functions, and the
auditable replay APIs.
