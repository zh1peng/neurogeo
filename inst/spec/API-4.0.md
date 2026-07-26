# neurogeo API 4.0

Version 4.0 is a smaller science-facing API and continues to implement the
stable NGCS 3.5 scientific contracts.

The public contract retains one spatial domain, one aligned values block,
explicit spatial and measurement semantics, sparse support operations,
bounded format I/O, scientific models, and whitelist-only replay.

Implementation-only metric objects, public delayed constructors,
block-support wrappers, generic execution/cache/atomic utilities, duplicate
batched model wrappers, and schema/conformance introspection are removed.
Use controlled metric names, `ngeo_change_support()`, `ngeo_validate()`, the
base model functions, format-specific writers, and replay APIs.

Package version 4.0.0 does not imply a new scientific schema: persistent
objects continue to declare their applicable NGCS 3.x schema version.
