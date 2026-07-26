# Migrating from neurogeo 3.1 to 3.2

Version 3.2 is additive. Existing transform graph, support-map, and
change-of-support APIs are unchanged.

Construct and explicitly select a supplied transform path before creating a
resampling plan. Plan construction is inert; map construction and execution
require `authorize = TRUE`.

Declare geometric coverage, wholly missing support, extensive conservation,
unknown measurement semantics, uncertainty, and resource budgets
independently. Non-affine/lossy paths remain auditable but are not executable
by the 3.2 bridge. Neurogeo does not call registration software.
