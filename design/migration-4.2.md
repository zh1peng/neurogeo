# Migration to neurogeo 4.2

No API migration is required from 4.1.1.

Spatial statistics using `na_action = "omit"` and W-normalized weights now
re-normalize the retained raw-weight subgraph. Results with missing elements
may change; complete-data results and B/none semantics are unchanged.

Version 4.2 otherwise changes no estimator, argument default, object schema,
or result field. It adds independent reference agreement, simulation
calibration, and more explicit scientific boundaries for existing methods.
