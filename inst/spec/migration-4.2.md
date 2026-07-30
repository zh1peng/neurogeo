# Migration to neurogeo 4.2

No API migration is required from 4.1.1. Existing signatures, defaults, result
fields, and object schemas are unchanged.

Spatial statistics using `na_action = "omit"` and W-normalized weights now
re-normalize the retained raw-weight subgraph. Missing-element results may
change; complete-data results are unchanged.
