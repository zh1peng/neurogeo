# Migration to neurogeo 4.4

No object or analysis migration is required from 4.3.1.

Ordinary cortical layouts retain separate panel legends. Set
`shared_scale = TRUE` only when panels represent comparable values or share a
categorical color contract.

`ngeo_qc()` is additive. Existing calls to `ngeo_validate()` must remain:
validation answers whether an object satisfies structural invariants; QC
reports valid conditions that require scientific review.
