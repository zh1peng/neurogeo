# neurogeo API 4.1

Version 4.1 adds:

```r
ngeo_example_data(name = NULL, verify = TRUE)
```

The function discovers the bundled, controlled format fixtures and verifies
their recorded byte sizes and SHA-256 values. The returned table includes
source commits, immutable URLs, package versions, licenses, roles, local
paths, and verification status.

The package continues to implement NGCS 3.5. No object schema, indexing
contract, measurement semantics, transform rule, or scientific boundary
changes in 4.1.
