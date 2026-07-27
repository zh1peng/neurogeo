---
title: neurogeo
description: R reference implementation of the Neuroimaging Geoinformatics Core Specification
---

# neurogeo

`neurogeo` is the R reference implementation of the Neuroimaging
Geoinformatics Core Specification (NGCS).

## Object contract

```text
one spatial domain
+ one aligned values block
+ explicit space, topology, metric, measurement semantics, and provenance
```

## Scope

The package provides standard-format input, spatial-domain objects, sparse
weights, spatial statistics, change-of-support operators, transform graphs,
and provenance records. It does not perform MRI preprocessing, registration,
segmentation, or surface reconstruction.

## Documentation

- [Installation and basic use](/en/guide/)
- [Analysis workflows](/en/tutorials/)
- [Module index](/en/modules/)
- [Function reference](/api/reference/)
