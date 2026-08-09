---
title: neurogeo
description: R reference implementation of the Neuroimaging Geoinformatics Core Specification
---

<img class="ng-home-logo" src="/logo.png" alt="neurogeo logo representing neuroimaging geometry, spatial relations, and measurement">

# neurogeo

`neurogeo` is the R reference implementation of the Neuroimaging
Geoinformatics Core Specification (NGCS).

> **6.0 audit status:** public API additions are frozen while correctness,
> documentation, and release evidence are audited. Surface spin and the Moran
> eigen-sign surrogate are uncalibrated experimental methods, not stable null
> inference.

## Object contract

```text
one shared spatial base
+ one aligned values block
+ layer metadata
+ de-duplicated measure definitions
+ operation history
```

## Scope

The package provides standard-format input, spatial-base objects, sparse
spatial weights, spatial statistics, support aggregation, transform graphs,
and operation history. It does not perform MRI preprocessing, registration,
segmentation, or surface reconstruction.

## Documentation

- [Installation and basic use](/en/guide/)
- [Analysis workflows](/en/tutorials/)
- [Module index](/en/modules/)
- [Function reference](/api/reference/)
