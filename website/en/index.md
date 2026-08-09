---
title: neurogeo
description: Auditable spatial data and statistics for neuroimaging
---

<img class="ng-home-logo" src="/logo.png" alt="neurogeo logo">

# From neuroimaging files to interpretable spatial results

`neurogeo` keeps values on vertices, voxels, grayordinates, parcels, or points aligned with spatial identity, measurement semantics, support, and operation history. It begins after preprocessing, registration, or segmentation and supports spatial relations, aggregation, statistical analysis, and reproducible output.

## What data do you have?

- **NIfTI / volume:** start with [affine, mask, and voxel units](/en/tutorials/format-workflows#nifti-volume).
- **GIFTI / FreeSurfer surface:** start with [vertex order, faces, and coordinate roles](/en/tutorials/format-workflows#gifti-or-freesurfer-surface).
- **CIFTI / grayordinate:** start with [brain models, structures, and components](/en/tutorials/format-workflows#cifti-grayordinate).
- **ROI × subject/cohort:** start with [parcel base, subject layers, and group design](/en/tutorials/format-workflows#roi-cohort).

Each input reaches its workflow within two clicks. New users should first complete the [15-minute quickstart](/en/tutorials/getting-started).

## What does the package do?

- reads and writes common neuroimaging formats while checking ordered elements;
- declares topology, distance, spatial weights, and support operators;
- distinguishes intensive, extensive, count, and categorical measurements;
- runs spatial statistics, support-aware analyses, and resource-bounded models;
- records source identity, history, inference contracts, and portable manifests.

It does not perform MRI preprocessing, registration, segmentation, or surface reconstruction, and it never silently resamples incompatible spaces.

## Choose a documentation type

- [Installation and first run](/en/guide/)
- [Tutorial paths](/en/tutorials/)
- [User glossary](/en/glossary/)
- [Methods, assumptions, and limitations](/en/modules/)
- [API reference](/api/reference/)
- [简体中文](/)

> **Inference status:** surface spin and the Moran eigen-sign surrogate remain uncalibrated opt-in experimental methods. They are not stable null inference before preregistered type-I and power validation.
