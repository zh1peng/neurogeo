---
title: Neuroimaging user glossary
description: User definitions of base, layer, measure, metric, support, and null in neurogeo 6.0
---

# Neuroimaging user glossary

These are current user terms. Historical names belong only in [6.0 migration](/api/articles/migration-6.0.html).

| Term | Meaning in neurogeo | Common mistake |
|---|---|---|
| spatial base | ordered spatial elements to which value rows are aligned plus geometry and space identity | not any arbitrary “analysis region” |
| element | one ordered vertex, voxel, grayordinate, parcel, or point | not necessarily a voxel |
| values | the numeric block strictly aligned to base rows | does not contain geometry |
| layer | one values column with a stable `layer_id` | display names need not be unique |
| measure | a deduplicated definition of unit, value type, support behavior, missing policy, and aggregation | not an informal column note |
| coordinate space | coordinate reference, kind, and unit; unknown means `unknown` | missing metadata does not mean MNI or mm |
| topology | structural connection such as surface faces | not the same as numeric spatial weights |
| distance method | distance chosen for one analysis | chord distance is not surface geodesic distance |
| spatial weights | sparse neighborhood/weight operator bound to one base | not an ensemble probability |
| support | spatial extent or weighting represented by a measurement | more than a coordinate point |
| support map | explicit target-by-source sparse aggregation or resampling operator | does not perform registration or segmentation |
| sampling unit | independently sampled or exchangeable unit in the study design | an atlas or voxel is not automatically an independent subject |
| null model | exact exchange, rotation, or model assumption generating a comparison distribution | “random” is not a complete definition |
| estimand | quantity estimated under a declared base, metric, support, and design | a function name alone is insufficient |
| uncertainty target | quantity actually described by an SE, interval, or simulation distribution | need not include all data and model uncertainty |

Use `ngeo_inference_contract()` to see the last six interpretation fields for a stable scientific result.

**语言：** [简体中文](/glossary/)
