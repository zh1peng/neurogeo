---
title: Tutorial paths
description: Executable tutorials organized by learning stage and neuroimaging input
---

# Tutorial paths

The main tutorials share one neuroimaging case study: DK68 cortical thickness
for 100 HC and 100 SCZ participants, plus vertex-level sample data for five
participants per group. Spatial relations, support, I/O, inference, modelling,
execution, and replay return their results to brain plots. Tiny point grids are
kept for API conformance tests rather than the main teaching narrative.

## First use

1. [Installation and first run](/en/guide/)
2. [15-minute quickstart](/en/tutorials/getting-started)
3. [Choose NIfTI, surface, CIFTI, or ROI/cohort](/en/tutorials/format-workflows)

- [NIfTI volume: read → QC → analysis → plot → round-trip](/en/tutorials/workflow-volume)
- [GIFTI surface: geometry → topology → metric](/en/tutorials/workflow-surface)
- [CIFTI: brain models → components → analysis](/en/tutorials/workflow-cifti)
- [DK68 ROI × cohort: parcels are the base, subjects are layers](/en/tutorials/workflow-roi-cohort)

## Understand the object

- [Base, values, layer, and measure](/en/tutorials/core-concepts)
- [User glossary](/en/glossary/)
- [Quality control](/en/modules/quality-control)

## Build spatial relations

- [Neighbors, distances, and weights](/en/tutorials/neighbors-and-weights)
- [Parcellation and aggregation](/en/tutorials/parcellation-and-aggregation)
- [Change of support](/en/tutorials/change-of-support)
- [Transform-aware resampling](/en/modules/transform-aware-resampling)

## Analyse and interpret

- [Support uncertainty](/en/modules/support-uncertainty)
- [Support-aware inference](/en/modules/support-aware-inference)
- [Multilayer inference in 200 subjects](/en/modules/multilayer-inference)
- [Spatial modelling](/en/tutorials/spatial-modelling)
- [Spatiotemporal analysis](/en/modules/spatiotemporal-analysis)

## Execution and reproducibility

- [Bounded execution on vertex case-control data](/en/modules/bounded-execution)
- [Scalable I/O](/en/modules/scalable-io)
- [Reproducible DK cortical replay](/en/modules/reproducible-replay)

Every stable scientific tutorial should state the estimand, sampling unit, null, metric, support, and uncertainty target. Experimental methods are labeled and separated from stable paths.
