---
title: Runtime options
description: Generated limits used by neurogeo 6.0
---

# Runtime options

**Language:** [简体中文](/concepts/options)

This page is generated from every `getOption("neurogeo.*")` call in the
R sources. Set an option only for the current analysis scope and restore it
afterward. These global safety ceilings are distinct from the per-operation
limits declared by `ngeo_resource_budget()`.

| Option | Default | Accepted value | Lifecycle | Consumer source |
|---|---:|---|---|---|
| `neurogeo.endpoint_chunk` | `128L` | positive integer | stable | `R/group-inference-47.R` |
| `neurogeo.kriging_psd_relative_tolerance` | `1e-10` | positive number | stable | `R/spatial-models.R` |
| `neurogeo.kriging_variance_relative_tolerance` | `1e-10` | positive number | stable | `R/spatial-models.R` |
| `neurogeo.max_atlas_comparison_pairs` | `1e+06` | positive number | stable | `R/support-atlas.R` |
| `neurogeo.max_coregionalization_layers` | `6L` | positive integer | stable | `R/coregionalization-49.R` |
| `neurogeo.max_correlogram_edges` | `5000000L` | positive integer | stable | `R/statistics-local-association.R` |
| `neurogeo.max_correlogram_lag` | `50L` | positive integer | stable | `R/statistics-local-association.R` |
| `neurogeo.max_covariance_draw_dimension` | `5000L` | positive integer | stable | `R/support-uncertainty.R` |
| `neurogeo.max_covariance_psd_check` | `2000L` | positive integer | stable | `R/support-uncertainty.R` |
| `neurogeo.max_cross_variogram_pairs` | `100000L` | positive integer | stable | `R/coregionalization-49.R` |
| `neurogeo.max_dense_basis_elements` | `512L` | positive integer | stable | `R/spatial-basis-45.R` |
| `neurogeo.max_distance_pairs` | `1e+06` | positive number | stable | `R/distance.R` |
| `neurogeo.max_exact_logdet` | `2000L` | positive integer | stable | `R/spatial-models.R` |
| `neurogeo.max_exact_mapping_pairs` | `2e+06` | positive number | stable | `R/support-builders.R` |
| `neurogeo.max_exact_neighbors` | `5000L` | positive integer | stable | `R/weights.R` |
| `neurogeo.max_exact_support_rank` | `500L` | positive integer | stable | `R/support-uncertainty.R` |
| `neurogeo.max_exact_transformations` | `100000L` | positive integer | stable | `R/exchangeability-47.R` |
| `neurogeo.max_full_covariance_targets` | `2000L` | positive integer | stable | `R/support-uncertainty.R` |
| `neurogeo.max_kernel_targets` | `2000L` | positive integer | stable | `R/models.R` |
| `neurogeo.max_kriging_condition` | `1e+12` | positive number | stable | `R/spatial-models.R` |
| `neurogeo.max_layer_pairs` | `1000L` | positive integer | stable | `R/layer-coupling-46.R` |
| `neurogeo.max_mgwr_elements` | `500L` | positive integer | stable | `R/mgwr-49.R` |
| `neurogeo.max_model_covariance_dimension` | `2000L` | positive integer | stable | `R/model-uncertainty.R` |
| `neurogeo.max_neighbor_edges` | `10000000L` | positive integer | stable | `R/weights.R` |
| `neurogeo.max_permutations` | `99999L` | positive integer | stable | `R/statistics.R` |
| `neurogeo.max_plot_edges` | `500000L` | positive integer | stable | `R/plot.R` |
| `neurogeo.max_qc_elements` | `1000000L` | positive integer | stable | `R/qc.R` |
| `neurogeo.max_sf_features` | `100000L` | positive integer | stable | `R/chart-sf.R` |
| `neurogeo.max_spatiotemporal_pairs` | `1e+06` | positive number | stable | `R/spatiotemporal.R` |
| `neurogeo.max_spectral_null_elements` | `2000L` | positive integer | stable | `R/null-models.R` |
| `neurogeo.max_support_contributions` | `1e+07` | positive number | stable | `R/support-builders.R` |
| `neurogeo.max_support_draws` | `1000L` | positive integer | stable | `R/support-diagnostics.R` |
| `neurogeo.max_temporal_pairs` | `1e+06` | positive number | stable | `R/spatiotemporal.R` |
| `neurogeo.max_variogram_pairs` | `1e+06` | positive number | stable | `R/model-uncertainty.R;R/statistics.R` |
| `neurogeo.permutation_block` | `16L` | positive integer | stable | `R/group-inference-47.R` |

Generated from 35 unique options; edit the R call site and rerun the generator.
