# neurogeo 4.6 coupling estimands

Version 4.6.0 adds one stable entry point, `ngeo_layer_coupling()`. It returns
the existing `ngeo_subject_features` endpoint matrix and does not add a cohort
or coupling container.

## Shared contract

All numeric maps are continuous and intensive. Element-wise missing values
are not accepted; an absent unit-layer map produces missing endpoints for
that unit. Layers are aligned only through a validated `ngeo_layer_index`.
Every endpoint records centering, support weighting, standardization,
direction, component, band, energy floor, basis/operator/support identities,
weights normalization, bounds, units, and null target.

## Same-location coupling

For support mass `A`, maps are centered by their support-weighted means and

`r = x' A y / sqrt((x' A x) (y' A y))`.

The endpoint is dimensionless and bounded by `[-1, 1]`. Constant maps are an
error. It is a baseline spatial co-location summary, not an autocorrelation
correction and not population inference.

## Band structure and spectral coupling

Projection uses one fixed, value-independent `ngeo_spatial_basis`. For each
complete component band it reports layer energies, raw cross-energy,
normalized cross-energy, and retained variance for both layers. Normalized
coupling is set to missing when either band energy is at or below the declared
energy floor. Bands cannot split a near-degenerate eigenspace.

Complete-band energy and cross-energy are invariant to eigenvector sign and
orthogonal rotation within a degenerate eigenspace. Rank-matched bands are not
claimed to be equal physical wavelengths.

## Directional lag and classic bivariate Moran

Area-weighted lag correlation is

`cor_A(x, W y) = x' A W y / sqrt((x' A x) ((W y)' A (W y)))`.

Classic bivariate Moran is kept as a separate compatibility estimand. Both
maps are sample-standardized and

`I = (n / S0) (x' W y) / (x' x)`.

Both estimands retain `x_to_y` and `y_to_x`; they are never averaged. The
declared `ngeo_weights` normalization is used. Isolates are rejected in the
stable 4.6 contract.

## Reference-map spatial null

The optional null path requires one reference-map unit, an explicit
randomized stack, an explicit fixed stack, a shared transformation group, and
declared preserved properties. A tested pair must cross the randomized and
fixed stacks; jointly transforming both members is rejected. Spin mappings
from an existing `ngeo_null` are reused for stacks. A single-layer simulated
`ngeo_null` may be reused when only one layer is randomized.

Null output records the randomized and fixed stacks, preserved properties,
algorithm, group hash, simulations, and `population_inference = FALSE`.
These p-values concern a spatial-map null and cannot be interpreted as
subject/population inference.

## Promotion decisions

Cotangent Laplace--Beltrami and `ngeo_spatial_features()` remain unexported.
Their optional 4.6 promotion gates are not prerequisites for the graph-basis
coupling workflow.
