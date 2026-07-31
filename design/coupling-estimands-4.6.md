# Coupling estimand decisions for 4.6

Status: frozen before implementation

| Estimand | Centering | Support | Standardization | Direction | Bounds / units | Null target |
|---|---|---|---|---|---|---|
| same-location | support mean | `A` | weighted norms | symmetric pair | `[-1,1]`, unitless | spatial map or later subject |
| band energy | support mean before projection | `A`-orthonormal basis | none | layer-specific | squared map units times support | descriptive |
| spectral cross-energy | support mean before projection | fixed `A`-orthonormal basis | none | symmetric pair | product units times support | spatial map or later subject |
| spectral coupling | support mean before projection | fixed `A`-orthonormal basis | band norms | symmetric pair | `[-1,1]`, unitless | spatial map or later subject |
| directional lag | support mean | `A`, declared `W` | weighted norms after lag | retained | `[-1,1]`, unitless | spatial map or later subject |
| classic cross-Moran | arithmetic mean | declared `W` | sample z-score, `n/S0` | retained | dimensionless, not correlation-bounded | compatibility spatial statistic |

Bands are caller-declared retained-mode memberships. They cannot overlap or
split a near-degenerate eigenspace. A spectral coupling denominator at or
below `energy_floor` yields missing coupling plus a low-energy diagnostic;
energy and cross-energy remain available.

An absent map produces a missing unit endpoint. A present map containing
non-finite values or no variance fails. Pairwise element deletion is not a
stable 4.6 behavior.

Reference-map transformations and future whole-subject permutations are
different inference regimes. Reference-map transformations never establish a
population p-value. A shared transformation may preserve covariance within a
randomized stack, but each tested pair must cross from that stack to an
explicitly fixed stack.
