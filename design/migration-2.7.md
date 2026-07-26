# Migrating from neurogeo 2.6 to 2.7

Version 2.7 is additive. Existing variogram, kriging, GWR, spatial regression,
CAR, and cross-support point-estimate functions retain their behavior.

Create measurement covariance with `ngeo_support_covariance()` on the exact
model domain. If change of support occurs first, propagate covariance to that
target support and construct or retain a target-domain covariance before
calling the 2.7 model functions.

Use uncertainty-aware wrappers only when their assumptions match the
scientific model. In particular, a bandwidth sensitivity range is not a
confidence interval; independent variance components may be summed only when
independence is justified; and `ngeo_car_uncertainty()` makes a Gaussian
posterior claim that `ngeo_car()` does not.

For SAR/SEM simulation, keep the seed and successful simulation count in
reported provenance. Seeded simulations are ordered identically for every
supported worker count.
