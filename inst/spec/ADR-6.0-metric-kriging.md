# ADR: metric eligibility and kriging covariance safety

Status: accepted for 6.x safety remediation, 2026-08-09.

- Anatomical surface coordinates are metric-eligible. Registration,
  visualization, and chart coordinates are not.
- Surface and grayordinate non-contiguity weights default to edge-geodesic
  distance and cannot cross disconnected components silently.
- Edge-geodesic edges and inverse-distance neighbours must have finite positive
  length; duplicate/zero-length geometry is rejected.
- Stable kriging accepts spherical, exponential, and Gaussian variogram kernels
  only with Euclidean-like metrics (`euclidean`, `world_euclidean`, and
  `region_centroid`). Arbitrary graph shortest-path covariance is rejected.
- Each local covariance must be PSD within relative tolerance `1e-10` and have
  condition number at most `1e12`. Automatic jitter is disabled (upper bound
  zero) so the fitted model is not changed silently.
- Prediction variance below `-1e-10 * max(1, local scale)` is an error. A smaller
  negative value attributable to floating-point roundoff is clamped to zero and
  retained in covariance diagnostics.
