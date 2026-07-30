# Scientific validation contract for neurogeo 4.2

The release gate compares matched estimands with `spdep`, `spatialreg`,
`gstat`, and `GWmodel`, and records reference versions, seeds, and numerical
tolerances.

It covers Moran's I, Geary's C, local Moran Ii, SLX, SAR, SEM, spherical
variograms, ordinary kriging, and Gaussian GWR. Seeded simulations gate Moran
type-I error, SAR/SEM parameter bias and RMSE, kriging bias and nominal
coverage, and GWR coefficient bias and RMSE.

Edge cases include weight normalization, isolates, missing-value omission
with normalization reconstruction, disconnected graphs, and seeded
permutation reproducibility.

The evidence does not establish clinical validity, unknown registration,
universal asymptotics, Bayesian CAR inference, unmatched parameterizations,
large-domain exact SAR/SEM likelihoods, or group-level/repeated-measures
inference. NGCS 3.5 resource, semantics, space, and provenance constraints
remain normative.
