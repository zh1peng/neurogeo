# Neuroimaging Geoinformatics Core Specification 2.4 addendum

NGCS 2.4 requires bounded and auditable variogram fitting, local kriging,
GWR, SAR/SEM, Gaussian CAR, and cross-support model comparison. Implementations
MUST record metric, bounds, weights/domain identity, numerical method,
tolerance, uncertainty or conditioning, and provenance. Dense covariance and
log-determinant operations MUST have explicit resource guards. CAR
impropriety, isolates, and constraints MUST be explicit. Cross-support
results MUST NOT claim coefficient invariance. All NGCS 1.0-2.3 contracts
remain in force.
