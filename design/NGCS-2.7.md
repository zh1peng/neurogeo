# Neuroimaging Geoinformatics Core Specification 2.7 addendum

Status: stable  
Version: 2.7  
Base specification: NGCS 2.6

NGCS 2.7 standardizes uncertainty-aware spatial modelling. All NGCS
1.0-2.6 contracts remain.

Every model covariance MUST bind the exact ordered domain hash and element
identifiers consumed by the model. Covariance from another domain, ordering,
or support MUST fail before fitting or simulation.

An uncertainty-aware variogram MAY subtract declared additive measurement
error from pair semivariance. It MUST state the zero-mean, Gaussian, and
signal/error independence assumptions and MUST distinguish raw, corrected,
and truncated-at-zero semivariance. Parameter intervals based on simulation
MUST retain the seed, successful draw count, and worker count.

Kriging uncertainty MUST report process, measurement, variogram-parameter,
declared support, and total variance separately. Adding those components
requires an explicit independence assumption.

GWR coefficient covariance MUST be local and target-specific. Its output MUST
include coefficient intervals, effective sample size, and condition number.
A range over alternative bandwidths is a sensitivity range and MUST NOT be
called a confidence interval.

SAR and SEM uncertainty MAY use Gaussian response-covariance simulations.
The same pre-generated realization sequence MUST produce identical ordered
results under every supported worker count. Failed fits MUST be counted and a
declared minimum success fraction enforced.

CAR posterior uncertainty is a Bayesian Gaussian result only when the
observation covariance, graph precision, smoothing precision, and proper or
intrinsic constraint are explicitly declared. Deterministic `ngeo_car()`
smoothing alone remains non-Bayesian.

Cross-support model ensembles MUST separate within-support and
between-support variance by the law of total variance. They describe only the
declared support family and MUST NOT claim local parcellation invariance.

Conformance requires direct matrix references, analytic-versus-simulation
agreement, domain-mutation rejection, seeded worker-count equality, and
known-effect bias, RMSE, interval-coverage, and residual-autocorrelation
reports.
