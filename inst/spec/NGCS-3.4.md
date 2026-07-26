# Neuroimaging Geoinformatics Core Specification 3.4

Status: stable  
Version: 3.4  
Base specification: NGCS 3.3

NGCS 3.4 defines deterministic bounded iterative spatial model execution.
Sparse or matrix-free operators are normative for large domains. Dense
operators are permitted only below an explicit exact-small threshold.

An `ngcs/solver-control` MUST declare relative tolerance, iteration limit,
log-determinant trace order and probe count, deterministic seed, worker count,
exact-small threshold, non-convergence policy, resource budget, and immutable
identity. Every iterative solution MUST report convergence, iterations,
relative-residual history, termination reason, method, and control identity.
Breakdown and non-convergence MUST be visible and classed.

Approximate `log|I - parameter W|` MUST enforce a declared convergent norm
bound. Hutchinson power-series estimates MUST report probe count, order,
seed, workers, Monte Carlo standard error, and a conservative truncation
bound. Probe results for one seed MUST NOT depend on worker count.

Iterative SAR and SEM likelihoods MUST bind one ordered domain and sparse
weights. Their spatial parameter, likelihood optimization status,
log-determinant method/error diagnostics, coefficients, and any linear-solve
report MUST be retained. Iterative Gaussian CAR MUST declare proper or
intrinsic precision semantics and retain the complete sparse solve report.

Indexed batched GWR and kriging MUST preserve caller target order, reproduce
the corresponding monolithic local calculation, expose batch count and
identity, and enforce target/batch resource limits. Batching MUST NOT change
the statistical method.
