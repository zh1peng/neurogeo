# Neuroimaging Geoinformatics Core Specification 3.4

Status: stable  
Version: 3.4  
Base specification: NGCS 3.3

Large spatial model execution uses sparse or matrix-free operators with
immutable controls for tolerance, iterations, seeded approximation, workers,
exact-small boundaries, non-convergence, and resources.

CG/BiCGSTAB solutions report residual histories and termination reasons.
Hutchinson log-determinant power series report Monte Carlo standard error and
truncation bounds under a checked norm-convergent parameter range.
SAR/SEM/CAR results bind domain, weights, controls, optimization, and solver
evidence. Indexed batched GWR/kriging preserve target order and monolithic
method equivalence.
