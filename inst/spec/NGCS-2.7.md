# Neuroimaging Geoinformatics Core Specification 2.7 addendum

NGCS 2.7 requires model uncertainty to bind the exact ordered domain and to
state every linearization, Gaussian, and independence assumption.

Variograms distinguish raw and measurement-corrected semivariance. Kriging
reports process, measurement, parameter, support, and total variance. GWR
reports local coefficient covariance and labels bandwidth ranges as
sensitivity only. SAR/SEM simulations retain deterministic seeds and
successful draw counts. CAR posterior claims require an explicit Gaussian
observation covariance and proper or intrinsic constraint. Cross-support
ensembles separate within- and between-support variance and do not claim
parcellation invariance.
