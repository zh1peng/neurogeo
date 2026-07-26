# NGCS 2.2 uncertainty addendum

NGCS 2.2 adds domain-bound diagonal, matrix, and low-rank covariance;
common-domain operator/registration/segmentation ensembles; sparse
conditioning; analytic and Monte Carlo support uncertainty; and
between-/within-operator sensitivity decomposition.

Covariance and ensembles MUST bind ordered domain identities. Normalized
intensive propagation uses the support-normalized Jacobian. Full covariance
and exact rank computations MUST have resource guards. Ensemble ranges MUST
NOT be called confidence intervals without a declared probability model.
