# Neuroimaging Geoinformatics Core Specification 2.5 addendum

NGCS 2.5 requires one fixed-dimension aligned delayed values block,
deterministic chunk reads, logically hash-equivalent target-by-source sparse
blocks, pure-R CIFTI-2 dscalar/dlabel/dtseries output, and scoped BIDS
derivative sidecars. Brain-model order, indices, affine, maps, labels, time,
measurement semantics, and provenance MUST be retained. External
neuroimaging binaries are not runtime dependencies. One-million-element
conformance MUST remain sparse and avoid full dense operator materialization.
All NGCS 1.0-2.4 contracts remain in force.
