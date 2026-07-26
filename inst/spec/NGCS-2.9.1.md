# Neuroimaging Geoinformatics Core Specification 2.9.1 maintenance addendum

Status: stable  
Version: 2.9.1  
Base specification: NGCS 2.9

NGCS 2.9.1 changes no normative object or analysis semantics. It corrects
the shipped interoperability inventory and makes installed-package corpus
verification a release gate.

Source and installed supported-format documents MUST describe the same input,
output, external-dependency, round-trip, and BIDS boundaries. They MUST list
the pure-R CIFTI dscalar/dlabel/dtseries writer and NGCS support-map exchange
schemas 1 and 2.

An installed reference implementation MUST discover its conformance manifest
and fixtures without relying on a source checkout. Fixture schemas and
SHA-256 checksums MUST verify through the public conformance API.

Cross-platform configuration MUST include Windows, Linux, and macOS and MUST
run installed-package corpus validation. A configured job is not recorded as
completed evidence until it executes successfully.
