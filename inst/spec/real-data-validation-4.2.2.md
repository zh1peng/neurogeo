# Real-data validation contract for neurogeo 4.2.2

The machine-readable 4.2.2 report covers four real-data workflows: NIfTI,
GIFTI/FreeSurfer, CIFTI dscalar/dlabel/dtseries, and atlas/change of support.
External fixtures are download-only and must match their immutable byte size
and SHA-256 before use.

Required evidence includes ordered elements, spaces, measurement semantics,
round-trip values, label/time axes, sparse operators, file-backed reads,
support conservation, uncertainty, provenance, malformed-input rejection,
and explicit non-exercised scale cases.

The evidence does not establish preprocessing quality, anatomical
registration, clinical validity, population inference, universal
performance, or rights beyond the cited original data terms.
