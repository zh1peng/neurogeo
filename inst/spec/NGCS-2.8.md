# Neuroimaging Geoinformatics Core Specification 2.8 addendum

NGCS 2.8 requires exact hashed coordinate-space identities, aliases bound to
one hash, and directed graphs containing only caller-supplied transforms.
Path search is deterministic, rejects ambiguity without explicit selection,
and may invert only eligible non-lossy supported affine edges. Cycles and
space-field mismatches are diagnosed. Path provenance binds every ordered
edge and hash. Application requires explicit authorization and never performs
registration or resampling.
