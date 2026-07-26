# neurogeo 2.1-2.5 final release audit

Audit date: 2026-07-26  
Final package: neurogeo 2.5.0  
Final specification: NGCS 2.5  
Status: complete

| Version | Release run | Archive SHA-256 |
|---|---|---|
| 2.1.0 | `neurogeo-2.1.0-20260726T051248Z` | `3fc85ce0ca5c5147bc50cae09b2c96fc2b80bf109e051154ab4d413c26ebbc8b` |
| 2.2.0 | `neurogeo-2.2.0-20260726T054011Z` | `2f1dbe6a4e977516fe0ab457aae3d10d6541ba0d6faafbaa7247b9913da5331b` |
| 2.3.0 | `neurogeo-2.3.0-20260726T060232Z` | `d75ad898c8a5a951cbab347b06dd12aff50c11dd45d3f2c61791669bfe403326` |
| 2.4.0 | `neurogeo-2.4.0-20260726T062644Z` | `f0e9fc09894de2f45b011dfd12a30871be8eab096ccdac96e85545fe89704c4d` |
| 2.5.0 | `neurogeo-2.5.0-20260726T064814Z` | `b613103ea62dcc20b42af0cf57d10744dd01af129b04f3917c4e3e4aae46081a` |

## Final verification

- 748 unit, conformance, golden I/O, integration, property, inference, model,
  and scalable-I/O assertions passed; 0 failed and 0 warned.
- 33 full release-performance assertions passed.
- Pure-R CIFTI dscalar, dlabel, and dtseries write/read round-trips passed.
- The one-million-source gate completed in 3.83 seconds with zero numerical
  error, a 16 MB sparse operator, and no external binary dependency.
- All 11 inherited/version-specific validation reports were regenerated for
  the final source state.
- The pkgdown reference and all articles built successfully. The only sitrep
  is the intentionally unset public site URL.
- The final manifest contains 15 hashed artifacts.
- Final local `R CMD check --as-cran` reported `Status: OK`; CRAN incoming and
  system-clock network checks were disabled locally as recorded in the
  manifest.

## Scope audit

The final implementation retains one spatial domain and one aligned values
block, explicit space/topology/metric/measurement semantics, and auditable
provenance. It does not add preprocessing, implicit registration/resampling,
external neuroimaging runtime binaries, a custom neuroimaging binary format,
unbounded whole-brain dense matrices, or BIDS dataset orchestration.
