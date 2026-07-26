# Final release audit: neurogeo 2.6 through 2.9

Audit date: 2026-07-26  
Objective: complete every staged release from NGCS 2.6 through NGCS 2.9
without weakening the one-domain/one-values-block architecture or adding an
external neuroimaging runtime dependency.

## Release evidence

| Version | Release run | Archive SHA-256 | Check |
|---|---|---|---|
| 2.6.0 | `neurogeo-2.6.0-20260726T073550Z` | `7b2db74cd923bae2e04f89ea8dabddd1aa6a03a76bcddc8bedbe887aebdf499b` | Status: OK |
| 2.7.0 | `neurogeo-2.7.0-20260726T080517Z` | `f39ab628b027f29436a99ce5f54d750f0fca3b0a455476b83f6687f241d6d888` | Status: OK |
| 2.8.0 | `neurogeo-2.8.0-20260726T082540Z` | `e3d0caf05c9ab6f4875117d742b48c7da650a315d68214f5204f27dcc8180db5` | Status: OK |
| 2.9.0 | `neurogeo-2.9.0-20260726T090018Z` | `bd19bfd4910aa2a575accf1d4900a939319e32e2e6c7cfa6f690db297575e254` | Status: OK |

For every row, the archive exists, the recomputed SHA-256 equals the release
manifest, and the archived `00check.log` contains an exact `Status: OK`.

## Stage completion

### 2.6 bounded execution

Completed classed resource budgets, deterministic plans, resumable
identity-bound checkpoints, content-addressed caches, atomic output, true
blockwise support operations, and delayed-native streaming. The release
passed 798 assertions and 33 performance assertions. Its million-source
gate completed without logical-operator materialization.

### 2.7 uncertainty-aware modelling

Completed variogram, kriging, GWR, SAR/SEM, Gaussian CAR, and cross-support
ensemble uncertainty. The release passed 826 assertions and 33 performance
assertions, direct analytic references, Monte Carlo calibration, known-effect
coverage, and deterministic worker-count tests.

### 2.8 explicit spaces and transform graph

Completed normative space identities, registries and aliases, directed
supplied-transform graphs, deterministic path selection, mismatch/cycle
diagnostics, immutable hashes, provenance, and authorized affine
application. The release passed 853 assertions and 33 performance
assertions, with zero direct-reference affine error.

### 2.9 interoperability and 3.0 readiness

Completed validated pure-R CIFTI metadata/axis/datatype round-trips,
canonical and atomic BIDS derivative transactions, checksummed chunked
support-map schema 2 with schema-1 migration, a self-verifying
language-independent NGCS 1.0-2.9 corpus, and platform/API inventories.
The release passed 892 assertions and 33 performance assertions. Its final
manifest snapshots 16 validation reports. All 166 exports are stable and
retained for the 3.0 boundary; no 2.x API is deprecated.

## Architectural audit

The four releases retain:

- one spatial domain and one strictly aligned values block;
- explicit space, topology, metric, support, measurement semantics,
  transform, and provenance;
- sparse or blockwise support operators with no implicit dense whole-domain
  fallback;
- caller-supplied transforms only, with no automatic registration or
  resampling;
- pure-R format backends without FreeSurfer, FSL, or Connectome Workbench as
  runtime dependencies;
- BIDS derivative I/O without dataset orchestration;
- JSON and Matrix Market exchange without a custom neuroimaging binary
  format.

## Residual boundary

Windows has completed local release evidence. Linux and macOS are explicitly
listed as configured remote CI whose results remain external evidence; the
compatibility matrix does not misrepresent configuration as execution.

## Audit conclusion

The 2.6-2.9 objective is complete. Each stage meets its documented exit
criteria, each final archive is reproducible from its manifest evidence, and
neurogeo 2.9.0 is the audited endpoint for planning a separate 3.0 goal.
