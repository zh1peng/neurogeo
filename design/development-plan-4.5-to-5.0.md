# neurogeo 4.5 to 5.0 execution plan

Status: 4.5 complete; 4.6 in progress

Authoritative detailed plan: `../neurogeo_4.5-5.0_development_plan_zh.md`.
Scientific-method issue: <https://github.com/zh1peng/neurogeo/issues/6>.

## Objective

Deliver one auditable vertical workflow for support-aware multiscale layer
coupling with subject-level inference:

1. exact map-column indexing by independent unit and layer;
2. a fixed, value-independent, topology-derived spatial basis;
3. support-weighted projection and scale-specific layer endpoints;
4. whole-subject covariate-adjusted permutation inference;
5. one common permutation schedule over a declared support family;
6. calibrated simulation, real-data, resource, documentation, and package
   evidence for version 5.0.

## Stages

- 4.5: layer contract, exact map binding, graph basis, and projection.
- 4.6: same-location, spectral, and directional layer coupling.
- 4.7: exchangeability, Freedman-Lane, max-T, and group inference.
- 4.8: common-schedule inference across declared supports.
- 4.9: optional experimental ordination, bounded cross-variograms, LMC, and
  MGWR feasibility. These methods do not block 5.0.
- 5.0: freeze APIs and specifications; finish calibration, real-data,
  performance, documentation, and distribution evidence.

The detailed Chinese plan controls mathematical definitions, task IDs, exit
criteria, promotion rules, resource gates, and the final Definition of Done.

## Completion evidence

### 4.5.0

- Exact layer indexing and map binding tests pass for memory, delayed, and
  verified file-backed values.
- Analytic path, cycle, grid, disconnected-component, support-weighted
  orthogonality, reconstruction, and degeneracy tests pass.
- The installed package, complete unit suite, examples, and the two new
  Chinese vignettes pass.
- The 32,400- and 91,200-element validation gates pass with 64 modes, sparse
  partial eigensystems, no dense full-domain matrix, and maximum residual
  below `3.2e-9`.
- Tarball checking passes package installation, code, documentation,
  examples, and tests. Full legacy-vignette execution still exposes four
  pre-existing tutorial examples that depend on absent files or obsolete
  calls; this remains a documented 5.0 distribution blocker rather than a
  4.5 scientific claim.

## Workflow policy

Development is committed directly to `main`. No pull request, version tag, or
GitHub Release is created. Each scientific behavior is specified and tested
before its public facade is added. Existing unrelated worktree changes are
preserved.
