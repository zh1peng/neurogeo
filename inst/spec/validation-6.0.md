# neurogeo 6.0 validation contract

A 6.0 release candidate is complete only when all of the following evidence is
generated from the candidate source tree:

1. unit tests pass with one and multiple workers;
2. label alignment is tested for construction, subsetting, binding, change of
   support, resampling, and temporal derivation;
3. shared-measure aggregation is covered;
4. object manifests report NGCS 6.0 and current terminology;
5. installed conformance finds the API, NGCS, migration, and validation 6.0
   specifications;
6. `R CMD check` completes without package-code, documentation, example, or
   test failures;
7. large performance tests remain opt-in and record skipped status when their
   explicit environment gate is absent.

Historical 4.x and 5.0 scientific calibration reports remain evidence for the
unchanged algorithms they cover. They do not replace a fresh 6.0 unit,
contract, manifest, installed-conformance, and package-check run.

## Internal Phase 3 calibration boundary

VAL-303 executes the frozen 36-cell sampling-unit design with 5,000 attempted
replicates per supported calibration cell. Subject, site, and spatial-block
labels share the same independent-unit mathematics; map nulls are nine
separation cells and never receive fabricated group-level coverage. Exact
enumeration is used for free, within-block, and user-supplied schedules.

The first frozen run found one repeated failure pattern: free residual
permutation in the site-confounded design had type-I error 0.0684 (Wilson 95%
interval 0.0617 to 0.0757). The blocked and block-respecting user schedules
passed. The prespecified stop rule therefore restricts the stable path:
declared blocks may not be silently supplied to `free` or `sign_flip`; users
must choose within-block or a valid user schedule. The retained result status
is `passed-with-restriction`, not an unqualified pass. This is internal
simulation evidence, not external PALM execution or cohort validation.

VAL-306 executes 45 registered method-by-geometry-by-dependence cells. Point
estimates agree with gstat, GWmodel, spatialreg, or an analytic CAR precision
solve within `1e-6`; GWR spatial-block CV differs by at most
`3.89e-16` after reversing element order. Nine kriging cells each use 5,000
attempted Gaussian-process replicates: coverage estimates range from 0.9464 to
0.9550 and every Wilson interval lies within 0.93 to 0.97. The result status is
`passed-with-inferential-restriction`: SAR/SEM, GWR, and CAR base results expose
no calibrated p-value, so the frozen type-I metric is recorded as not
applicable rather than borrowed from a reference implementation.

VAL-302 remains externally blocked on the current candidate machine:
`wb_command`, `mri_surf2surf`, and `mri_vol2vol` are not installed. The
machine-readable prerequisite checker and external reference protocol are
registered, but neither is parity evidence. C03 therefore remains pending.
The stable documentation now distinguishes target-gather interpolation from
the implemented conservative surface source-scatter barycentric remap, and
CIFTI resampling remains runtime-rejected rather than implicitly assembled.
