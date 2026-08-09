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
