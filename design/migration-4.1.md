# Migration to neurogeo 4.1

Version 4.1 is backward compatible with 4.0 objects and public scientific
workflows. No object migration is required.

To use a controlled example file, replace ad hoc package-library paths with:

```r
path <- ngeo_example_data("rnifti-example")$path[[1L]]
volume <- read_ngeo_nifti(path)
```

Integrity verification is enabled by default. Set `verify = FALSE` only when
listing metadata in an environment where verification cost is intentionally
deferred.

The reference files are format fixtures, not preprocessing examples or
clinical datasets. Existing constraints against implicit registration,
resampling, preprocessing, and external neuroimaging binaries remain.
