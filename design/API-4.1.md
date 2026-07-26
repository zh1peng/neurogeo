# neurogeo API 4.1

Version 4.1 adds one public discovery function:

```r
ngeo_example_data(name = NULL, verify = TRUE)
```

It returns the manifest and installed paths for the controlled NIfTI, GIFTI,
CIFTI, and FreeSurfer format fixtures bundled with neurogeo. Each row records
an immutable upstream commit and URL, source package version, license, byte
size, SHA-256, format role, and expected validation use. Verification is on
by default and fails with a classed `ngeo_error_io` if bytes differ.

The scientific object model and NGCS schemas do not change. Version 4.1
continues to implement NGCS 3.5 and preserves:

- one domain and one aligned values block;
- explicit space, topology, metric, measurement semantics, and provenance;
- zero-based source indices and one-based internal R indices;
- no implicit registration or resampling;
- no runtime dependency on FreeSurfer, FSL, or Connectome Workbench.

The 4.1 release gate adds format-level reference validation for NIfTI,
GIFTI, CIFTI, and FreeSurfer read/write round-trips and explicit failures.
