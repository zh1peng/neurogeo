# Dependency and fixture audit

Status: completed for the local 1.0 release candidate

## Runtime

Core imports are `digest`, `graphics`, `Matrix`, `methods`, and `stats`.
`neurogeo` calls their public R APIs and vendors no dependency source.

Neuroimaging backends (`RNifti`, `gifti`, `cifti`, and
`freesurferformats`) are optional. `igraph`, `spdep`, `jsonlite`, `knitr`,
`rmarkdown`, and `testthat` are optional interoperability, documentation, or
test dependencies.

FreeSurfer, FSL, and Connectome Workbench binaries are not dependencies and
are not invoked by core readers.

## Code

- Package code is released under MIT.
- No GPL package implementation was copied.
- No FreeSurfer, Workbench, or FSL component is vendored.
- Adapters call documented backend functions.

## Fixtures

Files under `inst/extdata/conformance` and `inst/extdata/golden` are generated
specifically for this project and are covered by the package MIT license.
The golden generator is retained in `tools/generate-golden-fixtures.R`.

External workflow validation uses example files from installed backend
packages in place. Those files are not copied into `neurogeo`; the report
records package versions and checksums.
