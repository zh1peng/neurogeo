---
title: Installation, version pinning, and optional backends
description: Install neurogeo 6.0 on Linux, macOS, or Windows and select dependencies by task
---

# Installation, version pinning, and optional backends

## Choose the version promise first

The repository currently has **no 6.0 release tag**. The `main` branch moves
and is not a reproducible stable release. This documentation does not present
the nonexistent `v6.0.0` tag as available.

- To evaluate the current source, build and install a local checkout.
- To follow development, install `main` explicitly and record the 40-character commit SHA.
- For a publishable analysis, wait for the `v6.0.0` tag, release tarball, and matching validation evidence, or archive a reviewed commit within your study.

## Copyable commands for all three platforms

Every platform requires R 4.2.0 or later and a toolchain capable of building
R source packages.

On Linux or macOS, run from the repository root:

```sh
R CMD build .
R CMD INSTALL neurogeo_6.0.0.tar.gz
```

On Windows PowerShell, run from the repository root:

```powershell
R.exe CMD build .
R.exe CMD INSTALL neurogeo_6.0.0.tar.gz
```

If `R.exe` is not on `PATH`, use the full path to `bin\R.exe`. PowerShell can
define `R` as an alias for `Invoke-History`, which is why these commands use
`R.exe` explicitly.

The mutable development installation is for evaluation only:

```r
install.packages("remotes")
remotes::install_github("zh1peng/neurogeo@main")
```

Record what was actually installed:

```r
packageVersion("neurogeo")
utils::sessionInfo()
```

After the formal tag exists, the stable command will be
`remotes::install_github("zh1peng/neurogeo@v6.0.0")`. Do not run or cite it as
an existing release until that tag appears on the release page.

## File-format backends

“6.0 audit version” below is the compatibility-environment version, not an
implicit minimum. `DESCRIPTION` does not yet give lower bounds for these
Suggests. A locked environment and multi-platform CI must establish supported
minimums before release.

| Task | Optional dependency | 6.0 audit version | Alternative when unavailable |
|---|---|---:|---|
| NIfTI read/write and file-backed reading | `RNifti` | 1.9.0 | Build from an existing array, affine, and mask with `ngeo_volume()`; direct NIfTI I/O is unavailable |
| GIFTI reading | `gifti` | 0.9.0 | Build coordinates, faces, and values with `ngeo_surface()` |
| GIFTI writing | `freesurferformats` | 1.0.1 | Keep the `ngeo_surface`, or export ordinary matrices/data frames; direct GIFTI writing is unavailable |
| CIFTI reading | `cifti` | 0.5.0 | Build brain models, values, and optional surfaces with `ngeo_grayordinates()` |
| File-backed CIFTI metadata | `xml2` | 1.5.1 | Use a complete read or extract metadata externally; file-backed CIFTI is unavailable |
| CIFTI dscalar/dlabel/dtseries writing | built-in pure-R writer | 6.0.0 | No extra format backend; the input must still pass the explicit CIFTI contract |
| FreeSurfer surface/annot/curv/MGH/MGZ I/O | `freesurferformats` | 1.0.1 | Use the corresponding native constructor; direct FreeSurfer I/O is unavailable |
| BIDS JSON and support bundle/manifests | `jsonlite` (Import) | 2.0.0 | Included by the core installation; without it JSON exchange is unavailable |

These readers and writers do not require FreeSurfer, FSL, or Connectome
Workbench executables.

## Method backends

| Task | Dependency | 6.0 audit version | Behavior or alternative when unavailable |
|---|---|---:|---|
| Scalable kNN, distance bands, support matching; experimental surface-spin matching | `dbscan` | 1.2.4 | Small inputs can use a non-scalable base path; surface spin is unavailable |
| `igraph` conversion | `igraph` | 2.3.0 | Keep `ngeo_spatial_weights` or its sparse-matrix representation |
| `spdep` conversion; experimental spatial ordination | `spdep` | 1.4.2 | Keep built-in weights; ordination is unavailable |
| Large spatial-basis eigensolver | `RSpectra` | not installed in this audit environment | Small problems use the built-in dense fallback; reduce the problem or install the backend above its safety threshold |
| `sf` interoperability | `sf` | 1.1.0 | Use native base geometry and data frames |
| Experimental coregionalization | `gstat` + `sf` | 2.1.6 + 1.1.0 | No automatic substitute; the method is unavailable |
| Experimental MGWR | `GWmodel` + `sf` | 2.4.1 + 1.1.0 | Use stable single-bandwidth GWR or explicitly install the experimental backend |
| Experimental spatial ordination | `ade4` + `adespatial` + `spdep` | not installed in this audit environment | No automatic substitute; the method is unavailable |

`spatialreg`, `permuco`, and `permute` remain in Suggests but are not required
by a current 6.0 public runtime path. Their presence does not mean users must
install them; Phase 2 will converge these declarations.

## Reading a backend failure

A missing optional backend raises `ngeo_error_backend`; neurogeo does not
silently switch algorithms mid-computation. Its `field` identifies the package
and its `hint` gives an installation command. If your environment cannot add
dependencies, use the native constructor or built-in representation in the
tables above rather than treating results from a different backend as the same
algorithm.

Return to [installation and first run](/en/guide/) or continue with the
[15-minute quickstart](/en/tutorials/getting-started).
