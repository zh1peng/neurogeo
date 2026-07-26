# Reference fixture licenses and scope

The files in this directory are upstream format fixtures, not clinical data.
They are retained byte-for-byte and pinned by source commit, byte size, and
SHA-256 in `manifest.csv`.

- `nifti/example.nii.gz` comes from RNifti 1.9.0, distributed under
  GPL-2.0-only.
- `cifti/curvature.32k_fs_LR.dscalar.nii` comes from cifti 0.5.0,
  distributed under GPL-2.0-only. Its embedded metadata identifies an HCP
  fsLR group-average curvature workflow; neurogeo treats it only as an
  upstream interoperability fixture and makes no participant-level claim.
- The GIFTI and FreeSurfer files come from freesurferformats 1.0.1,
  distributed under the MIT license with copyright held by Tim Schäfer.

The canonical CRAN records are:

- <https://CRAN.R-project.org/package=RNifti>
- <https://CRAN.R-project.org/package=cifti>
- <https://CRAN.R-project.org/package=freesurferformats>

The exact immutable file URLs are recorded in `manifest.csv`. Redistribution
of neurogeo does not change the upstream license of these fixture files.
