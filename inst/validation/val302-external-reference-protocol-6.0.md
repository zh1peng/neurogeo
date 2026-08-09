# VAL-302 external reference protocol

Status: blocked until Connectome Workbench and FreeSurfer executables are
available in the validation environment. This protocol is not validation
evidence by itself.

The external run must use the frozen `VAL-302` factor grid and record:

1. exact executable paths and version output for `wb_command`,
   `mri_surf2surf`, and `mri_vol2vol`;
2. SHA-256 identities and licenses for every NIfTI, GIFTI/surface, CIFTI, mask,
   sphere/registration, and area file;
3. the full command line and output hash for every reference operation;
4. matched semantics: target-gather nearest/linear or barycentric interpolation
   must not be compared with source-scatter conservative remapping;
5. constant, linear, smooth-sinusoid, and compact-mass phantom errors under
   complete, partial-mask, and disconnected coverage;
6. attempted cells, unsupported cells, failures, and denominators without
   dropping an unfavorable result.

`tools/check-val302-external-prerequisites-60.R` produces a machine-readable
prerequisite report. It resolves `PATH`, the three `NEUROGEO_*` executable
overrides, and project-local hints frozen in
`inst/validation/val302-tool-manifest-6.0.csv`. It verifies versions, available
archive/executable hashes, and the FreeSurfer license before reporting ready.
A successful prerequisite check still means only that the execution
environment is ready. The later parity runner must consume the frozen design,
verify fixture hashes, and pass the registered error, conservation, coverage,
and candidate-miss gates before C03 can change from `pending-preregistered`.

The Windows Workbench 2.2.1 archive is pinned in the tool manifest and may be
expanded under `.tools/workbench-2.2.1`. FreeSurfer has no native Windows
distribution: use an approved WSL2 or container environment, freeze the two
executable hashes after installation, and supply a real `FS_LICENSE` obtained
from the official registration process. Do not create or share a synthetic
license file.

Current API boundary: CIFTI-to-CIFTI resampling is rejected rather than
assembled implicitly from cortical and volume components. Surface
`barycentric` is a conservative source-scatter remap and is not claimed to
match Workbench area-aware target-gather resampling.
