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
prerequisite report. A successful prerequisite check means only that the three
executables were found. The later parity runner must still consume the frozen
design, verify fixture hashes, and pass the registered error, conservation,
coverage, and candidate-miss gates before C03 can change from
`pending-preregistered`.

Current API boundary: CIFTI-to-CIFTI resampling is rejected rather than
assembled implicitly from cortical and volume components. Surface
`barycentric` is a conservative source-scatter remap and is not claimed to
match Workbench area-aware target-gather resampling.
