# VAL-302 external reference protocol

Status: externally validated for the declared 6.0 API boundary. The committed
evidence was generated from source commit `c839870516a59d93a10b6145c3be572dab629fe4`
with Connectome Workbench 2.2.1 and FreeSurfer 7.4.1, then verified offline by
`tools/check-val302-external-evidence-60.R`.

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
environment is ready. `tools/run-val302-external-reference-60.R` consumes the
frozen design and writes the complete cell table, command receipts, and result
report. The committed run attempted all 108 cells: 60 declared and
semantically matched cells passed, while 48 CIFTI-to-CIFTI or incompatible
surface target-gather cells were retained as explicit unsupported results.
All 90 external command receipts completed successfully and bind their input
and output hashes.

The Windows Workbench 2.2.1 archive and the FreeSurfer 7.4.1 Linux executable
hashes are pinned in the tool manifest. The recorded FreeSurfer execution host
is `linux212`; its license was checked by hash and existence only. Do not
create, commit, or share a synthetic or real license file.

Current API boundary: CIFTI-to-CIFTI resampling is rejected rather than
assembled implicitly from cortical and volume components. Surface
`barycentric` is a conservative source-scatter remap and is not claimed to
match Workbench area-aware target-gather resampling. FreeSurfer and Workbench
are external validation comparators only: neither is an `Imports` dependency,
a stable API backend, nor a requirement for ordinary neurogeo execution.
