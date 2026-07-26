# neurogeo 2.9 compatibility matrix

| Platform | Minimum R | Pure-R core | External neuroimaging binary | Evidence |
|---|---:|---:|---:|---|
| Windows | 4.2.0 | yes | no | local 2.9 release validation |
| Linux | 4.2.0 | yes | no | configured CI; remote result required |
| macOS | 4.2.0 | yes | no | configured CI; remote result required |

The matrix reports evidence, not an assumption of equivalence. The local
release archive is checked on Windows. Linux and macOS become release
evidence only when their configured CI jobs have executed successfully for
the same source revision.

The core readers and writers do not require FreeSurfer, FSL, or Connectome
Workbench. Optional R packages provide file-format backends.
