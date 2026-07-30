# neurogeo 4.2.2 external validation data

The files named by `manifest.csv` are not bundled in the neurogeo source
archive. `tools/fetch-reference-422.R` downloads them to an ignored local
cache and verifies the pinned byte size and SHA-256 before validation.

The three `Conte69.*` CIFTI files originate in the CIFTI-2 test-data archive
and are distributed by ciftiTools 0.19.0. The cited Connectome Workbench beta
terms explicitly exempt use of the Conte69 atlas from the beta data
redistribution and publication restrictions. The manifest nevertheless uses
`download-only` so the release does not broaden that permission.

The two `S1200.*` GIFTI surfaces are distributed by ciftiTools under the HCP
Open Access Data Use Terms. They remain `download-only`; users are directed to
the upstream terms and acknowledgement requirements.

`brain.mgz` is the format fixture distributed by freesurferformats 1.0.1,
whose package is MIT licensed. It is also retained as `download-only`.

The upstream package license, original data terms, immutable source URL,
source commit, byte size, and SHA-256 are separate manifest fields. A package
license is not treated as proof of unrestricted rights in source data.
