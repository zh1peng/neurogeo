# Immutable 6.0 prerelease process

Status: prepared; no signed tag, GitHub Release, or DOI exists yet.

1. Complete the formative study, required validation suites, full performance,
   cross-platform CI, documentation build, and a clean-candidate attestation
   bound to one commit and source tar SHA-256.
2. Obtain independent approval for every science/API change. Enable protected
   `main` with required review and checks in repository settings; source files
   cannot assert that external setting.
3. Record the clean source-tar size. After the following external objects
   exist, create the ignored receipt
   `release/external-release-evidence-6.0.json` using
   `inst/spec/external-release-evidence-schema-6.0.json`, then run
   `Rscript tools/check-release-readiness-60.R --require-release`.
4. Create a signed `v6.0.0-rc1` tag at the attested commit. Upload the exact
   source tarball plus SHA-256 and platform hashes to a GitHub prerelease.
5. Deposit the identical tarball in Zenodo Sandbox and verify metadata and
   checksum. After acceptance, publish the production record and add its DOI
   to `CITATION.cff`, `codemeta.json`, README, and release evidence.
6. Test the immutable install from a clean library and record `sessionInfo()`.

The receipt must identify the signed tag and its commit, the local source
tarball and SHA-256, matching GitHub and Zenodo asset hashes, the DOI, protected
branch state, and at least one independent review URL for the same commit. The
checker verifies the local tag signature, commit, tarball, and cross-record
hashes. Humans must still inspect the external URLs and governance records.

Never add a tag, signature, GitHub Release, Zenodo badge, DOI, protected-branch
claim, or independent-review claim before that external evidence exists.
