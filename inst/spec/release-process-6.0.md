# Immutable 6.0 prerelease process

Status: prepared; no signed tag, GitHub Release, or DOI exists yet.

1. Complete the formative study, required validation suites, full performance,
   cross-platform CI, documentation build, and a clean-candidate attestation
   bound to one commit and source tar SHA-256.
2. Obtain independent approval for every science/API change. Enable protected
   `main` with required review and checks in repository settings; source files
   cannot assert that external setting.
3. Record the clean source-tar size and run
   `Rscript tools/check-release-readiness-60.R --require-release` after the
   following external objects exist.
4. Create a signed `v6.0.0-rc1` tag at the attested commit. Upload the exact
   source tarball plus SHA-256 and platform hashes to a GitHub prerelease.
5. Deposit the identical tarball in Zenodo Sandbox and verify metadata and
   checksum. After acceptance, publish the production record and add its DOI
   to `CITATION.cff`, `codemeta.json`, README, and release evidence.
6. Test the immutable install from a clean library and record `sessionInfo()`.

Never add a tag, signature, GitHub Release, Zenodo badge, DOI, protected-branch
claim, or independent-review claim before that external evidence exists.
