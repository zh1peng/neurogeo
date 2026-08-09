# Security policy

## Supported versions

The latest published 6.x release will receive security fixes. Until a signed
6.0 tag exists, the repository is an audit candidate and no moving branch is
presented as a supported release.

## Reporting a vulnerability

Do not open a public issue for a vulnerability that could expose data, execute
untrusted code, overwrite files, or compromise the build supply chain. Use the
repository's private GitHub Security Advisory reporting channel. If that
channel is unavailable, contact the package maintainer at the address in
`DESCRIPTION` and include the affected commit, platform, minimal reproducer,
impact, and any known mitigation. Do not include participant data or private
paths.

The maintainer should acknowledge a report within seven days, maintain a
private remediation branch, and publish severity, affected versions, fix
commit, and upgrade guidance when disclosure is safe.
