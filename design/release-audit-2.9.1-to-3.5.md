# neurogeo 2.9.1-3.5 final release audit

Status: complete  
Audit date: 2026-07-26  
Scope: locally archived Windows release evidence

| Version | Release run | Source archive SHA-256 | Check |
|---|---|---|---|
| 2.9.1 | `neurogeo-2.9.1-20260726T100846Z` | `0708cfab755fe8158b243e7ed6cb5f2bc09bf386b9dc5cadc60bc398c5e67bae` | Status: OK |
| 3.0.0 | `neurogeo-3.0.0-20260726T103011Z` | `6d5c489a2888f8c0c41b36cd14d1f15acd7c39f0a60a3c24a46b3efb06e37c8b` | Status: OK |
| 3.1.0 | `neurogeo-3.1.0-20260726T111357Z` | `eaf200808b96ac5cf56fb56464c4a9147cfc74b69617cc937a4fe57c786c49a5` | Status: OK |
| 3.2.0 | `neurogeo-3.2.0-20260726T115004Z` | `8dd8d321fe4625626d7e50f93d3eb54286ef5f76be9964224b009db75f272e80` | Status: OK |
| 3.3.0 | `neurogeo-3.3.0-20260726T122600Z` | `93cc64e03386fe5886d21a8c95379cb91e3df3b020e34eb5a1f9aeb2ce9ef46b` | Status: OK |
| 3.4.0 | `neurogeo-3.4.0-20260726T125400Z` | `65bfb906854b13fcfec99167c67b7d6c212167f49b17fb5388e859f43ceec2c6` | Status: OK |
| 3.5.0 | `neurogeo-3.5.0-20260726T132123Z` | `6d46590e53a1075310d9b472a7da09b2294b8b12fcf7db4cb861004ec2a00eaa` | Status: OK |

## Audit method

For each release run, the audit re-read `manifest.json`, recomputed the
source archive SHA-256 from the archived bytes, and inspected the associated
`check/*/00check.log`. All seven recomputed hashes matched the recorded
manifest values and all seven checks ended with `Status: OK`.

The 3.5 release additionally contains all 23 required validation reports,
session information, check evidence, and per-artifact MD5/SHA-256 values in
its release manifest. Local checks disabled CRAN incoming network and system
clock checks; no Linux or macOS result is inferred from this Windows audit.

## Final state

The roadmap objective is satisfied through NGCS/neurogeo 3.5. The core
architecture remains one spatial domain plus one aligned values block with
explicit space, topology, metric, measurement semantics, and auditable
provenance. No external neuroimaging binary became a runtime dependency.
