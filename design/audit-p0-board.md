# neurogeo 6.0 Phase 0 remediation board

Feature freeze baseline: 226 exports, SHA-256
`713fe6ff24b3bcd8468254e2a9a0577112c0df37d2ffc37c6b7ab7b7670a2019`.
The CI gate is `tools/check-feature-freeze-60.R`. No new export is allowed until
the Phase 0 attestation passes and required review is recorded.

| ID | Implementation owner | Required reviewer | Reproduction / gate | Status |
|---|---|---|---|---|
| GOV-000 | release maintainer | scientific maintainer | `tools/check-feature-freeze-60.R` | implemented; review pending |
| EVID-001 | release + scientific maintainer | independent reviewer | `tools/run-audit-corpus-60.R` | implemented; review pending |
| DOC-001 | documentation maintainer | release maintainer | `tools/check-doc-entrypoints-60.R` | implemented; review pending |
| TERM-001 | documentation maintainer | neuroimaging methods reviewer | `tools/check-user-terminology-60.R` | implemented; review pending |
| API-001 | R maintainer | neuroimaging methods reviewer | `test-audit-regressions-60.R` layer fixture | implemented; review pending |
| ENG-001 | R maintainer | release engineer | atomic failure-injection fixtures | implemented; cross-platform CI pending |
| SCI-FIX-001 | R maintainer | statistical methods reviewer | unequal-support weighted reference | implemented; review pending |
| SCI-FIX-002 | statistical maintainer | external validation reviewer | Haar moments, strata, collision diagnostics | implemented as experimental; calibration pending VAL-301 |
| SCI-FIX-003 | statistical maintainer | external validation reviewer | irregular-graph invariant counterexample | downgraded to experimental; calibration pending VAL-301 |
| SCI-FIX-004 | R maintainer | neuroimaging methods reviewer | role/component/zero-distance fixtures | implemented; review pending |
| SCI-FIX-005 | statistical maintainer | neuroimaging methods reviewer | metric allowlist, PSD/condition/variance gates | implemented; review pending |
| REL-001 | release engineer | R maintainer | identity negative fixtures | implemented; review pending |
| REL-002 | release engineer | R maintainer | `tools/run-p0-evidence-60.R` | pending clean-candidate run |
| PAPER-001 | paper lead | release maintainer | superseded banner in `paper/paper.md` | implemented; review pending |

Status terms are factual: “implemented” does not mean scientifically validated,
independently reviewed, or release-ready. Those states close only when their
named evidence exists.
