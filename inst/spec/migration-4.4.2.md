# Migration to neurogeo 4.4.2

No user-code or object migration is required from 4.4.1.

For package contributors, current installed contracts must be edited in
`inst/spec/`. Do not mirror new API or migration contracts into `design/`.
Run `tools/run-validation-suite.ps1` for the complete local validation suite
and `tools/run-full-performance.R` for the explicit large-data performance
gate.
