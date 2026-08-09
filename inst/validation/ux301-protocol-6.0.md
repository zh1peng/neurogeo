# UX-301 external neuroimaging usability protocol

Status: frozen execution packet; awaiting a stable prerelease and real
participants. Template rows must never be populated with synthetic users.

## Participants and independence

- Recruit 8--15 people who did not develop, review, or author neurogeo.
- Record experience as `novice` or `expert`; include both strata, with at least
  three participants in each stratum.
- Every participant gives recorded consent and uses the same signed prerelease
  commit. Do not pool pilot sessions or Phase 1 formative participants.
- The observer may read the standardized help rules below only after an
  explicit request. The observer may not type commands, point to a UI target,
  interpret an error, or repair participant code.

## Frozen tasks

Each participant performs all tasks in the listed order from a clean R
library and a fresh copy of the study workspace.

1. `identify-entrypoint`: identify the correct Start/Tutorial path for the
   participant's primary format and state whether the relevant method is
   stable or experimental.
2. `quickstart`: complete the bilingual 15-minute quickstart and obtain the
   expected Moran result without copying observer code.
3. `format-workflow`: complete one NIfTI, surface, CIFTI, or ROI/cohort path
   chosen before timing from the participant's primary format.
4. `layer-recovery`: diagnose an intentionally ambiguous layer name, select a
   unique `layer_id`, and explain why silent first-match behavior is unsafe.
5. `interpret-result`: identify estimand, sampling unit, null, metric, support,
   uncertainty target, and stable/experimental boundary from a result and its
   inference contract.

`completed=true` means the frozen observable endpoint was reached without
developer intervention. Every attempted task remains in the denominator.

## Standardized help rules

- `H0`: no help requested.
- `H1`: reread the task aloud verbatim.
- `H2`: point to the already-listed documentation section, without naming a
  function or code line.
- `H3`: restate the meaning of one domain term using the published glossary.

Any other intervention invalidates that participant for the confirmatory
summary but the attempted rows remain preserved and reported as protocol
deviations. Help requests, rule IDs, timing, failure summaries, and misuse
types are recorded per task.

## Frozen gates

- 8--15 independent participants; novice/expert strata both represented with
  at least three participants.
- Exactly five attempted task rows per participant.
- Overall task success rate at least 0.80 using all attempted rows.
- Median `quickstart` duration at most 15 minutes.
- Total wrong-layer selections equals zero.
- All rows bind one signed prerelease tag and one 40-character source commit.
- All deviations, help, failures, and misuse types remain in the report.

Results belong in `inst/validation/ux301-results-6.0.csv`. Run
`Rscript tools/check-ux301-60.R --require-complete` only after the signed
prerelease and all sessions exist. The non-strict command checks preparation
and reports `study_complete=false`; it is not user evidence.
