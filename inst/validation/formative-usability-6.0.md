# Phase 1 formative usability protocol

Status: protocol ready; participant evidence not yet collected.

This is one formative round with at least five real neuroimaging users. It is
not a release-success study and must not be populated by developers, Agents,
simulated participants, or internally timed script runs. The purpose is to
find failures in the 15-minute bilingual quickstart and return every observed
failure to a public or access-controlled issue tracker.

## Recruitment and consent

- Recruit at least five people who use MRI, fMRI, cortical-surface, CIFTI, or
  ROI/cohort data and did not author the quickstart.
- Record only a pseudonymous participant ID; do not store names, email
  addresses, clinical data, screen recordings, or institutional identifiers in
  this repository.
- Obtain the locally required consent for observation and record only the
  Boolean `consent_recorded` field.
- Use exactly one `round_id`, `P1-formative-01`. Do not combine pilot sessions
  or a later release-validation study with this round.

## Session

1. Start with a clean R 4.2+ library and the reviewed package commit.
2. Give the participant only the appropriate Chinese or English installation
   page and 15-minute quickstart. The observer may answer clarification
   questions, but records each one.
3. Start timing when the participant begins the quickstart. Stop after the
   local Moran result and history are interpreted, or when the participant
   abandons the task.
4. Record completion, elapsed wall-clock minutes, clarification count, and a
   short failure summary. Internal script runtime is not participant time.
5. Create an issue for every incomplete session or distinct blocking failure;
   record its stable URL in `issue_url`.

## Acceptance gate

Run:

```sh
Rscript tools/check-formative-usability-60.R --require-complete
```

The gate passes only when there is exactly one round, at least five unique real
participants, all sessions record consent, the median completion time is at
most 15 minutes, and every incomplete session links to an issue. A failed gate
is evidence to revise the tutorial, not permission to remove a participant.

Results belong in `inst/validation/formative-usability-results-6.0.csv`. The
empty checked-in file is a schema template and explicitly does not satisfy the
Phase 1 exit gate.
