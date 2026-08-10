# neurogeo 6.0 current-delivery waivers

Status: explicitly authorized by the project owner on 2026-08-10 for the
current repository delivery. The machine-readable source is
`release-scope-waivers-6.0.json`.

The owner authorized four scope decisions: independent review of the prepared
VAL-301/VAL-308 amendments may be omitted; real-participant tutorial and UX
studies may be omitted; PUB-401 through PUB-407 may be omitted; and the current
maintainer PR may merge without an independent approving review.

These decisions do not convert missing work into completed evidence. In
particular:

- C02 stays pending and the experimental null-model restrictions remain;
- VAL-308 remains partial rather than a completed frozen factorial benchmark;
- no external-user completion-time or success-rate claim is made;
- no external cohort, independent-laboratory, manuscript, or Nature
  Communications readiness claim is made;
- CI and all non-review branch protections remain mandatory for the merge.

FreeSurfer and Connectome Workbench are not covered by a waiver. They were run
as explicit VAL-302 external comparators and remain outside the neurogeo
runtime dependency graph.
