# neurogeo 4.4 API contract

Version 4.4.0 adds one stable-core function, `ngeo_qc()`. NGCS 3.5 object
schemas and all scientific estimators remain unchanged.

## Cortical layout stabilization

`ngeo_cortical_layout(..., shared_scale = TRUE)`:

- requires one common continuous or categorical value type;
- requires one HCL palette for continuous panels;
- pools included finite face values into one continuous range;
- preserves one categorical color per label and rejects conflicts;
- recolors rendering derivatives only;
- draws one layout legend;
- never merges the panel domains or changes source values.

The default `shared_scale = FALSE` preserves the 4.3 layout behavior.

## Unified quality control

`ngeo_qc()` accepts one `ngeo` object, an optional source-aligned support map,
and an optional cortical chart selection. It returns:

- a `checks` table with `pass`, `info`, `warning`, `not_evaluated`, and
  `not_applicable` states;
- bounded per-map missingness, non-finite, and constant summaries;
- sparse topology component/isolate summaries when available;
- cortical chart coverage and distortion summaries when available;
- existing sparse support diagnostics when a support map is supplied.

The function calls strict validation before reporting. It does not replace
`ngeo_validate()`, repair an object, infer unknown metadata, or change an
input. Values are not materialized beyond `max_value_cells`; topology is not
constructed beyond `getOption("neurogeo.max_qc_elements")`.

`print.ngeo_qc()` and `plot.ngeo_qc()` are presentation methods for the same
report and do not add a second QC object model.
