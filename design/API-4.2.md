# neurogeo API 4.2

Version 4.2 adds no public function, class, argument, return field, model, or
NGCS schema. It is a scientific-validation release for existing 4.1 methods.

One numerical correctness change applies when spatial statistics use
`na_action = "omit"`. The retained raw-weight subgraph is now normalized
according to the declared W/B/none policy. In 4.1, a W matrix was subset after
normalization, so rows adjacent to an omitted element no longer summed to one.

The validation report is produced by:

```text
tools/run-scientific-validation-42.R
```

The report is release evidence, not a new runtime object. neurogeo continues
to implement NGCS 3.5.
