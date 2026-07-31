# Subject-level inference decisions for 4.7

Status: frozen before implementation

- One row is one independent subject, precomputed paired contrast, or other
  declared exchangeability unit. Sessions from the same subject are not
  independent rows.
- Feature endpoints are fixed before inference. Spatial vertices, regions,
  bands, and layer maps remain inside the subject record.
- A one-sided model formula defines the full design. `test` names one formula
  term or supplies one named one-df contrast.
- Freedman--Lane permutes reduced-model residual rows. Sign-flip schedules
  multiply complete residual rows. The same schedule row applies to all
  endpoints.
- One-df tests return signed t statistics. Multi-df terms return non-negative
  partial F statistics. Single-step max-T uses absolute t or F as appropriate.
- `complete_family` retains one common unit set across all endpoints; no
  pairwise endpoint deletion is confirmatory.
- Auto transformation uses Fisher z only for endpoint metadata that explicitly
  declares correlation bounds. Raw values and the chosen transform remain in
  the result.
- Permutation validity assumes exchangeable residuals or sign symmetry.
  Group-wise variance and leverage diagnostics do not repair arbitrary
  heteroscedasticity.
