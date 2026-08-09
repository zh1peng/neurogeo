# neurogeo 6.0 controlled glossary

This glossary is normative for user-facing 6.x documentation. Historical
specifications may retain their original terminology when clearly labelled as
historical.

| Preferred term | Meaning | Avoid in current user text |
|---|---|---|
| spatial base | Ordered spatial elements plus geometry, space, and optional topology shared by all layers | domain when describing the 6.0 container |
| layer | One values column and its aligned metadata | map when selecting a values column |
| layer ID | Stable unique selector for a layer | assuming a display name is unique |
| measure | De-duplicated definition of what layer values mean | embedding measurement semantics only in a column name |
| spatial support | Area, volume, vertex set, parcel, or other footprint represented by a value | treating support as a generic software-support claim |
| support map | Explicit target-by-source operator that changes spatial support | implicit aggregation or resampling |
| spatial weights | Analysis-specific neighbourhood or weight matrix | spatial spatial_weights |
| coordinate space | Named reference/template in which coordinates are interpreted | coordinate set |
| coordinate set | A concrete surface coordinate array with anatomical, registration, visualization, or chart role | coordinate space |
| distance method / metric | Declared rule for computing distances | distance_method in prose; GIFTI metric when referring to distance |
| GIFTI metric/shape data | Per-vertex GIFTI data array | GIFTI distance metric |
| experimental uncalibrated | Available only by explicit opt-in and not valid as stable inferential evidence | MVP, prototype, or promotion gate in user-facing text |
