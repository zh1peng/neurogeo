# Historical API prototype

Status: superseded by `API-1.0.md`; retained as design history  
Specification: NGCS 1.0

## Object contract

```r
list(
  domain = <ngeo_domain>,
  values = <n_element x n_map matrix or NULL>,
  maps = <data.frame>,
  measures = <data.frame>,
  labels = <named list>,
  provenance = <list>
)
```

The implementation-local index base is always one. Source indices and their
index base are retained separately.

## Surface example

```r
surface <- ngeo_surface(
  coordinates = matrix(
    c(0, 0, 0,
      1, 0, 0,
      0, 1, 0),
    ncol = 3,
    byrow = TRUE
  ),
  faces = matrix(c(0, 1, 2), nrow = 1),
  values = c(2.1, 2.4, 2.2),
  space = ngeo_space("fsnative", kind = "surface"),
  index_base = "zero"
)

ngeo_validate(surface, "strict")
ngeo_vertex_area(surface)
ngeo_capabilities(surface)
```

The face is stored internally as `1, 2, 3`; zero-based source indexing
remains recorded in the domain and provenance.

## Volume example

```r
volume <- ngeo_volume(
  values = array(seq_len(27), dim = c(3, 3, 3)),
  dim = c(3, 3, 3),
  affine = diag(c(2, 2, 2, 1)),
  mask = array(c(TRUE, rep(FALSE, 26)), dim = c(3, 3, 3)),
  space = ngeo_space("scanner", kind = "volume")
)

ngeo_validate(volume, "strict")
ngeo_voxel_volume(volume)
ngeo_capabilities(volume)
```

Zero values are data. They never imply a mask unless a caller explicitly
constructs such a mask before calling the constructor.

## Validation levels

- `basic`: top-level structure, alignment, geometry shape, and index range.
- `strict`: domain and space consistency plus topology diagnostics.
- `scientific`: strict checks plus warnings for unknown space and
  measurement semantics.

`ngeo_repair()` is deliberately absent from the prototype. It will be added
only after a whitelist of scientifically safe repairs is specified.

## Phase 1 additions

The controlled-S3 core now also provides:

- `ngeo_points()`, `ngeo_grayordinates()`, and `ngeo_regions()`;
- explicit measurement and transform records;
- `ngeo_domain_hash()` for implementation-level object identity;
- synchronized `ngeo_subset()` for element and map selection;
- core field accessors and capability diagnostics.

## Originally deferred APIs

The Phase 0 prototype intentionally deferred:

- adjacency and weights;
- all file importers;
- partition and aggregation.

These were implemented before 1.0 while preserving the object contract
above. The normative public contract is now `design/API-1.0.md`.
