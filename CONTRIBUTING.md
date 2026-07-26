# Contributing

Contributions should preserve the one-domain/one-values-block contract and
remain within neuroimaging geoinformatics.

Before proposing a feature, describe:

1. the spatial domain and element support;
2. required topology and metric;
3. measurement and aggregation semantics;
4. transform and provenance effects;
5. capability and performance boundaries.

Code changes need focused tests. Format readers require golden I/O cases;
semantic changes require language-independent conformance fixtures; numeric
methods require an independent reference; performance-sensitive changes
require sparse-memory regression checks.

Run:

```sh
Rscript -e "testthat::test_local()"
R CMD build .
R CMD check --as-cran neurogeo_*.tar.gz
```

Do not include private participant data, copied dependency source, or files
without redistribution permission.
