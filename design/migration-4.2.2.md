# Migration to neurogeo 4.2.2

No user-code migration is required from 4.2.1.

The release tooling now places a project-local `.r-lib` before system
libraries. This prevents validation reports from silently loading an older
installed neurogeo version.

External 4.2.2 validation data are deliberately not installed. Maintainers
run:

```r
Rscript tools/fetch-reference-422.R
Rscript tools/run-real-data-validation-422.R
```

Every download is pinned by commit, byte size, and SHA-256. The manifest
separates upstream package licensing from original data terms.
