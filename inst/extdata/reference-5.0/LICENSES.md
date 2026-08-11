# neurogeo 5.0 real-data validation inputs

All files in `manifest.csv` remain download-only and are fetched from the
immutable ENIGMA Toolbox commit recorded there. They are not bundled in the
neurogeo source archive.

The ENIGMA Toolbox repository is BSD-3-Clause. Its 20-subject example dataset
contains anonymized covariates, Desikan-Killiany cortical thickness, and
cortical surface area from one MICA-MNI site. The documentation identifies ten
controls and ten individuals with epilepsy. Separate example-data license
terms are not stated, so neurogeo does not redistribute the files and makes no
broader reuse claim.

Conte69 geometry and parcellation tables are used only to define the spatial
domain and declared support family. Desikan-Killiany, Schaefer, and Glasser
atlas use requires citation of their original publications. The Glasser table
is consumed with the documented right-hemisphere code-180 correction in the
tutorial helper; the immutable downloaded file is never modified. The
validation report must record exact files, checksums, maps, support
definitions, preprocessing provenance available from the source, and sample
exclusions.

The workflow is execution evidence for neurogeo, not a powered clinical study.
No scientific claim about epilepsy is made from this small example dataset.
