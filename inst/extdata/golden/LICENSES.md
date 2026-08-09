# Teaching and golden fixture licenses

The files in this directory are synthetic and were created by the neurogeo
project. They contain no participant data and no geometry copied from a human
template or atlas.

The project dedicates these fixture files to the public domain under
[CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/). Package source
code that generates or reads them remains under neurogeo's MIT license.

`tools/generate-golden-fixtures.R` is the normative generator for NIfTI,
GIFTI, CIFTI, FreeSurfer, and associated metadata fixtures. The two CSV files
are fixed project-authored teaching tables. The generated formats deliberately
use tiny artificial arrays and a tetrahedron; they are format and workflow
fixtures, not anatomical or clinical reference data.
