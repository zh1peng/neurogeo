# Documentation information architecture 6.0

The documentation follows the user's task rather than the package's internal
file or development history. `documentation-manifest-6.0.csv` is the canonical
source/route inventory.

| Content area | Belongs here | Does not belong here |
|---|---|---|
| Home | user outcomes, four input types, scope, first decision | schema history or long API lists |
| Start here | installation, 15-minute quickstart, first validation | advanced statistical methods |
| Tutorial | complete learning sequence with concepts and interpretation | disconnected recipes or developer notes |
| How-to | one concrete task such as reading NIfTI or selecting a layer | conceptual essays |
| Concept | stable mental model, glossary, units, base/layer/measure/support | release chronology |
| Methods & assumptions | estimand, sampling unit, null, metric, support, uncertainty, references | installation instructions |
| API reference | lifecycle-labelled signatures and return contracts | teaching narrative |
| Validation & reproducibility | fixtures, oracles, calibration, evidence, replay | unverified scientific claims |
| Developer / specification | NGCS schemas, ADRs, contribution rules | first-use instructions |
| Migration / NEWS | version changes, compatibility, migration reports | current mental model except explicit mappings |

From either home page, NIfTI, surface, CIFTI, and ROI/cohort users reach the
matching workflow in at most two clicks: Home → format selector → workflow.
Stable and experimental methods must never share an unlabeled navigation
group. A translated page links to its declared counterpart; a source-only page
is labelled as such by the manifest rather than silently falling back to a
different language.

Generated VitePress pages are build artifacts and are not authoritative source.
Their front matter records the Rmd SHA-256 and links to the actual source file.
Deployment is allowed only when the built `documentation-build.json` matches
the current package version and documentation manifest hash.
