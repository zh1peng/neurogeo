# Governance

`neurogeo` separates specification governance from reference-implementation
maintenance.

- Major API changes require a public issue and accepted ADR.
- Specification changes require one engineering reviewer and one scientific
  reviewer.
- Focused, tested development changes are committed directly to `main`.
  Pull requests are used only when the maintainer explicitly requests one.
- GitHub Releases and version tags are not created under the current
  maintainer policy.
- Roadmap priority follows the neurogeospatial core scope. A request must
  state its domain, support, topology, metric, and measurement implications.
- Security or scientific-correctness issues may use an embargoed review, but
  the final change and rationale are documented.

CRAN, Bioconductor, or another package-registry submission requires separate
explicit maintainer authorization.
