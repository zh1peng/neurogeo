# Algorithm capability matrix

| Operation | Surface | Volume | Points | Grayordinates | Regions |
|---|---:|---:|---:|---:|---:|
| explicit coordinates | yes | world affine | yes | when geometry/affine exists | with centroids |
| implicit adjacency | mesh | 6/18/26 voxel | no | component-local | when supplied/derived |
| support size | vertex area | voxel volume | unknown by default | per component | stored/derived |
| Euclidean distance | coordinate set | world coordinates | coordinates | when complete | centroids |
| edge-geodesic/hops | yes | hops | no | with surface geometry | hops when adjacent |
| sparse contiguity weights | yes | yes | no | with geometry | when adjacent |
| KNN/distance-band weights | yes | yes | yes | with complete coordinates | with centroids |
| crisp partition | yes | yes | yes | yes | yes |
| semantic aggregation | yes | yes | support required for intensive | support required for intensive | yes |
| Moran/Geary/LISA | with matching weights | with matching weights | with explicit weights | with matching weights | with matching weights |
| variogram | with metric | world metric | coordinate metric | with complete metric | with centroids |
| support-map source/target | yes | yes | with explicit support for intensive change | yes when component support is available | yes |
| known support-map builder | nearest/barycentric registration | affine/trilinear/axis-aligned overlap | aligned atlas with explicit support | aligned atlas when component support exists | aligned atlas |
| support diagnostics | sparse | sparse | sparse | sparse | sparse |
| domain-bound covariance | yes | yes | yes | yes | yes |
| operator ensembles | yes | yes | with explicit support | yes | yes |
| support-aware effect comparison | with complete maps | with complete maps | with explicit support | with complete component support | with complete maps |
| common-support family inference | with complete maps and declared null | with complete maps and declared null | with complete maps and declared null | with complete maps and declared null | with complete maps and declared null |
| cross-atlas consensus | effect summaries | effect summaries | effect summaries | effect summaries | effect summaries |
| boundary ensemble inference | common ordered domains | common ordered domains | with explicit support | common ordered domains | common ordered domains |
| affine geometry application | metric coordinates | voxel affine | coordinates | component geometry/affines | with centroids |
| exact space registry/audit | yes | yes | yes | component-aware | yes |
| supplied transform graph/path | affine geometry | affine geometry | affine geometry | component geometry/affines | affine centroids |
| kernel regression | edge geodesic | world metric | coordinate metric | with complete eligible metric | with centroids |
| fitted variogram / local kriging | bounded eligible metric | bounded world metric | bounded coordinate metric | with complete eligible metric | with centroids |
| GWR | bounded eligible metric | bounded world metric | bounded coordinate metric | with complete eligible metric | with centroids |
| SAR/SEM/CAR | with matching weights | with matching weights | with explicit weights | with matching weights | with matching weights |
| uncertainty-aware variogram/kriging/GWR | with domain covariance | with domain covariance | with domain covariance | with eligible metric and covariance | with centroids and covariance |
| SAR/SEM simulation and Gaussian CAR posterior | with matching weights/covariance | with matching weights/covariance | with explicit weights/covariance | with matching weights/covariance | with matching weights/covariance |
| within/between-support model ensemble | declared support family | declared support family | declared support family | declared support family | declared support family |
| delayed values/chunks | yes | yes | yes | yes | yes |
| block support operator | yes | yes | with explicit support | yes | yes |
| blockwise change/diagnostics/variance | yes | yes | with explicit support | yes | yes |
| delayed streaming summary/covariance/OLS | yes | yes | yes | yes | yes |
| delayed streaming Moran | with matching weights | with matching weights | with explicit weights | with matching weights | with matching weights |
| bounded execution/checkpoint/cache | yes | yes | yes | yes | yes |
| pure-R CIFTI write | component | component | no | dscalar/dlabel/dtseries | component |
| CIFTI 2.9 metadata/datatype contract | component metadata | voxel brain models | no | NamedMap/labels/time axes | no |
| BIDS derivative transaction | when explicitly named | when explicitly named | coordinate derivatives only | dscalar/dlabel/dtseries | label/value derivatives |
| chunked support-map exchange | yes | yes | yes | yes | yes |
| NGCS 3.0 schema validation | yes | yes | yes | yes | yes |
| canonical portable metadata manifest | yes | yes | yes | yes | yes |
| file-backed aligned values | NIfTI/MGH metric files | NIfTI/MGH/MGZ | no binary format | CIFTI | label/value files through their base format |
| bounded file slicing | frame/vertex when stored as volume-like data | voxel/frame | not applicable | brain-model/map | element/map when stored by a supported base format |
| authorized transform-aware resampling | nearest/barycentric after supplied affine path | nearest/trilinear/axis-aligned overlap after supplied affine path | unsupported in 3.2 | through explicit component-level maps only | through explicit support maps only |

An algorithm also requires aligned loaded values of a compatible measurement
type. Categorical values are rejected by numeric spatial statistics.
