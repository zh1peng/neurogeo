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
| file-backed aligned values | NIfTI/MGH metric files | NIfTI/MGH/MGZ | no binary format | CIFTI | label/value files through their base format |
| bounded file slicing | frame/vertex when stored as volume-like data | voxel/frame | not applicable | brain-model/map | element/map when stored by a supported base format |
| authorized transform-aware resampling | nearest/barycentric after supplied affine path | nearest/trilinear/axis-aligned overlap after supplied affine path | unsupported in 3.2 | through explicit component-level maps only | through explicit support maps only |

An algorithm also requires aligned loaded values of a compatible measurement
type. Categorical values are rejected by numeric spatial statistics.
