# neurogeo 4.4.1 API contract

Version 4.4.1 adds no public function, class, or object schema.

## Exact CAR resource boundary

`ngeo_car()` and `ngeo_car_uncertainty()` now apply the same exact dense
dimension guard used by exact SAR/SEM likelihoods. The default maximum is
2,000 observations and can be changed with
`options(neurogeo.max_exact_logdet = n)`.

Requests above the limit fail with `ngeo_error_resource` before a dense
smoother, precision matrix, or covariance matrix is materialized. Larger
problems should use the iterative spatial-model APIs when applicable.

## Generic CIFTI output

`write_ngeo()` recognizes `.dscalar.nii`, `.dlabel.nii`, and `.dtseries.nii`
paths and dispatches them to the pure-R CIFTI writer. It returns the same
compact output manifest used for other generic formats.

The dedicated `write_ngeo_cifti()` function remains available when explicit
CIFTI axis or metadata control is required.
