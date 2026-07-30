# neurogeo API 4.1.1

Version 4.1.1 is a maintenance release. It adds no public function, class,
argument, return field, or NGCS schema.

Spatial-model weight subsetting now uses the package's staged sparse Matrix
coercion helper. The resulting matrix remains a general numeric compressed
sparse column matrix with unchanged row order, normalization, and isolate
semantics.

The package continues to implement NGCS 3.5.
