# Migration to neurogeo 3.6

- Pass metric names directly instead of constructing `ngeo_metric` objects.
- Use `ngeo_change_support()` for legacy block maps.
- Use `ngeo_validate()` for registered NGCS objects.
- Use `ngeo_gwr()` and `ngeo_kriging()` instead of batched wrappers.
- Use replay manifests for auditable workflows; generic execution/cache
  helpers leave the public API in 4.0.
