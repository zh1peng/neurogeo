# neurogeo 2.7 API

- `ngeo_variogram_uncertainty()` corrects empirical semivariance for declared
  measurement covariance and simulates fitted parameters.
- `ngeo_kriging_uncertainty()` decomposes prediction variance into process,
  measurement, variogram-parameter, support, and total components.
- `ngeo_gwr_uncertainty()` returns local coefficient covariance, intervals,
  and bandwidth sensitivity.
- `ngeo_spatial_regression_uncertainty()` simulates SAR or SEM coefficients
  and predictions from matching Gaussian measurement covariance.
- `ngeo_car_uncertainty()` computes Gaussian proper/intrinsic CAR MAP and
  posterior covariance under an explicit observation model.
- `ngeo_support_model_ensemble()` separates within- and between-support model
  variance.
- `ngeo_model_calibration()` reports bias, RMSE, interval coverage, interval
  width, and optional residual Moran summaries.

`ngeo_kriging()` now retains its sparse observation-to-prediction linear
weights as an attribute for auditable covariance propagation. All neurogeo
2.6 APIs remain available.
