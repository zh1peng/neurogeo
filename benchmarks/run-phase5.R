library(neurogeo)

side <- 180L
coordinates <- as.matrix(expand.grid(
  x = seq_len(side),
  y = seq_len(side)
))
coordinates3 <- cbind(coordinates, 0)
lower_left <- as.vector(outer(
  seq_len(side - 1L),
  (seq_len(side - 1L) - 1L) * side,
  "+"
))
faces <- rbind(
  cbind(lower_left, lower_left + 1L, lower_left + side),
  cbind(
    lower_left + 1L,
    lower_left + side + 1L,
    lower_left + side
  )
)
values <- sin(coordinates[, 1L] / 12) +
  cos(coordinates[, 2L] / 15)
surface <- ngeo_surface(
  coordinates3,
  faces,
  values = values,
  measures = ngeo_measure(spatial_semantics = "intensive")
)

print(system.time(
  weights <- ngeo_weights(surface, method = "mesh_contiguity", style = "W")
))
print(system.time(print(ngeo_moran(surface, weights))))
print(system.time(invisible(ngeo_local_moran(surface, weights))))

points <- ngeo_points(
  coordinates[seq_len(1000L), , drop = FALSE],
  values = values[seq_len(1000L)],
  measures = ngeo_measure(spatial_semantics = "intensive")
)
print(system.time(invisible(ngeo_variogram(points, breaks = 20L))))
