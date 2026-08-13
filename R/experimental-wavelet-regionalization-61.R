.ngeo_wavelet_inputs <- function(x, index, basis, pairs, scales) {
  ngeo_validate(x, "basic")
  lookup <- .ngeo_coupling_index(x, index)
  if (!inherits(basis, "ngeo_spatial_basis") ||
      !identical(basis$base_hash, base_hash(x))) {
    .ngeo_abort(
      "`basis` must be a matching fixed spatial basis.",
      "ngeo_error_base_mismatch"
    )
  }
  .ngeo_validate_spatial_basis(basis, x)
  if (!is.numeric(scales) || !length(scales) || anyNA(scales) ||
      any(!is.finite(scales)) || any(scales <= 0) || anyDuplicated(scales)) {
    .ngeo_abort(
      "`scales` must contain unique positive diffusion times.",
      "ngeo_error_argument"
    )
  }
  pair_table <- .ngeo_coupling_pairs(index, pairs, TRUE)
  .ngeo_coupling_measures(
    x, unique(c(pair_table$x, pair_table$y))
  )
  list(
    lookup = lookup,
    pair_table = pair_table,
    scales = sort(as.numeric(scales))
  )
}

.ngeo_wavelet_projection <- function(values, component) {
  anchor <- values[[1L]]
  deviation <- values - anchor
  amplitude <- max(abs(deviation))
  if (!is.finite(amplitude)) {
    amplitude <- max(abs(values))
    scaled_values <- values / amplitude
    scaled <- scaled_values - scaled_values[[1L]]
  } else if (amplitude > 0) {
    scaled <- deviation / amplitude
  } else {
    scaled <- rep.int(0, length(values))
  }
  if (!is.finite(amplitude) || amplitude <= 0) {
    return(list(
      coefficient = rep.int(0, ncol(component$vectors)),
      amplitude = 0,
      total_energy = 0,
      retained_energy = 0,
      retained_fraction = NA_real_
    ))
  }
  normalized_support <- component$support / max(component$support)
  normalized_support <- normalized_support / sum(normalized_support)
  centered <- scaled - sum(normalized_support * scaled)
  coefficient <- as.numeric(crossprod(
    component$vectors,
    component$support * centered
  ))
  total_energy <- sum(component$support * centered^2)
  retained_energy <- sum(coefficient^2)
  list(
    coefficient = coefficient,
    amplitude = amplitude,
    total_energy = total_energy,
    retained_energy = retained_energy,
    retained_fraction = if (total_energy > 0) {
      min(1, retained_energy / total_energy)
    } else {
      NA_real_
    }
  )
}

.ngeo_wavelet_field <- function(projection, component, scale) {
  argument <- scale * component$eigenvalues
  kernel <- ifelse(
    is.infinite(argument) & argument > 0,
    0,
    argument * exp(-argument)
  )
  if (any(!is.finite(kernel))) {
    .ngeo_abort(
      "Wavelet scale and spectrum produced a non-finite kernel.",
      "ngeo_error_measure"
    )
  }
  projection$amplitude * as.numeric(
    component$vectors %*% (kernel * projection$coefficient)
  )
}

.ngeo_wavelet_cosine <- function(x, y, support) {
  scale_x <- max(abs(x))
  scale_y <- max(abs(y))
  if (!is.finite(scale_x) || !is.finite(scale_y)) {
    .ngeo_abort(
      "Wavelet coefficients must be finite.",
      "ngeo_error_measure"
    )
  }
  x_scaled <- if (scale_x > 0) x / scale_x else numeric(length(x))
  y_scaled <- if (scale_y > 0) y / scale_y else numeric(length(y))
  norm_x <- sqrt(sum(support * x_scaled^2))
  norm_y <- sqrt(sum(support * y_scaled^2))
  cross_scaled <- sum(support * x_scaled * y_scaled)
  list(
    coherence = if (norm_x > 0 && norm_y > 0) {
      cross_scaled / (norm_x * norm_y)
    } else {
      NA_real_
    },
    cross_scaled = cross_scaled,
    scale_x = scale_x,
    scale_y = scale_y,
    norm_x = norm_x,
    norm_y = norm_y
  )
}

.ngeo_wavelet_pair <- function(
    x, index, lookup, pair, component, scales) {
  output <- list()
  summary <- list()
  for (unit in seq_len(nrow(index$unit))) {
    layers <- lookup[unit, c(pair$x, pair$y)]
    if (anyNA(layers)) next
    current <- .ngeo_coupling_values(x, layers)
    x_values <- current[component$rows, 1L]
    y_values <- current[component$rows, 2L]
    projection_x <- .ngeo_wavelet_projection(x_values, component)
    projection_y <- .ngeo_wavelet_projection(y_values, component)
    for (scale in scales) {
      wavelet_x <- .ngeo_wavelet_field(projection_x, component, scale)
      wavelet_y <- .ngeo_wavelet_field(projection_y, component, scale)
      cosine <- .ngeo_wavelet_cosine(
        wavelet_x, wavelet_y, component$support
      )
      energy_x <- if (cosine$scale_x > 0) {
        cosine$scale_x^2 * cosine$norm_x^2
      } else {
        0
      }
      energy_y <- if (cosine$scale_y > 0) {
        cosine$scale_y^2 * cosine$norm_y^2
      } else {
        0
      }
      cross_energy <- if (cosine$scale_x > 0 && cosine$scale_y > 0) {
        cosine$scale_x * cosine$scale_y * cosine$cross_scaled
      } else {
        0
      }
      cross_wavelet <- wavelet_x * wavelet_y
      dimensional <- c(
        wavelet_x, wavelet_y, cross_wavelet,
        energy_x, energy_y, cross_energy
      )
      if (any(is.infinite(dimensional) | is.nan(dimensional))) {
        .ngeo_abort(
          paste(
            "Wavelet amplitudes or energies exceed the finite numeric range;",
            "rescale the input layers."
          ),
          "ngeo_error_measure"
        )
      }
      output[[length(output) + 1L]] <- data.frame(
        unit_id = index$unit$unit_id[[unit]],
        pair_id = pair$pair_id,
        layer_x = pair$x,
        layer_y = pair$y,
        component_id = component$component_id,
        scale = scale,
        element_id = component$element_id,
        wavelet_x = wavelet_x,
        wavelet_y = wavelet_y,
        cross_wavelet = cross_wavelet,
        stringsAsFactors = FALSE
      )
      summary[[length(summary) + 1L]] <- data.frame(
        unit_id = index$unit$unit_id[[unit]],
        pair_id = pair$pair_id,
        layer_x = pair$x,
        layer_y = pair$y,
        component_id = component$component_id,
        scale = scale,
        energy_x = energy_x,
        energy_y = energy_y,
        cross_energy = cross_energy,
        coherence = cosine$coherence,
        retained_energy_fraction_x = projection_x$retained_fraction,
        retained_energy_fraction_y = projection_y$retained_fraction,
        stringsAsFactors = FALSE
      )
    }
  }
  list(values = output, summary = summary)
}

#' Compute a localized spectral graph-wavelet scale space
#'
#' Applies the Mexican-heat kernel `g(s lambda) = s lambda exp(-s lambda)` to
#' an existing fixed spatial basis. Results are explicitly truncated to the
#' retained basis modes; retained signal-energy fractions and spectral peak
#' coverage are returned so omitted high-frequency content is visible.
#'
#' @param x Aligned multilayer `ngeo` object.
#' @param index Matching `ngeo_layer_index`.
#' @param basis Matching fixed `ngeo_spatial_basis`.
#' @param pairs Optional layer-feature pairs accepted by
#'   [ngeo_layer_coupling()].
#' @param scales Unique positive operator diffusion times. For a cotangent
#'   basis these have squared coordinate units; graph-Laplacian scales are
#'   operator-specific and dimensionless unless its operator is calibrated.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#'
#' @return An `ngeo_wavelet_coupling` with location-by-scale coefficients,
#'   support-weighted cross-energy/coherence summaries, and truncation
#'   diagnostics.
#' @templateVar example_call ngeo_wavelet_coupling(x, index, basis, scales = c(0.5, 1, 2))
#' @template stable-statistical-method
#' @references
#' Hammond, D. K., Vandergheynst, P., and Gribonval, R. (2011). Wavelets on
#' graphs via spectral graph theory. *Applied and Computational Harmonic
#' Analysis*, 30, 129--150.
#' @examples
#' \dontrun{
#' ngeo_wavelet_coupling(x, index, basis, scales = c(0.5, 1, 2))
#' }
#' @export
ngeo_wavelet_coupling <- function(
    x,
    index,
    basis,
    pairs = NULL,
    scales = c(0.25, 0.5, 1, 2, 4),
    budget = ngeo_resource_budget()) {
  input <- .ngeo_wavelet_inputs(x, index, basis, pairs, scales)
  available_units <- sum(vapply(
    seq_len(nrow(input$pair_table)),
    function(pair) {
      current <- input$pair_table[pair, , drop = FALSE]
      sum(stats::complete.cases(input$lookup[, c(current$x, current$y)]))
    },
    integer(1)
  ))
  output_rows <- as.double(available_units) * length(input$scales) *
    sum(vapply(basis$components, function(z) length(z$rows), integer(1)))
  summary_rows <- as.double(available_units) * length(input$scales) *
    length(basis$components)
  context <- .ngeo_budget_context(budget)
  .ngeo_budget_assert(
    context, "blocks", nrow(input$pair_table) * length(basis$components)
  )
  .ngeo_budget_assert(
    context, "materialized_elements", 10 * output_rows + 11 * summary_rows
  )
  .ngeo_budget_assert(
    context, "memory_bytes", 128 * output_rows + 160 * summary_rows
  )
  stage <- list()
  for (pair in seq_len(nrow(input$pair_table))) {
    for (component in basis$components) {
      stage[[length(stage) + 1L]] <- .ngeo_wavelet_pair(
        x, index, input$lookup,
        input$pair_table[pair, , drop = FALSE], component,
        input$scales
      )
      .ngeo_budget_checkpoint(context)
    }
  }
  values <- unlist(lapply(stage, `[[`, "values"), recursive = FALSE)
  summary <- unlist(lapply(stage, `[[`, "summary"), recursive = FALSE)
  if (!length(values)) {
    .ngeo_abort(
      "No complete unit-layer pair is available for graph wavelets.",
      "ngeo_error_layer_missing"
    )
  }
  values <- do.call(rbind, values)
  summary <- do.call(rbind, summary)
  rownames(values) <- rownames(summary) <- NULL
  coordinate_unit <- x$base$coordinate_space$unit
  physically_calibrated <- identical(basis$operator, "cotangent") &&
    is.character(coordinate_unit) && length(coordinate_unit) == 1L &&
    !is.na(coordinate_unit) && nzchar(coordinate_unit) &&
    !coordinate_unit %in% c("unknown", "unitless")
  scale_diagnostics <- do.call(rbind, lapply(basis$components, function(z) {
    data.frame(
      component_id = z$component_id,
      scale = input$scales,
      peak_eigenvalue = 1 / input$scales,
      maximum_retained_eigenvalue = if (length(z$eigenvalues)) {
        max(z$eigenvalues)
      } else {
        NA_real_
      },
      spectral_peak_covered = if (length(z$eigenvalues)) {
        1 / input$scales <= max(z$eigenvalues)
      } else {
        FALSE
      },
      stringsAsFactors = FALSE
    )
  }))
  result <- list(
    values = values,
    scale_summary = summary,
    base_hash = base_hash(x),
    index_hash = index$index_hash,
    basis_hash = basis$basis_hash,
    operator_hash = basis$operator_hash,
    value_hash = .ngeo_layer_digest(as.matrix(x$values)),
    analysis_hash = .ngeo_layer_digest(list(
      base_hash = base_hash(x), index_hash = index$index_hash,
      basis_hash = basis$basis_hash, pairs = input$pair_table,
      scales = input$scales, values = as.matrix(x$values)
    )),
    scales = data.frame(
      scale = input$scales,
      peak_eigenvalue = 1 / input$scales,
      central_wavelength = 2 * pi * sqrt(input$scales),
      scale_unit = if (physically_calibrated) {
        paste0(coordinate_unit, "^2")
      } else {
        "operator_inverse_eigenvalue"
      },
      wavelength_unit = if (physically_calibrated) {
        coordinate_unit
      } else {
        "operator_unit"
      },
      physical_calibration = physically_calibrated,
      stringsAsFactors = FALSE
    ),
    scale_diagnostics = scale_diagnostics,
    diagnostics = list(
      units = length(unique(values$unit_id)),
      pairs = nrow(input$pair_table),
      components = length(basis$components),
      retained_modes = vapply(
        basis$components, function(z) ncol(z$vectors), integer(1)
      ),
      kernel = "mexican_heat_s_lambda_exp_minus_s_lambda",
      truncated_to_retained_modes = TRUE,
      all_spectral_peaks_covered = all(
        scale_diagnostics$spectral_peak_covered
      )
    ),
    history = list(
      method = "support_weighted_spectral_graph_wavelet",
      base_hash = base_hash(x),
      index_hash = index$index_hash,
      basis_hash = basis$basis_hash,
      operator_hash = basis$operator_hash,
      scale_contract = if (physically_calibrated) {
        paste("cotangent diffusion time in", paste0(coordinate_unit, "^2"))
      } else {
        "operator diffusion time; physical distance calibration not claimed"
      },
      spectral_truncation = "retained basis modes only",
      status = "stable"
    )
  )
  .ngeo_gis_result(result, "ngeo_wavelet_coupling")
}

.ngeo_cotangent_geometry <- function(x, coordinates) {
  ngeo_validate(x, "basic")
  if (!inherits(x, "ngeo_surface")) {
    .ngeo_abort(
      "A cotangent operator requires an ngeo surface.",
      "ngeo_error_capability"
    )
  }
  if (!nrow(x$base$geometry$faces)) {
    .ngeo_abort(
      "A cotangent operator requires at least one triangular face.",
      "ngeo_error_geometry"
    )
  }
  if (any(!x$base$elements$included)) {
    .ngeo_abort(
      "The experimental cotangent basis requires an unmasked surface.",
      "ngeo_error_capability"
    )
  }
  coordinate_name <- if (identical(coordinates, "active")) {
    x$base$geometry$active_coordinates
  } else {
    coordinates
  }
  if (!is.character(coordinate_name) || length(coordinate_name) != 1L ||
      !coordinate_name %in% names(x$base$geometry$coordinates)) {
    .ngeo_abort(
      "`coordinates` must name one available surface coordinate set.",
      "ngeo_error_geometry"
    )
  }
  metadata <- x$base$geometry$coordinate_meta
  metadata_row <- match(coordinate_name, metadata$name)
  if (is.na(metadata_row) ||
      !isTRUE(metadata$metric_eligible[[metadata_row]])) {
    .ngeo_abort(
      "Cotangent geometry requires a metric-eligible anatomical coordinate set.",
      "ngeo_error_metric"
    )
  }
  point <- x$base$geometry$coordinates[[coordinate_name]]
  if (ncol(point) == 2L) point <- cbind(point, 0)
  face <- x$base$geometry$faces
  canonical_face <- t(apply(face, 1L, sort))
  if (anyDuplicated(as.data.frame(canonical_face))) {
    .ngeo_abort(
      "Cotangent construction rejects duplicate triangular faces.",
      "ngeo_error_geometry"
    )
  }
  edge <- rbind(
    face[, c(1L, 2L), drop = FALSE],
    face[, c(2L, 3L), drop = FALSE],
    face[, c(3L, 1L), drop = FALSE]
  )
  edge <- t(apply(edge, 1L, sort))
  edge_key <- paste(edge[, 1L], edge[, 2L], sep = "\u001f")
  edge_incidence <- tabulate(match(edge_key, unique(edge_key)))
  if (any(edge_incidence > 2L)) {
    .ngeo_abort(
      "Cotangent construction rejects non-manifold edges shared by more than two faces.",
      "ngeo_error_geometry"
    )
  }
  incident_faces <- split(
    rep.int(seq_len(nrow(face)), times = ncol(face)),
    factor(as.integer(face), levels = seq_len(nrow(point)))
  )
  for (vertex in seq_len(nrow(point))) {
    current_faces <- incident_faces[[vertex]]
    if (length(current_faces) <= 1L) next
    other <- lapply(current_faces, function(i) {
      face[i, face[i, ] != vertex]
    })
    link_edge <- do.call(rbind, other)
    link_vertices <- sort(unique(as.integer(link_edge)))
    link_adjacency <- Matrix::sparseMatrix(
      i = match(c(link_edge[, 1L], link_edge[, 2L]), link_vertices),
      j = match(c(link_edge[, 2L], link_edge[, 1L]), link_vertices),
      x = 1,
      dims = rep.int(length(link_vertices), 2L)
    )
    link_degree <- Matrix::rowSums(link_adjacency != 0)
    if (length(unique(ngeo_components(link_adjacency))) != 1L ||
        any(link_degree > 2L) || sum(link_degree == 1L) %in% c(1L, 3L)) {
      .ngeo_abort(
        "Cotangent construction rejects non-manifold surface vertices.",
        "ngeo_error_geometry"
      )
    }
  }
  first <- point[face[, 1L], , drop = FALSE]
  second <- point[face[, 2L], , drop = FALSE]
  third <- point[face[, 3L], , drop = FALSE]
  cross <- cbind(
    (second[, 2L] - first[, 2L]) * (third[, 3L] - first[, 3L]) -
      (second[, 3L] - first[, 3L]) * (third[, 2L] - first[, 2L]),
    (second[, 3L] - first[, 3L]) * (third[, 1L] - first[, 1L]) -
      (second[, 1L] - first[, 1L]) * (third[, 3L] - first[, 3L]),
    (second[, 1L] - first[, 1L]) * (third[, 2L] - first[, 2L]) -
      (second[, 2L] - first[, 2L]) * (third[, 1L] - first[, 1L])
  )
  double_area <- sqrt(rowSums(cross^2))
  edge_length <- sqrt(rbind(
    rowSums((second - first)^2),
    rowSums((third - second)^2),
    rowSums((first - third)^2)
  ))
  local_scale <- apply(edge_length, 2L, max)
  geometry_tolerance <- sqrt(.Machine$double.eps)
  if (any(!is.finite(double_area)) || any(!is.finite(local_scale)) ||
      any(local_scale <= 0) ||
      any(double_area <= geometry_tolerance * local_scale^2)) {
    .ngeo_abort(
      "Cotangent construction rejects zero-area or numerically degenerate faces.",
      "ngeo_error_geometry"
    )
  }
  list(
    coordinate_name = coordinate_name,
    point = point,
    face = face,
    double_area = double_area,
    boundary_edges = sum(edge_incidence == 1L)
  )
}

.ngeo_cotangent_at <- function(vertex, first, second, point, double_area) {
  left <- point[first, , drop = FALSE] - point[vertex, , drop = FALSE]
  right <- point[second, , drop = FALSE] - point[vertex, , drop = FALSE]
  rowSums(left * right) / double_area
}

.ngeo_cotangent_operator <- function(x, coordinates = "active") {
  geometry <- .ngeo_cotangent_geometry(x, coordinates)
  face <- geometry$face
  cotangent <- cbind(
    .ngeo_cotangent_at(
      face[, 1L], face[, 2L], face[, 3L],
      geometry$point, geometry$double_area
    ),
    .ngeo_cotangent_at(
      face[, 2L], face[, 3L], face[, 1L],
      geometry$point, geometry$double_area
    ),
    .ngeo_cotangent_at(
      face[, 3L], face[, 1L], face[, 2L],
      geometry$point, geometry$double_area
    )
  )
  from <- c(face[, 2L], face[, 3L], face[, 1L])
  to <- c(face[, 3L], face[, 1L], face[, 2L])
  weight <- c(cotangent[, 1L], cotangent[, 2L], cotangent[, 3L]) / 2
  n <- nrow(geometry$point)
  adjacency <- .ngeo_as_dgCMatrix(Matrix::sparseMatrix(
    i = c(from, to),
    j = c(to, from),
    x = c(weight, weight),
    dims = c(n, n)
  ))
  diag(adjacency) <- 0
  stiffness <- .ngeo_as_dgCMatrix(
    Matrix::Diagonal(x = Matrix::rowSums(adjacency)) - adjacency
  )
  mass <- numeric(n)
  face_mass <- geometry$double_area / 6
  for (column in seq_len(3L)) {
    contribution <- rowsum(
      face_mass, face[, column], reorder = FALSE
    )
    rows <- as.integer(rownames(contribution))
    mass[rows] <- mass[rows] + contribution[, 1L]
  }
  if (any(!is.finite(mass)) || any(mass <= 0)) {
    .ngeo_abort(
      "Every surface vertex must have positive incident barycentric area.",
      "ngeo_error_geometry"
    )
  }
  topology <- .ngeo_as_dgCMatrix(Matrix::sparseMatrix(
    i = c(
      face[, 1L], face[, 2L], face[, 2L],
      face[, 3L], face[, 3L], face[, 1L]
    ),
    j = c(
      face[, 2L], face[, 1L], face[, 3L],
      face[, 2L], face[, 1L], face[, 3L]
    ),
    x = 1,
    dims = c(n, n)
  ))
  list(
    stiffness = stiffness,
    mass = mass,
    topology = topology,
    component = ngeo_components(topology),
    coordinate_name = geometry$coordinate_name,
    negative_cotangent_edges = sum(adjacency@x < 0) / 2,
    surface_area = sum(geometry$double_area) / 2,
    boundary_edges = geometry$boundary_edges
  )
}

.ngeo_cotangent_basis <- function(
    x,
    coordinates = "active",
    n_modes = 64L,
    components = c("separate", "error"),
    tolerance = 1e-8,
    budget = ngeo_resource_budget()) {
  n_modes <- .ngeo_as_integer(n_modes, "n_modes")
  components <- match.arg(components)
  if (length(n_modes) != 1L || n_modes < 1L ||
      !is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance <= 0) {
    .ngeo_abort(
      "Cotangent mode count and tolerance are invalid.",
      "ngeo_error_argument"
    )
  }
  operator <- .ngeo_cotangent_operator(x, coordinates)
  component_ids <- sort(unique(operator$component))
  if (identical(components, "error") && length(component_ids) > 1L) {
    .ngeo_abort(
      "The surface is disconnected; use `components = \"separate\"`.",
      "ngeo_error_topology"
    )
  }
  output_cells <- sum(vapply(component_ids, function(id) {
    size <- sum(operator$component == id)
    as.double(size) * min(n_modes, max(0L, size - 1L))
  }, numeric(1)))
  budget_context <- .ngeo_budget_context(budget)
  .ngeo_budget_assert(budget_context, "materialized_elements", output_cells)
  .ngeo_budget_assert(
    budget_context, "memory_bytes",
    8 * output_cells + 16 * length(operator$stiffness@x)
  )
  basis_components <- lapply(seq_along(component_ids), function(position) {
    id <- component_ids[[position]]
    rows <- which(operator$component == id)
    decomposition <- .ngeo_partial_eigen(
      operator$stiffness[rows, rows, drop = FALSE],
      operator$mass[rows], n_modes, tolerance, budget_context
    )
    diagnostic <- .ngeo_basis_diagnostics(
      operator$stiffness[rows, rows, drop = FALSE],
      operator$mass[rows], decomposition$vectors,
      decomposition$values, tolerance
    )
    list(
      component_id = .ngeo_basis_component_id(x, rows, position),
      rows = rows,
      element_id = x$base$elements$element_id[rows],
      support = operator$mass[rows],
      eigenvalues = decomposition$values,
      vectors = decomposition$vectors,
      mode_within_component = seq_along(decomposition$values),
      degenerate_cluster = .ngeo_degenerate_clusters(
        decomposition$values, tolerance
      ),
      diagnostics = diagnostic,
      solver = decomposition$method
    )
  })
  names(basis_components) <- vapply(
    basis_components, `[[`, character(1), "component_id"
  )
  support_hash <- .ngeo_layer_digest(list(
    base_hash = base_hash(x), mass = operator$mass,
    coordinates = operator$coordinate_name
  ))
  operator_hash <- .ngeo_layer_digest(list(
    base_hash = base_hash(x), stiffness = operator$stiffness,
    support_hash = support_hash
  ))
  result <- list(
    operator = "cotangent",
    coordinates = operator$coordinate_name,
    base_hash = base_hash(x),
    space_hash = ngeo_coordinate_space_hash(x$base$coordinate_space),
    operator_hash = operator_hash,
    basis_hash = .ngeo_layer_digest(list(
      operator_hash = operator_hash,
      n_modes = n_modes,
      components = lapply(basis_components, function(z) list(
        rows = z$rows,
        eigenvalues = z$eigenvalues,
        vectors = z$vectors
      ))
    )),
    support = list(
      values = operator$mass,
      type = "lumped_barycentric_area",
      hash = support_hash
    ),
    components = basis_components,
    n_modes_requested = n_modes,
    symmetrized = FALSE,
    tolerance = tolerance,
    diagnostics = list(
      surface_area = operator$surface_area,
      boundary_edges = operator$boundary_edges,
      mass_total = sum(operator$mass),
      negative_cotangent_edges = operator$negative_cotangent_edges,
      expected_zero_modes = length(component_ids),
      max_residual = max(c(0, unlist(lapply(
        basis_components, function(z) z$diagnostics$residual
      )))),
      max_orthogonality_error = max(c(0, vapply(
        basis_components,
        function(z) z$diagnostics$orthogonality_error,
        numeric(1)
      )))
    ),
    history = list(
      method = "cotangent_finite_element_laplace_beltrami",
      uses_map_values = FALSE,
      uses_group_labels = FALSE,
      mass = "lumped_barycentric_area",
      boundary_condition = "natural_neumann_on_mesh_boundary",
      pointwise_convergence_claimed = FALSE,
      status = "stable"
    )
  )
  result$basis_hash <- .ngeo_spatial_basis_hash(result)
  class(result) <- "ngeo_spatial_basis"
  result
}

.ngeo_regionalization_features <- function(x, layers, support) {
  selected <- .ngeo_layer_selection(x, layers)
  if (!length(selected) || is.null(x$values)) {
    .ngeo_abort(
      "Regionalization requires one or more loaded numeric layers.",
      "ngeo_error_values"
    )
  }
  .ngeo_projection_measures(x, selected)
  values <- as.matrix(x$values[, selected, drop = FALSE])
  if (any(!is.finite(values))) {
    .ngeo_abort(
      "Regionalization requires finite feature values.",
      "ngeo_error_missing"
    )
  }
  standardized <- lapply(seq_len(ncol(values)), function(i) {
    .ngeo_weighted_standardize(values[, i], support)$values
  })
  if (any(vapply(standardized, function(z) any(!is.finite(z)), logical(1)))) {
    .ngeo_abort(
      "Regionalization features must vary over the spatial base.",
      "ngeo_error_measure"
    )
  }
  list(
    values = do.call(cbind, standardized),
    selected = selected
  )
}

.ngeo_regionalization_edges <- function(adjacency, features, element_id) {
  entries <- Matrix::summary(adjacency)
  keep <- entries$i < entries$j & entries$x != 0
  from <- entries$i[keep]
  to <- entries$j[keep]
  if (!length(from)) {
    .ngeo_abort(
      "Regionalization requires a connected graph with at least one edge.",
      "ngeo_error_topology"
    )
  }
  difference <- features[from, , drop = FALSE] -
    features[to, , drop = FALSE]
  output <- data.frame(
    from = from,
    to = to,
    from_id = element_id[from],
    to_id = element_id[to],
    cost = sqrt(rowSums(difference^2)),
    stringsAsFactors = FALSE
  )
  swap <- output$from_id > output$to_id
  first <- output$from_id
  output$from_id[swap] <- output$to_id[swap]
  output$to_id[swap] <- first[swap]
  output
}

.ngeo_regionalization_mst <- function(edges, n) {
  parent <- seq_len(n)
  rank <- integer(n)
  root <- function(node) {
    while (parent[[node]] != node) {
      parent[[node]] <<- parent[[parent[[node]]]]
      node <- parent[[node]]
    }
    node
  }
  selected <- integer()
  for (edge in order(edges$cost, edges$from_id, edges$to_id)) {
    first <- root(edges$from[[edge]])
    second <- root(edges$to[[edge]])
    if (first == second) next
    selected <- c(selected, edge)
    if (rank[[first]] < rank[[second]]) {
      parent[[first]] <- second
    } else if (rank[[first]] > rank[[second]]) {
      parent[[second]] <- first
    } else {
      parent[[second]] <- first
      rank[[first]] <- rank[[first]] + 1L
    }
    if (length(selected) == n - 1L) break
  }
  if (length(selected) != n - 1L) {
    .ngeo_abort(
      "Regionalization currently requires one connected spatial graph.",
      "ngeo_error_topology"
    )
  }
  edges[selected, , drop = FALSE]
}

.ngeo_tree_adjacency <- function(tree, n) {
  .ngeo_as_dgCMatrix(Matrix::sparseMatrix(
    i = c(tree$from, tree$to),
    j = c(tree$to, tree$from),
    x = 1,
    dims = c(n, n)
  ))
}

.ngeo_regionalization_cut <- function(
    tree, n, n_regions, support, min_elements, min_support,
    max_search_states, budget) {
  target_cuts <- n_regions - 1L
  edge_order <- order(
    -tree$cost, tree$from_id, tree$to_id, method = "radix"
  )
  context <- .ngeo_budget_context(budget)
  states <- 0L
  best <- integer()

  components_after <- function(removed) {
    retained <- if (length(removed)) tree[-removed, , drop = FALSE] else tree
    ngeo_components(.ngeo_tree_adjacency(retained, n))
  }
  search <- function(start, removed) {
    states <<- states + 1L
    if (states > max_search_states) {
      .ngeo_abort(
        paste(
          "Regionalization exceeded `max_search_states`; increase the",
          "explicit search limit or reduce the requested region count."
        ),
        "ngeo_error_resource"
      )
    }
    .ngeo_budget_assert(context, "blocks", states)
    .ngeo_budget_checkpoint(context)
    component <- components_after(removed)
    size <- tabulate(component)
    support_size <- as.numeric(rowsum(support, component, reorder = FALSE))
    if (any(size < min_elements) || any(support_size < min_support)) {
      return(invisible(NULL))
    }
    if (length(removed) == target_cuts) {
      best <<- removed
      return(TRUE)
    }
    needed <- target_cuts - length(removed)
    last <- length(edge_order) - needed + 1L
    if (start > last) return(invisible(NULL))
    for (position in seq.int(start, last)) {
      if (isTRUE(search(
        position + 1L, c(removed, edge_order[[position]])
      ))) return(TRUE)
    }
    FALSE
  }
  search(1L, integer())
  if (!length(best)) {
    .ngeo_abort(
      paste(
        "The selected deterministic MST cannot satisfy the requested",
        "region count and minimum-size constraints."
      ),
      "ngeo_error_partition"
    )
  }
  adjacency <- .ngeo_tree_adjacency(tree[-best, , drop = FALSE], n)
  list(
    component = ngeo_components(adjacency),
    accepted = best,
    adjacency = adjacency,
    search_states = states,
    objective = sum(tree$cost[best])
  )
}

#' Learn contiguous regions from aligned layers
#'
#' Standardizes selected numeric layers, constructs multivariate edge
#' dissimilarities on an undirected spatial graph, and cuts a deterministic
#' minimum spanning tree while enforcing minimum element and support sizes.
#' This is constrained MST regionalization, not an anatomical segmentation or
#' a calibrated inferential procedure.
#'
#' @param x An `ngeo` object with finite numeric layers.
#' @param spatial_weights Matching spatial weights; directed graphs are reduced
#'   to their undirected union for contiguity.
#' @param layers One or more layer selectors used with equal weight after
#'   standardization.
#' @param n_regions Requested number of connected regions.
#' @param min_elements Minimum number of base elements in every region.
#' @param min_support Minimum total base support in every region.
#' @param max_search_states Hard cap on bounded constrained cut-search states.
#'   Exhausting the cap is a resource error, not evidence that no partition
#'   exists.
#' @param budget Hard execution limits from [ngeo_resource_budget()].
#'
#' @return An `ngeo_partition` with regionalization diagnostics. A learned
#'   partition must be frozen on training data before testing downstream
#'   outcomes.
#' @templateVar example_call ngeo_contiguous_regionalization(x, spatial_weights, layers = c("thickness", "myelin"), n_regions = 10)
#' @template stable-statistical-method
#' @references
#' Assuncao, R. M., Neves, M. C., Camara, G., and da Costa Freitas, C. (2006).
#' Efficient regionalization techniques for socio-economic geographical units
#' using minimum spanning trees. *International Journal of Geographical
#' Information Science*, 20, 797--811.
#' @examples
#' \dontrun{
#' ngeo_contiguous_regionalization(
#'   x, spatial_weights, layers = c("thickness", "myelin"), n_regions = 10
#' )
#' }
#' @export
ngeo_contiguous_regionalization <- function(
    x,
    spatial_weights,
    layers,
    n_regions,
    min_elements = 1L,
    min_support = 0,
    max_search_states = 100000L,
    budget = ngeo_resource_budget()) {
  budget_context <- .ngeo_budget_context(budget)
  ngeo_validate(x, "basic")
  if (!inherits(spatial_weights, "ngeo_spatial_weights") ||
      !identical(spatial_weights$base_hash, base_hash(x))) {
    .ngeo_abort(
      "`spatial_weights` must match the regionalization base.",
      "ngeo_error_base_mismatch"
    )
  }
  n <- nrow(x$base$elements)
  n_regions <- .ngeo_as_integer(n_regions, "n_regions")
  min_elements <- .ngeo_as_integer(min_elements, "min_elements")
  max_search_states <- .ngeo_as_integer(
    max_search_states, "max_search_states"
  )
  if (length(n_regions) != 1L || n_regions < 2L || n_regions > n ||
      length(min_elements) != 1L || min_elements < 1L ||
      length(max_search_states) != 1L || max_search_states < 1L ||
      !is.numeric(min_support) || length(min_support) != 1L ||
      is.na(min_support) || !is.finite(min_support) || min_support < 0) {
    .ngeo_abort(
      "Region count and minimum-size constraints are invalid.",
      "ngeo_error_argument"
    )
  }
  if (n_regions > 256L) {
    .ngeo_abort(
      "The bounded 6.1 regionalization search supports at most 256 regions.",
      "ngeo_error_resource"
    )
  }
  support_info <- .ngeo_support_weights(x, "auto")
  if (as.double(n_regions) * min_elements > n ||
      as.double(n_regions) * min_support > sum(support_info$values)) {
    .ngeo_abort(
      "The requested region count cannot satisfy the minimum-size constraints.",
      "ngeo_error_partition"
    )
  }
  selected_budget <- .ngeo_layer_selection(x, layers)
  value_cells <- as.double(n) * length(selected_budget)
  weight_nonzero <- length(
    .ngeo_as_dgCMatrix(spatial_weights$raw_matrix)@x
  )
  .ngeo_budget_assert(
    budget_context, "materialized_elements",
    4 * value_cells + 8 * n + 4 * weight_nonzero
  )
  .ngeo_budget_assert(
    budget_context, "memory_bytes",
    40 * value_cells + 64 * n + 64 * weight_nonzero
  )
  feature <- .ngeo_regionalization_features(
    x, layers, support_info$values
  )
  adjacency <- .ngeo_undirected_adjacency(spatial_weights)
  edges <- .ngeo_regionalization_edges(
    adjacency, feature$values, x$base$elements$element_id
  )
  tree <- .ngeo_regionalization_mst(edges, n)
  .ngeo_budget_assert(
    budget_context, "materialized_elements", 8 * n + 8 * nrow(edges)
  )
  .ngeo_budget_assert(
    budget_context, "memory_bytes", 64 * n + 96 * nrow(edges)
  )
  cut <- .ngeo_regionalization_cut(
    tree, n, n_regions, support_info$values, min_elements, min_support,
    max_search_states, budget_context
  )
  membership <- sprintf("region_%03d", cut$component)
  partition <- ngeo_partition(x, membership, unlabeled_policy = "error")
  partition$source <- "contiguous_mst_regionalization"
  partition$history$operations <- c(
    partition$history$operations,
    list(.ngeo_operation(
      "contiguous_mst_regionalization",
      list(
        layers = x$layers$name[feature$selected],
        layer_id = x$layers$layer_id[feature$selected],
        n_regions = n_regions,
        min_elements = min_elements,
        min_support = min_support,
        weights_method = spatial_weights$method
      )
    ))
  )
  region_rows <- split(
    seq_len(n), factor(cut$component, levels = sort(unique(cut$component)))
  )
  region_support <- as.numeric(rowsum(
    support_info$values, cut$component, reorder = FALSE
  ))
  region_centroid <- rowsum(
    support_info$values * feature$values,
    cut$component, reorder = FALSE
  ) / region_support
  within_region_ss <- sum(support_info$values * (
    feature$values - region_centroid[cut$component, , drop = FALSE]
  )^2)
  partition$regionalization <- list(
    method = "constrained_mst_edge_cut",
    selected_layer_id = x$layers$layer_id[feature$selected],
    selected_layers = x$layers$name[feature$selected],
    tree = tree,
    cut_edges = tree[cut$accepted, , drop = FALSE],
    region = data.frame(
      region_id = sprintf("region_%03d", seq_along(region_rows)),
      elements = vapply(region_rows, length, integer(1)),
      support = vapply(
        region_rows, function(rows) sum(support_info$values[rows]), numeric(1)
      ),
      stringsAsFactors = FALSE
    ),
    within_region_sum_of_squares = within_region_ss,
    retained_mst_edge_cost = sum(tree$cost[-cut$accepted]),
    feature_weighting = "equal after per-layer standardization",
    standardization_weighting = "base_support",
    support_hash = support_info$hash,
    search_states = cut$search_states,
    edge_cost_ties = anyDuplicated(edges$cost) > 0L,
    mst_uniqueness_claimed = FALSE,
    cut_selection = "bounded_first_feasible_descending_edge_cost",
    globally_optimal = FALSE,
    status = "stable",
    inference = paste(
      "analytic regionalization only; freeze on training data before",
      "testing regional effects"
    )
  )
  .ngeo_validate_partition(partition, x)
  partition
}
