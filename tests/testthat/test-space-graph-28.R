space_graph_fixture <- function() {
  spaces <- list(
    A = ngeo_space("A", template = "template-a"),
    B = ngeo_space("B", template = "template-b"),
    C = ngeo_space("C", template = "template-c"),
    D = ngeo_space("D", template = "template-d")
  )
  affine <- function(from, to, translation) {
    matrix <- diag(4)
    matrix[1:3, 4L] <- translation
    ngeo_transform(
      spaces[[from]],
      spaces[[to]],
      "affine",
      method = paste(from, to),
      parameters = list(matrix = matrix)
    )
  }
  transforms <- list(
    ab = affine("A", "B", c(1, 0, 0)),
    bc = affine("B", "C", c(0, 2, 0)),
    ad = affine("A", "D", c(0, 0, 3)),
    dc = affine("D", "C", c(1, 2, -3)),
    ca = affine("C", "A", c(-1, -2, 0))
  )
  registry <- ngeo_space_registry(
    spaces,
    aliases = c(native = "A", standard = "C")
  )
  list(spaces = spaces, transforms = transforms, registry = registry)
}

test_that("space registry uses exact identities and explicit aliases", {
  fixture <- space_graph_fixture()
  expect_identical(
    ngeo_resolve_space(fixture$registry, "native"),
    fixture$spaces$A
  )
  expect_identical(
    ngeo_resolve_space(
      fixture$registry,
      ngeo_space_hash(fixture$spaces$B)
    ),
    fixture$spaces$B
  )

  duplicate_name <- ngeo_space("A", kind = "volume")
  ambiguous <- ngeo_space_registry(c(
    fixture$registry$spaces,
    list(duplicate_name)
  ))
  expect_error(
    ngeo_resolve_space(ambiguous, "A"),
    class = "ngeo_error_space_ambiguity"
  )
  explicit <- ngeo_register_space(
    ambiguous, duplicate_name, aliases = "volume-A"
  )
  expect_identical(
    ngeo_resolve_space(explicit, "volume-A"),
    duplicate_name
  )
})

test_that("transform paths compose exactly and bind provenance", {
  fixture <- space_graph_fixture()
  graph <- ngeo_transform_graph(
    fixture$registry,
    fixture$transforms[c("ab", "bc")],
    edge_ids = c("ab", "bc")
  )
  path <- ngeo_transform_path(graph, "native", "standard")
  direct <- ngeo_compose_transform(
    fixture$transforms$ab,
    fixture$transforms$bc
  )
  provenance <- ngeo_transform_path_provenance(path)

  expect_s3_class(path, "ngeo_transform_path")
  expect_equal(
    path$composed$parameters$matrix,
    direct$parameters$matrix
  )
  expect_identical(path$tokens, c("ab", "bc"))
  expect_identical(provenance$edge_hashes, path$edge_hashes)
  expect_identical(provenance$path_hash, path$path_hash)
})

test_that("path application requires authorization and preserves edge audit", {
  fixture <- space_graph_fixture()
  graph <- ngeo_transform_graph(
    fixture$registry,
    fixture$transforms[c("ab", "bc")],
    edge_ids = c("ab", "bc")
  )
  path <- ngeo_transform_path(graph, "A", "C")
  x <- ngeo_points(
    cbind(x = c(0, 1), y = c(0, 1)),
    values = cbind(signal = c(1, 2)),
    space = fixture$spaces$A
  )
  expect_error(
    ngeo_apply_transform_path(x, path),
    class = "ngeo_error_authorization"
  )
  changed <- ngeo_apply_transform_path(x, path, authorize = TRUE)

  expect_equal(changed$domain$coordinates[, 1L], c(1, 2))
  expect_equal(changed$domain$coordinates[, 2L], c(2, 3))
  expect_identical(changed$domain$space, fixture$spaces$C)
  expect_identical(
    changed$provenance$operations[[length(
      changed$provenance$operations
    )]]$parameters$path_hash,
    path$path_hash
  )
})

test_that("ambiguity fails until the caller selects a shortest path", {
  fixture <- space_graph_fixture()
  graph <- ngeo_transform_graph(
    fixture$registry,
    fixture$transforms[c("ab", "bc", "ad", "dc")],
    edge_ids = c("ab", "bc", "ad", "dc")
  )
  expect_error(
    ngeo_transform_path(graph, "A", "C"),
    class = "ngeo_error_transform_ambiguity"
  )
  selected <- ngeo_transform_path(
    graph,
    "A",
    "C",
    selection = c("ab", "bc")
  )
  diagnostics <- ngeo_transform_graph_diagnostics(graph)

  expect_identical(selected$tokens, c("ab", "bc"))
  expect_gt(nrow(diagnostics$ambiguous_pairs), 0L)
})

test_that("cycles and space mismatches are explicitly diagnosed", {
  fixture <- space_graph_fixture()
  graph <- ngeo_transform_graph(
    fixture$registry,
    fixture$transforms[c("ab", "bc", "ca")],
    edge_ids = c("ab", "bc", "ca")
  )
  diagnostics <- ngeo_transform_graph_diagnostics(graph)
  expect_length(diagnostics$cycle_space_hashes, 3L)

  left <- ngeo_space(
    "surface-mm", kind = "surface", units = "mm",
    structure = "CORTEX_LEFT", density = "32k"
  )
  right <- ngeo_space(
    "surface-m", kind = "surface", units = "m",
    structure = "CORTEX_RIGHT", density = "164k"
  )
  audit <- ngeo_space_audit(left, right)
  expect_false(attr(audit, "compatible"))
  expect_true(all(
    c("units", "structure") %in%
      audit$field[audit$severity == "incompatible"]
  ))
  expect_false(audit$match[audit$field == "density"])
})

test_that("inverse paths require eligible non-lossy affine edges", {
  fixture <- space_graph_fixture()
  graph <- ngeo_transform_graph(
    fixture$registry,
    fixture$transforms$ab,
    edge_ids = "ab",
    invertible = TRUE,
    lossy = FALSE
  )
  inverse <- ngeo_transform_path(
    graph, "B", "A", allow_inverse = TRUE
  )
  expect_identical(inverse$tokens, "ab^-1")
  expect_equal(
    inverse$composed$parameters$matrix,
    solve(fixture$transforms$ab$parameters$matrix)
  )

  lossy <- ngeo_transform_graph(
    fixture$registry,
    fixture$transforms$ab,
    edge_ids = "ab",
    invertible = TRUE,
    lossy = TRUE
  )
  expect_error(
    ngeo_transform_path(lossy, "B", "A", allow_inverse = TRUE),
    class = "ngeo_error_transform_path"
  )
  forward <- ngeo_transform_path(lossy, "A", "B")
  expect_false(forward$applicable)
})

test_that("non-affine and mutated graph edges cannot be applied", {
  fixture <- space_graph_fixture()
  warp <- ngeo_transform(
    fixture$spaces$A,
    fixture$spaces$B,
    "warp",
    method = "supplied warp",
    interpolation = "linear",
    parameters = list(reference = "warp-field.nii.gz")
  )
  graph <- ngeo_transform_graph(
    fixture$registry,
    warp,
    edge_ids = "warp",
    invertible = FALSE,
    lossy = TRUE
  )
  path <- ngeo_transform_path(graph, "A", "B")
  expect_false(path$applicable)

  affine_graph <- ngeo_transform_graph(
    fixture$registry,
    fixture$transforms$ab,
    edge_ids = "ab"
  )
  affine_graph$transforms$ab$parameters$matrix[1L, 4L] <- 99
  expect_error(
    ngeo_validate_transform_graph(affine_graph),
    class = "ngeo_error_transform_graph_mutation"
  )
})
