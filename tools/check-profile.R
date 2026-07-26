repository <- Sys.getenv("NEUROGEO_CHECK_REPOSITORY")
if (nzchar(repository)) {
  options(repos = c(CRAN = repository))
}
