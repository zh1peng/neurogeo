if (dir.exists(".r-lib")) {
  .libPaths(c(normalizePath(".r-lib"), .libPaths()))
}
pkgdown::build_reference(pkg = ".")
pkgdown::build_articles(pkg = ".")
pkgdown::build_news(pkg = ".")
