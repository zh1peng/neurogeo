if (dir.exists(".r-lib")) {
  .libPaths(c(.libPaths(), normalizePath(".r-lib")))
}
pkgdown::build_reference(pkg = ".")
pkgdown::build_articles(pkg = ".")
pkgdown::build_news(pkg = ".")
