#' Package Refresh
#'
#' "Refresh" a package by unloading its namespace and immediately re-requiring
#' that namespace. This is useful if an updated version of the package has been
#' installed by another process on the machine in the meantime, to avoid
#' having to restart the R session.
#'
#' @param pkg Package name (character vector)
#'
#' @export
refresh <- function(pkg) {
  unloadNamespace(pkg)
  requireNamespace(pkg)
}
