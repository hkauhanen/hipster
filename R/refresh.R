#' Package Refresh
#'
#' "Refresh" a package by unloading its namespace and immediately re-\code{library}ing
#' the package. This is useful if an updated version of the package has been
#' installed by another process on the machine in the meantime.
#'
#' @param pkg Package name (character vector)
#'
#' @export
refresh <- function(pkg) {
  unloadNamespace(pkg)
  requireNamespace(pkg)
}
