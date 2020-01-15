#' Get the Element(s) of a Vector Closest to a Reference Value
#'
#' Return the \emph{k} elements of a vector closest (in terms of Euclidean distance) to a reference value, as well as those elements' indices in the vector.
#'
#' @param x Reference value
#' @param vec Vector
#' @param k How many closest neighbours to return
#' @return A data frame with the following columns, sorted by increasing distance:
#' \describe{
#' \item{\code{index}}{Index of element (i.e. \code{value} is the same as \code{vec[index]})}
#' \item{\code{value}}{Element}
#' \item{\code{dist}}{Euclidean distance of element to reference value}
#' }
#'
#' @export
closest_in_vector <- function(x,
                              vec,
                              k = 1) {
  df <- data.frame(index=NA, value=vec, dist=NA)
  df$index <- 1:length(vec)
  df$dist <- abs(x - vec)
  df <- df[order(df$dist, decreasing=FALSE), ]
  if (k > nrow(df)) {
    warning("k greater than the length of vector")
    k <- nrow(df)
  }
  df[1:k, ]
}
