#' Factor to Numeric
#'
#' Turns a factor into a numeric vector.
#'
#' @param x Factor to be transformed
#'
#' @return Numeric vector
#'
#' @export
factor2numeric <- function(x) {
  if (is.factor(x)) {
    return(as.numeric(levels(x))[x])
  } else {
    return(x)
  }
}
