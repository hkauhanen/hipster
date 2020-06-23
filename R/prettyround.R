#' Pretty Print Rounded Figures
#'
#' Pretty print a rounded figure so that all trailing digits are shown,
#' including zeroes.
#'
#' @param x Number
#' @param digits Number of digits
#' @return Pretty-printed number. Note that this is of class character, not numeric.
#'
#' @export
prettyround <- function(x,
                        digits) {
  format(round(x, digits), nsmall = digits)
}
