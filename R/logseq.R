#' Logarithmic Sequences
#'
#' Generates a logarithmically spaced sequence.
#'
#' @param from Starting point of sequence
#' @param to Endpoint of sequence
#' @param length.out Desired length of sequence
#' @return A numeric vector
#'
#' @export
logseq <- function(from,
                   to,
                   length.out) {
  if (from <= 0 || to <= 0) {
    stop("Arguments 'from' and 'to' must be strictly positive")
  }
  exp(seq(from=log(from), to=log(to), length.out=length.out))
}
