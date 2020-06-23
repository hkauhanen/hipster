#' Seed Seed
#'
#' First, seeds the random number generator with \code{seed}. Then generates
#' a sequence of \code{n} random integers between 1 and
#' \code{.Machine$integer.max}. Finally, seeds the random number generator
#' with the \code{n}th (i.e. last) number in this sequence. The point of this
#' function is that it may be used to seed the RNG at the start of a job
#' that is run on a cluster, so that parallel jobs are using – if not
#' statistically independent random number streams – at least not the exact
#' same stream.
#'
#' @param n Number (e.g. id of parallel job)
#' @param seed Initial seed; this should be set so that results are replicable
#' @param ... Further parameters passed to \code{set.seed}
#' @return The final seed, i.e. the \code{n}th number in the sequence
#'
#' @importFrom stats runif
#' @export
seed_seed <- function(n,
                      seed,
                      ...) {
  set.seed(seed, ...)
  numbers <- round(runif(n=n, min=1, max=.Machine$integer.max))
  set.seed(numbers[n], ...)
  return(numbers[n])
}
