#' Bootstrap a Data Frame
#'
#' Form a bootstrap sample from a data frame \code{data} by sampling
#' \code{nrow(data)} rows of the data frame with replacement.
#'
#' @param data Data frame
#' @return Data frame
#'
#' @export
bootstrap <- function(data) {
  data[sample(1:nrow(data), size=nrow(data), replace=TRUE), ]
}
