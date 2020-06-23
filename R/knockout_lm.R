#' Knock Out Outliers in a Linear Regression
#'
#' Knock out outliers in a linear regression by recursively pruning those
#' data points that contribute the most to the regression error, operationalized
#' as the residual sum of squares at each knockout iteration.
#'
#' @param data Data frame
#' @param formula Regression formula
#' @param id.var Name of identifier variable (column)
#' @return Data frame with the following columns:
#' \describe{
#' \item{\code{iteration}}{Knockout iteration}
#' \item{\code{knockee}}{Data pointed knocked out at this iteration}
#' \item{\code{RSS_reduction}}{Reduction in residual sum of squares resulting from the knockout}
#' }
#'
#' @importFrom stats deviance lm
#' @export
knockout_lm <- function(data,
                        formula,
                        id.var) {
  no_iterations <- nrow(data) - 1
  out <- data.frame(iteration=1:no_iterations, knockee=NA, RSS_reduction=0)

  knock_one <- function(data,
                        formula) {
    mod <- lm(formula, data)
    RSS <- deviance(mod)
    RSSvec <- rep(NA, nrow(data))

    for (j in 1:nrow(data)) {
      reduced_data <- data[-j, ]
      reduced_mod <- lm(formula, reduced_data)
      RSSvec[j] <- deviance(reduced_mod)
    }

    RSS_diff <- RSS - RSSvec
    outlier <- which.max(RSS_diff)
    outlier_name <- data[[id.var]][outlier]

    list(data=data[-outlier, ], removed=outlier_name, improvement=max(RSS_diff))
  }

  for (i in 1:no_iterations) {
    result <- knock_one(data=data, formula=formula)
    data <- result$data
    out[i,]$knockee <- as.character(result$removed)
    out[i,]$RSS_reduction <- result$improvement
  }

  out
}
