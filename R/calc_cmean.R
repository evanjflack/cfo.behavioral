#' Calculate conditional sample means and standard errors
#'
#' calc_cmean() is a function that calculates sample means (plus
#'  standard errors and confidence intervals) by group.
#'
#' @param data a data.frame or data.table
#' @param y a character vector of column names of data to calculate the group mean
#'  of
#' @param x a character vector of columns in data to group by
#' @param se logical, if TRUE output will calculate standard error and confidence
#'  intervals of mean alng with  number of observations of conditional sample
#' mean
#' @param alpha a real number between 0 and 1, to use as the alpha level in
#'  confidence interval construction (default is .05)
#'
#' @importFrom data.table data.table
#' 
#' @examples
#' library(data.table)
#' data <- data.table(x1 = sample(1:5, 1000, replace = TRUE),
#'                    x2 = sample(1:2, 1000, replace = TRUE))
#' data[, y := 2*x1 + 4*x2 + rnorm(mean = 0, sd = 2, n = 1000)]
#'
#' calc_cmean(data, y = c("y"), x = c("x1", "x2"))
#'
#' @return The data.frame/data.table of conditional sample means
#'
#' @export
calc_cmean <- function(data, y, x, se = FALSE, alpha = .05) {
  if (se == TRUE) {
    ci_mult <- stats::qnorm(1 - alpha / 2)
    DT_cmean <- data[, lapply(.SD, mean_se),  by = x, .SDcols = y]
    DT_cmean <- DT_cmean[, measure := rep(c("mean", "se"),
                                          nrow(DT_cmean) / 2)]
    DT_cmean <- data.table::melt(DT_cmean, id.var = c(x, "measure"))
    DT_cmean <- data.table::dcast(DT_cmean,
                                  stats::as.formula(paste(paste(x,
                                                                collapse  = 
                                                                  " + "),
                                                          "+ variable ~ measure")),
                                  value.var = "value")
    DT_cmean[, `:=`(lb = mean - ci_mult * se, ub = mean + ci_mult * se)]
  } else {
    DT_cmean <- data[, lapply(.SD, mean), by = x, .SDcols = y]
    DT_cmean <- data.table::melt(DT_cmean, id.var = x, value.name = "mean")
  }
  return(DT_cmean)
}

mean_se <- function(x) {
  c(mean = mean(x, na.rm = TRUE),
    se = stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
}