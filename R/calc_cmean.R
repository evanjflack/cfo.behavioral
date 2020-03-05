#' Calculate conditional sample means and standard errors
#'
#' calc_cmean() is a function that calculates sample means (plus
#'  standard errors and confidence intervals) by group.
#'
#' @param DT a data.table
#' @param y a character vector of column names of data to calculate the group 
#'  mean of
#' @param x a character vector of columns in data to group by
#' @param se logical, if TRUE output will calculate standard error and 
#'  confidence intervals of mean alng with  number of observations of 
#'  conditional sample mean
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
#' @importFrom data.table is.data.table melt
#' @importFrom stats as.formula sd
#'
#' @export
calc_cmean <- function(DT, y, x, se = F){
  if (!is.data.table(DT)) stop("'DT' must be class data.table")
  if (se == T) {
    DT_cmean <- DT[, lapply(.SD, mean_se),  by = x, .SDcols = y] %>%
      .[, measure := rep(c("mean", "se", "obs"), nrow(.)/3)] %>%
      melt(id.var = c(x, "measure")) %>%
      dcast(as.formula(paste(paste(x, collapse  = " + "),
                             "+ variable ~ measure")),
            value.var = "value") %>%
      # 95% CI interval bounds
      .[, `:=`(lb = mean - 1.96*se, ub = mean + 1.96*se)]
  } else {
    DT_cmean <- DT[, lapply(.SD, mean), by = x, .SDcols = y] %>%
      melt(id.var = x, value.name = "mean")
  }
}

mean_se <- function(x) {
  c(mean = mean(x, na.rm = T), se = sd(x, na.rm = T)/sqrt(sum(!is.na(x))),
    obs = sum(!is.na(x)))
}

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("measure"))
} 

