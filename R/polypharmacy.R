#' \code{} package
#'
#' @docType package
#' @name polypharmacy
#' 
#' @importFrom dplyr %>%
#' @importFrom data.table data.table is.data.table as.data.table `:=` copy setnames melt uniqueN setorderv dcast fread
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_split_fixed
#' @importFrom broom tidy
#' @importFrom stats lm as.formula formula sd quantile
#' @importFrom tictoc tic toc
#' @importFrom optparse parse_args OptionParser
#' @importFrom magrittr `%<>%`
#' @importFrom Matrix Matrix
#' 
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".SD", "estimate",
                           "measure", "month", "std.error", "subset_var",
                           "term", "variable", "y"))
} 