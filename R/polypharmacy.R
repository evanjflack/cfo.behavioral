#' \code{} package
#'
#' @docType package
#' @name polypharmacy
#' 
#' @importFrom data.table `:=` 
#' @importFrom magrittr `%<>%` `%>%`
#' 
NULL

# quiets concerns of R CMD check re: the .'s that appear in pipelines and the 
# .SD that occur in data.table
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".SD"))
}