#' \code{} package
#'
#' @docType package
#' 
#' @name polypharmacy
#' 
#' @description 
#' R package for the work in progress "Behavioral Hazard and Patient Decision Making" by Chanddra, Flack, and Obermeyer.
#' 
#' @importFrom data.table `:=` 
#' @importFrom magrittr `%<>%` `%>%`
#' 
"_PACKAGE"

# quiets concerns of R CMD check re: the .'s that appear in pipelines and the 
# .SD that occur in data.table
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".SD"))
}