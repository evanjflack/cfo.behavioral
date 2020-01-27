#' Convert logit prdictions to probabilities
#' 
#' @param logit vector of logit preditions
#' 
#' @return vector of predicted probabilities
#' 
#' @export
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}