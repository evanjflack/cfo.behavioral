#' Bin a numeric variable
#' 
#' bin_variable() is a function that creates bins of wither equal interval 
#'  length of equal size.
#'  
#' @param x numeric vector to be binned
#' @param min where to start binning
#' @param max where to end binnig
#' @param int interval length of bins
#' @param quant number of quantiles
#'  
#' @return vector of bins
#'
#' @export
bin_variable <- function(x, min = NULL, max = NULL, int = NULL, quant = NULL) {
  if (is.null(quant)) {
    bins <- cut(x, c(-Inf, seq(min, max, int), Inf), seq(min, max + int, int))
    bins <- as.numeric(as.character(bins))
  } else {
    cuts <- stats::quantile(x, seq(0, 1, 1/quant))
    cuts[1] <- -Inf
    cuts[length(cuts)] <- Inf
    bins <- cut(x, cuts, seq(1, quant))
    bins <- as.numeric(as.character(bins))
  }
  return(bins)
}