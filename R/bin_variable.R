#' Bin a numeric variable
#' 
#' bin_variable() is a function that creates bins of either equal interval 
#'  length of equal size (quartiles)
#'  
#' @param x numeric vector, variable to binned
#' @param min numeric, where to start binning
#' @param max numeric, where to end binnig
#' @param int numeric, interval length of bins
#' @param quant, interger, number of quantiles
#'  
#' @return vector of bin labels
#'
#' @export
bin_variable <- function(x, min = NULL, max = NULL, int = NULL, quant = NULL) {
  # Bins of equal length
  if (is.null(quant)) {
    bins <- cut(x, 
                breaks = c(-Inf, seq(min, max, int), Inf), 
                labels = seq(min, max + int, int)) %>%
      as.character() %>%
      as.numeric()
  } else {
  # Bins of equal size (quantiles)
    cuts <- quantile(x, seq(0, 1, 1/quant))
    cuts[1] <- -Inf
    cuts[length(cuts)] <- Inf
    bins <- cut(x, breaks = cuts, labels = seq(1, quant)) %>%
      as.character() %>%
      as.numeric()
  }
  return(bins)
}