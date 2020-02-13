#' Calculate normalized percentage
#' 
#' @param DT data.table
#' @param x1 character, the variable you want the normalized percentage of
#' @param x2 character vector, the variables you want to normalize by
#' 
#' @return data.table of normalized perctages
#' 
#' @export
calc_norm_perc <- function(DT, x1, x2) {
  dt_norm_perc <- DT %>% 
    .[, .(.N), by = c(x1, x2)] %>% 
    merge(DT[, .(.N), by = x2], by = x2) %>% 
    .[, norm_perc := N.x/N.y] %>% 
    .[, `:=`(N.x = NULL, N.y = NULL)]
  return(dt_norm_perc)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("N.x", "N.y", "norm_perc", ".N"))
}