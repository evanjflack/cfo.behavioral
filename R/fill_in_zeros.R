#' Add rows and fill 0s
#' 
#' When making features based on claims data, it is often the case that some 
#'  people in your sample will not have any claims. fill_in_zeros() adds these 
#'  individuals to the data.frame generated from the claims data and fills in a 
#'  value of 0 for these people with no claims.
#' 
#' @param DT a data.table generated from claims data
#' @param DT_id a data.table with all IDs in analytic sample
#' @param id_name name of id column(s) that are in both DT and DT_id
#' 
#' @return data.frame with added rows and 0s
#' 
#' @export 
fill_in_zeros <- function(DT, DT_id, id_name) {
  DT <- DT %>% 
    merge(
      DT_id[, id_name, with = F], by = id_name, all.y = T)
  DT[is.na(DT)] <- 0
  return(DT)
}
