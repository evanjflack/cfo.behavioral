#' Fill in NAs as 0
#' 
#' @param DT a data.table with NA values
#' @param DT_id a data.table with all unique IDs
#' @param id_name name of id column(s)
#' 
#' @return data.frame with added rows and 0s
#' 
#' @export 
fill_in_zeros <- function(DT, DT_id, id_name) {
  DT %<>%
    merge(
      DT_id[, id_name, with = F], by = id_name, all.y = T)
  DT[is.na(DT)] <- 0
  return(DT)
}
