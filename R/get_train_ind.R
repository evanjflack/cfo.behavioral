#' Get Indicies of Training Sample
#' 
#' @param ids data.table with keep variables
#' @param population character, name of population 
#' @param pred_cat character, name of prediction category
#' 
#' @return integer vector of the row indicies of observations in the training 
#' set
#' 
#' @export
get_train_ind <- function(ids, population, pred_cat) {
  keep_var1 <- paste("keep", population, sep = "_")
  keep_var2 <- paste("keep", pred_cat, sep = "_")
  keep_train <- which(ids[, keep_var1, with = F] == 1 & 
                        ids[, keep_var2, with = F] == 1 & 
                        ids$set == "train")
  return(keep_train)
}