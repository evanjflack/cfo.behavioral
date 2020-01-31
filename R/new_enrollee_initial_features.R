#' Create Initial ATC Indicators (New Enrollees)
#' 
#' @param DT data.table
#' @param DT_id data.table
#' @param atc_ind data.table
#' @param initial_days integer
#' 
#' @return data.table
#' 
#' @export
indicate_initial_atc <- function(DT, DT_id, atc_ind, initial_days) {
  initial_DT <- DT %>%
    .[within_days %between% c(0, initial_days), ] %>%
    .[, .(bene_id, lab_prod)]
  
  initial_atc <- initial_DT %>% 
    merge(atc_ind, by = "lab_prod") %>% 
    .[, lab_prod := NULL] %>% 
    .[, lapply(.SD, max), by = bene_id] %>% 
    fill_in_zeros(DT_id[, .(bene_id)], "bene_id")
  
  return(initial_atc)
}

#' Calculate Initial Spending (New Enrollees)
#' 
#' @param DT data.table
#' @param DT_id data.table
#' @param initial_days integer vector
#' 
#' @return data.table
#' 
#' @export
calc_initial_spending <- function(DT, DT_id, initial_days) {
  initial_spending <- DT %>% 
    .[within_days >= 0, ]
  for (i in initial_days) {
    message(paste0("initial days = ", i))
    initial_spending[, paste0("initial_cost_", i) := ifelse(within_days <= i, 
                                                            cost, 0)]
  }
  initial_spending %<>%
    .[, lapply(.SD, sum), by = .(bene_id),
      .SDcols = grep("initial_cost_", names(.))] %>% 
    fill_in_zeros(DT_id[, .(bene_id)], "bene_id")
  
  return(initial_spending)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("bene_id", "lab_prod", "within_days"))
}