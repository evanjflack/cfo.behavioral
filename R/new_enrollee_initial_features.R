#' Make ATC Indicators
#' 
#' @param DT data.table
#' @param DT_id data.table
#' @param id_vars character vector
#' @param atc_ind data.table
#' 
#' @return data.table of atc indicators at the level of DT_id
#' 
#' @export
make_atc_indicators <- function(DT, DT_id, id_vars, atc_ind) {
  DT_atc <- DT %>% 
    merge(atc_ind, by = "lab_prod") %>% 
    .[, lab_prod := NULL] %>% 
    .[, lapply(.SD, max), by = id_vars] %>% 
    fill_in_zeros(DT_id, id_vars)
  return(DT_atc)
}

#' Create Initial ATC Indicators (New Enrollees)
#' 
#' @param DT data.table
#' @param DT_id data.table
#' @param atc_ind data.table
#' @param initial_days integer
#' 
#' @return data.table
#' 
#' @importFrom data.table data.table %between%
#' 
#' @export
indicate_initial_atc <- function(DT, DT_id, atc_ind, initial_days) {
  initial_DT <- DT %>%
    .[within_days %between% c(0, initial_days), ] %>%
    .[, .(bene_id, lab_prod)]
  
  initial_atc <- make_atc_indicators(initial_DT, DT_id, "bene_id", atc_ind)
  
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