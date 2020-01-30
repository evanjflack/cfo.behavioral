#' Creat Mortality Outcomes
#' 
#' @param DT_deaths data.table with bene_id, and from_days
#' @param start_day integer (default = 0), when to start counting outcomes from
#' @param int integer (default = 30), length of mortality intervals
#' @param int_num interger (default = 12), number of intervals to iterate 
#'  through
#'
#' @return a data.table with bene_id, and indicators for cumulative mortality 
#'  and mortality hazard.
#'  
#' @export
create_mortality_outcomes <- function(DT_deaths, start_day = 0, int = 30, 
                                      int_num = 12) {

  mortality_outcomes <- DT_deaths[, .(bene_id, from_days)] 
  outcome_periods <- seq(start_day + int, int*int_num, int)
  for (i in outcome_periods) {
    mortality_outcomes %<>% 
      .[, paste("mort_int", i - int, i, sep = "_") := 
          ifelse(from_days >= i - int & from_days < i , 1, 0)] %>%  
      .[, paste("mort", i, sep = "_") := 
          ifelse(from_days < i, 1, 0)]
  }
  
  # fill in 0s
  mort_vars <- grep("mort", names(mortality_outcomes), value = T)
  mortality_outcomes %<>% 
    .[, c("bene_id", "from_days", mort_vars), with = F] %>% 
    fill_in_zeros(pde_benes, "bene_id") %>% 
    .[from_days == 0 & get(paste0("mort_", int)) == 0, from_days := 9999]
  
  return(mortality_outcomes)
}