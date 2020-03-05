#' Creat New Enrollee Mortality Outcomes
#' 
#' create_mortality_outcomes() is a function that makes indicators for both
#'  cumulative mortality and mortality hazard for the new enrollee sample.
#' 
#' @param DT_deaths data.table with bene_id, and from_days (days from specified
#'  starting point until death)
#' @param DT_id data table with bene_id
#' @param start_day integer (default = 0), when to start counting outcomes from
#' @param int integer (default = 30), length of mortality intervals
#' @param int_num interger (default = 12), number of intervals to iterate 
#'  through
#'
#' @return a data.table with the following colums: 
#' \item{bene_id}{}
#' \item{mort_X}{Indicator for mortality from start day to X days, e.g. mort_30}
#' \item{mort_X_Y}{Indicator for mortality between X and Y days, e.g. 
#'                 mort_30_60}
#'  
#' @export
create_mortality_outcomes <- function(DT_deaths, DT_id = "pde_benes", 
                                      start_day = 0, int = 30, 
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
    fill_in_zeros(DT_id, "bene_id") %>% 
    .[from_days == 0 & get(paste0("mort_", int)) == 0, from_days := 9999]
  
  return(mortality_outcomes)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("bene_id", "from_days"))
}