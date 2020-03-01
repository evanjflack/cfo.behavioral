#' Make ATC Indicators
#' 
#' @param DT data.table with all elements of id_vars as well as lab_prod
#' @param DT_id data.table data.table with all elements of id_vars
#' @param id_vars character vector, names of variables to aggregate on
#' @param xwalk data.table with columns lab_prod as well as any indicators 
#'  (usually atc1-4 or rxcui)
#' 
#' @return data.table of atc indicators at the level of DT_id
#' 
#' @export
make_class_indicators <- function(DT, DT_id, id_vars, xwalk) {
  DT_atc <- DT %>% 
    merge(xwalk, by = "lab_prod") %>% 
    .[, lab_prod := NULL] %>% 
    .[, lapply(.SD, max), by = id_vars] %>% 
    fill_in_zeros(DT_id, id_vars)
  return(DT_atc)
}

#' Create Initial ATC Indicators (New Enrollees)
#' 
#' @inheritParams make_class_indicators
#' @param atc_ind data.table, lab_prod to atc indicator xwalk
#' @param initial_days integer, maximum number of days to use from enrollment
#' 
#' @return data.table with atc class indicators from from enrollment through 
#'  initial days
#' 
#' @importFrom data.table data.table %between%
#' 
#' @export
indicate_initial_atc <- function(DT, DT_id, id_vars, atc_ind, initial_days) {
  # Subset to claims within initial days
  initial_DT <- DT %>%
    .[within_days %between% c(0, initial_days), ] %>%
    .[, c(id_vars, "lab_prod"), with = FALSE]
  
  # Merge with xwalk and aggregate
  initial_atc <- make_class_indicators(DT = initial_DT, 
                                       DT_id = DT_id, 
                                       id_vars = id_vars, 
                                       xwalk = atc_ind)
  return(initial_atc)
}

#' Calculate Initial Spending (New Enrollees)
#' 
#' @inheritParams indicate_initial_atc
#' 
#' @return data.table
#' 
#' @export
calc_initial_spending <- function(DT, DT_id, id_vars, initial_days) {
  initial_spending <- DT %>% 
    .[within_days >= 0, ]
  for (i in initial_days) {
    message(paste0("initial days = ", i))
    initial_spending[, paste0("initial_cost_", i) := ifelse(within_days <= i, 
                                                            cost, 0)]
  }
  initial_spending %<>%
    .[, lapply(.SD, sum), by = id_vars,
      .SDcols = grep("initial_cost_", names(.))] %>% 
    fill_in_zeros(DT_id, id_vars)
  
  return(initial_spending)
}

#' Calculate Spending within a Class
#' 
#' @inheritParams make_class_indicators
#' @param cost_var character, name of the cost variable to use
#'  
#' @export
calc_spending_in_class <- function(DT, cost_var, DT_id, id_vars, xwalk) {
  DT_atc_cost <- DT %>% 
    merge(xwalk, by = "lab_prod")
  
  atc_vars <- grep("atc", names(DT_atc_cost), value = T)
  cost_vec <- unlist(DT_atc_cost[, cost_var, with = FALSE])
  names(cost_vec) <- NULL
  id_cols_DT <- DT_atc_cost[, id_vars, with = FALSE]
  
  DT_atc_cost %<>% 
    .[, lapply(.SD, function(x) x*cost_vec), .SDcols = atc_vars] %>% 
    cbind(id_cols_DT, .)
  
  DT_atc_cost %<>% 
    .[, lapply(.SD, sum), by = id_vars, .SD = atc_vars] %>% 
    fill_in_zeros(DT_id, id_vars)
  
  return(DT_atc_cost)
}

#' Calculate year 1/2 differences
#' 
#' @param DT data.table with id_vars, time_var, and year 1/2 utilization 
#'  variables
#' @param id_vars character vector, names of id variables
#' @param time_var character, name of time variable
#' @param year1_id character, value of time_var for year 1 observations
#' @param year2_id character, value of time_var for year 2 observations
#' 
#' @return data.table of year 1/2 differences
#' 
#' @export
calc_year_1_2_diff <- function(DT, id_vars, time_var, year1_id, year2_id) {
  
  cast_formula <- paste(c(id_vars, "variable"), collapse = " + ") %>% 
    paste("~", time_var) %>% 
    as.formula()
  
  cast_formula2 <- paste(id_vars, collapse = " + ") %>% 
    paste("~ variable")
  
  year_1_2_diff <- DT %>% 
    melt(id.var = c(id_vars, time_var)) %>% 
    dcast(cast_formula, value.var = "value") %>% 
    setnames(c(year1_id, year2_id), c("value_1", "value_2")) %>% 
    .[, diff := value_1 - value_2] %>% 
    dcast(cast_formula2, value.var = "diff") %>%
    setnames(names(.)[. %in% id_vars %>% not()], 
             paste0(names(.)[. %in% id_vars %>% not()], "_diff"))
  
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("bene_id", "lab_prod", "within_days", "..cost_var", 
                           "value_1", "value_2"))
}