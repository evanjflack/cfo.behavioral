#' Calculate New Enrollee Part D Spending by Month
#' 
#' @param DT data.table with the columns bene_id, year, month, cost, and 
#'  oop_cost
#' @param DT_id, data.table with the column bene_id
#' 
#' @return data.table with 7 columns: bene_id, year, month, cost (spending in 
#'  that month), cum_cost (total spending in that year, through that month), 
#'  oop_cost (oop spending in that month), and cum_oop_cost. 
#' 
#' @importFrom data.table dcast melt setnames
#' @importFrom stats ave
#' @importFrom stringr str_split_fixed
#' @importFrom magrittr not
#' 
#' @export
calc_spending_by_month <- function(DT, DT_id) {
  
  cost_mo_wide <- DT %>% 
    .[, .(cost = sum(cost), oop_cost = sum(oop_cost)), 
      by = .(bene_id, year, month)] %>% 
    dcast(bene_id ~ year + month, value.var = c("cost", "oop_cost"),  
          fun.aggregate = sum) %>% 
    fill_in_zeros(DT_id[, .(bene_id)], "bene_id")
  
  oop_cost_vars <- grep("oop", names(cost_mo_wide), value = T)
  cost_vars <- grep("cost_", names(cost_mo_wide), value = T) %>% 
    .[. %in% oop_cost_vars %>% not()]
  
  cost_mo_long <- cost_mo_wide %>%
    .[, c("bene_id", cost_vars), with = F] %>% 
    melt(id.var = "bene_id", value.name = "cost", 
         variable.name = "month1")
  
  oop_cost_mo_long <- cost_mo_wide %>%
    .[, c("bene_id", oop_cost_vars), with = F] %>% 
    melt(id.var = "bene_id", value.name = "oop_cost", 
         variable.name = "month1")
  
  year_mo <- cost_mo_long$month1 %>% 
    gsub("cost_", "", .) %>% 
    str_split_fixed(., "_", 2) %>% 
    as.data.table() %>% 
    setnames(names(.), c("year", "month")) %>% 
    .[, lapply(.SD, as.numeric)]
  
  cost_mo_long %<>% cbind(year_mo) %>% 
    .[, .(bene_id, year, month, cost)] %>% 
    cbind(oop_cost_mo_long[, .(oop_cost)]) %>% 
    .[order(bene_id, year, month), ]
  
  cost_mo_long %<>% 
    .[, cum_cost := ave(cost, bene_id, year, FUN = cumsum)] %>% 
    .[, cum_oop_cost := ave(oop_cost, bene_id, year, FUN = cumsum)]
  
  return(cost_mo_long)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("bene_id", "month", "year", "cost", "cum_cost", 
                           "oop_cost", "cum_oop_cost"))
}