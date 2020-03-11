#' Calculate New Enrollee Part D Spending by Month
#' 
#' calc_spend_by_month() is a function that calculates both the total/OOP
#' spending by calendar month, and cumulative amounts for each within a year.
#' 
#' @param DT data.table with the columns bene_id, year (relative to enrollment 
#'  year, starting at 1), month, cost, and oop_cost
#' @param DT_id data.table with the column bene_id
#' 
#' @return data.table with 7 columns: 
#' \item{bene_id}{}
#' \item{year}{}
#' \item{month}{}
#' \item{cost}{Total spending in month}
#' \item{cum_cost}{Total spending though month in year}
#' \item{oop_cost}{OOP spending in month}
#' \item{cum_oop_cost}{Total OOP spending through month in year}
#' 
#' @importFrom data.table dcast melt setnames
#' @importFrom stats ave
#' @importFrom stringr str_split_fixed
#' @importFrom magrittr not
#' 
#' @export
calc_spending_by_month <- function(DT, DT_id) {
  
  # Sum cost/oop_cost by month, reshape to wide, and fill in 0s
  cost_mo_wide <- DT %>% 
    .[, .(cost = sum(cost), oop_cost = sum(oop_cost)), 
      by = .(bene_id, year, month)] %>% 
    dcast(bene_id ~ year + month, value.var = c("cost", "oop_cost"),  
          fun.aggregate = sum) %>% 
    fill_in_zeros(DT_id[, .(bene_id)], "bene_id")
  
  # Reshape back to long
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
  
  # Format year/month in long data
  year_mo <- cost_mo_long$month1 %>% 
    gsub("cost_", "", .) %>% 
    str_split_fixed(., "_", 2) %>% 
    as.data.table() %>% 
    setnames(names(.), c("year", "month")) %>% 
    .[, lapply(.SD, as.numeric)]
  
  # Combine year/month, cost and oop_cost data
  cost_mo_long %<>% cbind(year_mo) %>% 
    .[, .(bene_id, year, month, cost)] %>% 
    cbind(oop_cost_mo_long[, .(oop_cost)]) %>% 
    .[order(bene_id, year, month), ]
  
  # Cumulative sum of cost/oop cost within year
  cost_mo_long %<>% 
    .[, cum_cost := ave(cost, bene_id, year, FUN = cumsum)] %>% 
    .[, cum_oop_cost := ave(oop_cost, bene_id, year, FUN = cumsum)]
  
  return(cost_mo_long)
}

#' Calculate New Enrollee Part D Spending by Month and Brand/Generic Type
#' 
#' @inheritParams calc_spending_by_month
#' 
#' @return data.table with spending/cumulative spending (total/oop) by year, 
#'  month, and brand type. 0 - generic, 1 - brand, 2 - unknown
#'  
#' @export
calc_spending_by_month_brand <- function(DT, DT_id) {
  
  # Sum cost/oop_cost by month, reshape to wide, and fill in 0s
  cost_mo_wide <- DT %>% 
    .[, .(cost = sum(cost), oop_cost = sum(oop_cost)), 
      by = .(bene_id, year, month, brand)] %>% 
    dcast(bene_id ~ year + month + brand, value.var = c("cost", "oop_cost"),  
          fun.aggregate = sum) %>% 
    fill_in_zeros(DT_id[, .(bene_id)], "bene_id")
  
  # Reshape back to long
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
  
  # Format year/month in long data
  year_mo_brand <- cost_mo_long$month1 %>% 
    gsub("cost_", "", .) %>% 
    str_split_fixed(., "_", 3) %>% 
    as.data.table() %>% 
    setnames(names(.), c("year", "month", "brand")) %>% 
    .[, lapply(.SD, as.numeric)]
  
  # Combine year/month, cost and oop_cost data
  cost_mo_long %<>% cbind(year_mo_brand) %>% 
    .[, .(bene_id, year, month, brand, cost)] %>% 
    cbind(oop_cost_mo_long[, .(oop_cost)]) %>% 
    .[order(bene_id, year, month, brand), ]
  
  # Cumulative sum of cost/oop cost within year
  cost_mo_long %<>% 
    .[, cum_cost := ave(cost, bene_id, year, brand, FUN = cumsum)] %>% 
    .[, cum_oop_cost := ave(oop_cost, bene_id, year, brand, FUN = cumsum)]
  
  return(cost_mo_long)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("bene_id", "month", "year", "cost", "cum_cost", 
                           "oop_cost", "cum_oop_cost", "brand"))
}