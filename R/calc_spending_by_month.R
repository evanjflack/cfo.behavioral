#' Calculate New Enrollee Part D Spending by Month
#' 
#' @param DT data.table with the columns bene_id, year, month, and cost
#' @param DT_id, data.table with the column bene_id
#' 
#' @return data.table with 5 columns: bene_id, year, month, cost (spending in 
#'  that month), and cum_cost (total spending in that year, through that month)
#'  
#' @export
#' 
#' @importFrom data.table dcast melt setnames
#' @importFrom stats ave
#' @importFrom stringr str_split_fixed
calc_spending_by_month <- function(DT, DT_id) {
  
  cost_mo <- DT %>% 
    .[, .(cost = sum(cost)), by = .(bene_id, year, month)] %>% 
    dcast(bene_id ~ year + month, value.var = "cost", fun.aggregate = sum) %>% 
    fill_in_zeros(DT_id[, .(bene_id)], "bene_id") %>% 
    melt(id.var = "bene_id", value.name = "cost", variable.name = "month1")
  
  year_mo <- as.data.table(str_split_fixed(cost_mo$month1, "_", 2)) %>% 
    setnames(names(.), c("year", "month")) %>% 
    .[, lapply(.SD, as.numeric)]
  
  cost_mo %<>% cbind(year_mo) %>% 
    .[, .(bene_id, year, month, cost)] %>% 
    .[order(bene_id, year, month), ]
  
  cost_mo %<>% 
    .[, cum_cost := ave(cost, bene_id, year, FUN = cumsum)]
  
  return(cost_mo)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("bene_id", "month", "year", "cost", "cum_cost"))
}