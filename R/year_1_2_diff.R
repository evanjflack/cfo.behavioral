#' Estimate year 1 to 2 usage differencs
#' 
#' Estimate the first stage of likelihood that people with a year 2 usage
#'  amount had a specific usage amount in year 1
#' 
#' @param DT data.table
#' @param grid data.table with combinations of jan/dec amounts
#' @param year1_var character, name of the column of the year 1 usage amount
#' @param year2_var character, name of the column of the year 2 usage amount
#' 
#' @return data.table with first stage estimates for each combination in grid
#' 
#' @export
estimate_year_1_2_diff <- function(DT, grid, year1_var, year2_var) {
  dt_fit <- data.table()
  for (i in 1:nrow(grid)) {
    print(i)
    grid_y1 <- unlist(grid[, year1_var, with = F])[i]
    grid_y2 <- unlist(grid[, year2_var, with = F])[i]
    
    DT_fit <- DT %>%  
      .[, y1 := get(year1_var)] %>%
      .[, y2 := get(year2_var)] %>%
      .[y2 == grid_y2, ] %>%
      .[, y := ifelse(y1 == grid_y1, 1, 0)]
    
    fit <- lm(y ~ first_mo:factor(pred_cut1) + factor(pred_cut1), 
              data = DT_fit)
    
    dt_fit1 <- fit_to_dt(fit, "first_mo", c("pred_cut1", "high_risk_abs")) %>% 
      .[, paste0(year1_var) := grid_y1] %>% 
      .[, paste0(year2_var) := grid_y2]
    
    dt_fit %<>% rbind(dt_fit1)
    
  }
  return(dt_fit)
}

#' Estimate pooled year 1 to 2 usage differencs
#' 
#' Estimate the first stage of likelihood that people with a year 2 usage
#'  amount had a specific usage amount in year 1
#' 
#' @param DT data.table
#' @param diff_var character, name of the column with the year 1 - year 2 amount
#' @param year2_var character, name of the column of the year 2 usage amount
#' @param u_diff_var numeric vecotr with all unique amounts of the year 1/2 
#'  difference
#' @param exclude logical (default = TRUE), if TRUE, then excludes people with 
#'  year 2 amoutns that are not compatible with the year 1 difference we are 
#'  trying to estimate
#' 
#' @return data.table with first stage estimates for each value in u_diff_var
#' 
#' @export
estimate_year_1_2_diff_pooled <- function(DT, diff_var, year2_var, u_diff_var, 
                                         exclude = TRUE) {
  dt_fit <- data.table()
  for (i in u_diff_var) {
    print(i)
    
    if (exclude == TRUE) {
      DT_fit <- DT[get(year2_var) >= -i, ]
    } else {
      DT_fit <- DT
    }
    DT_fit %<>% 
      .[, y := ifelse(get(diff_var) == i, 1, 0)]
    fit <- lm(y ~ first_mo:factor(pred_cut1) + factor(pred_cut1), 
              data = DT_fit)
    
    dt_fit1 <- fit_to_dt(fit, "first_mo", c("pred_cut1")) %>% 
      .[, paste0(diff_var) := i]
    
    dt_fit %<>% rbind(dt_fit1)
  }
  return(dt_fit)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("y1", "y2"))
}