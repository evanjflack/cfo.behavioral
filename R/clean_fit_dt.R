#' Format fit data.table
#' 
#' Format the estimates, standard errors, and significance stars for estimate
#'  tables to be printed in Latex.
#' 
#' @param dt data.table 
#' @param id_vars character vector, name(s) of the idifiying featues of the 
#'  estimate
#' @param est_var character, name of the estimate column
#' @param se_var character, name pf the standard error coulumn 
#' @param p_var character, name of the  value column
#' @param dig integer, number of significant features to round estimate and 
#'  standard error
#'  
#' @return data.table ready to be printed with xtable
#' 
#' @export
clean_fit_dt <- function(dt, id_vars, est_var = "estimate", se_var = "std.error", 
                     p_var = "p.value", dig = 3) {
  dt_fit <- dt %>% 
    .[, `:=`(est = get(est_var), se = get(se_var), p_val = get(p_var))] %>% 
    .[, c(id_vars, "est", "se", "p_val"), with = F] %>% 
    .[, lapply(.SD, signif, digits = dig), by = id_vars] %>%
    .[, stars1 := ifelse(p_val <= .01, "***", ifelse(p_val <= .05, "**",
                                                    ifelse(p_val <= .1,
                                                           "*", "")))] %>%
    .[, est := paste0(est, stars1)] %>%
    .[, est_se := paste0("\\begin{tabular}{@{}c@{}}", est,
                         "\\\\ (", se,  ")\\end{tabular}")] %>%
    .[is.na(se), est_se := "-"] %>%
    .[, c(id_vars, "est_se"), with = F]
  
  return(dt_fit)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("est", "est_se", "p_val", "stars"))
}