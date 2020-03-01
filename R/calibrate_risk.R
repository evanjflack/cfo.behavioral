#' Calibrate Acute Event Prediction
#' 
#' @param DT data.table
#' @param keep_vars character vector, names of subsetting variables
#' @param cont_risk_var character, name of contrinuous risk prediction variable
#' @param n_quant interger, number of quantiles to break cont_risk_var in to
#' @param event_var character, name of the realized event variable (e.g. "ami")
#' @param treated_var character, name of the binary treatment indicator 
#'  variable
#' 
#' @return data.table with realized event rate by treatment status and predicted
#'  risk quantile
#' 
#' @export
calibrate_risk <- function(DT, keep_vars, cont_risk_var, n_quant, event_var, 
                           treated_var) {
  
  DT <- prep_fs_elasticity_data(DT, 
                                keep_vars = keep_vars, 
                                cont_risk_var = cont_risk_var, 
                                n_quant = n_quant)
  
  dtp_calib <- calc_cmean(DT, y = event_var, x = c("risk_cut_abs", treated_var), 
                          se = T) %>% 
    setnames(c("risk_cut_abs", treated_var), c("risk", "treated")) %>% 
    .[, treated := factor(treated, levels = c(0, 1), 
                          labels = c("Untreated", "Treated"))]
  
  return(dtp_calib)
}

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("treated"))
}