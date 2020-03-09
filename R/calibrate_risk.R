#' Calibrate Acute Event Prediction
#' 
#' calibrate_risk() is a function that calculates the actual event rate of acute
#'  events by predicted risk quantile and initial period treatment status.
#' 
#' @param DT data.table with keep_vars, cont_risk_var, event_var, and 
#'  treated_var
#' @param keep_vars character vector, names of subsetting variables (e.g. 
#'  "keep_jan")
#' @param cont_risk_var character, name of contrinuous risk prediction variable
#'  (e.g. "comp_card_untreated_statin")
#' @param n_quant interger, number of quantiles to break cont_risk_var in to
#' @param event_var character, name of the realized event variable (e.g. "ami")
#' @param treated_var character, name of the binary treatment indicator 
#'  variable
#' 
#' @return data.table with following columns: 
#' \item{treated}{indicator for initial period treatment} 
#' \item{risk}{quantile of predicted risk}
#' \item{mean}{realized event rate}
#' 
#' @export
calibrate_risk <- function(DT, keep_vars, cont_risk_var, n_quant, event_var, 
                           treated_var) {
  
  # Subset DT by keep_vars, and make risk quantiles
  DT <- prep_fs_elasticity_data(DT, 
                                keep_vars = keep_vars, 
                                cont_risk_var = cont_risk_var, 
                                n_quant = n_quant)
  
  # Calculate event rate by quantile and treatement status
  dtp_calib <- calc_cmean(DT, y = event_var, x = c("risk_cut_abs", treated_var), 
                          se = T) %>% 
    setnames(c("risk_cut_abs", treated_var), c("risk", "treated")) %>% 
    .[, treated := factor(treated, levels = c(0, 1), 
                          labels = c("Untreated", "Treated"))]
  
  return(dtp_calib)
}

# Dealw tih R CMD Check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("treated"))
}