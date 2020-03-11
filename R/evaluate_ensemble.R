#' Evaluate ML Ensemble Model Predictions
#' 
#' @param dt_scores data.table with all variables in other parameters
#' @param id_vars character vector, names of id variables
#' @param set_var character, name of variable with sett identifiers
#' @param pred_vars character vector, names of prediction variables
#' 
#' @return data.table with mse/rsq by model and set
#' 
#' @export
evaluate_ensemble <- function(dt_scores, id_vars, set_var, pred_vars) {
  dt_mse_rsq <- dt_scores %>% 
    .[, c(id_vars, set_var, pred_vars, "y"), with = FALSE] %>% 
    melt(id.var = c(id_vars, set_var, "y"), 
         variable.name = "model", value.name = "pred") %>% 
    .[, resid := y - pred] %>% 
    .[, resid2 := resid^2] %>% 
    .[, mean_y := mean(y), by = c(set_var)] %>% 
    .[, var2 := (y - mean_y)^2] %>% 
    .[, .(mse = mean(resid2), rsq = 1 - (sum(resid2)/sum(var2))), 
      by = c("model", set_var)] %>% 
    dcast(model ~ set, value.var = c("mse", "rsq"))
  return(dt_mse_rsq)
}
# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("resid", "pred", "resid2", "mean_y", "var2"))
}
