#' Estimate reduced form models
#' 
#' Iterate estimation of the mortality and utilization reduced forms over 
#'  multiple specificiations.
#' 
#' @param DT a data.table with relevant model/sample option variables. Must have 
#'  all the "keep" variables in grid, as well as the variables for all the 
#'  options specified in grid. For example, if outcome = "mort" and 
#'  outcome_period = 30, DT must have a column named "mort_30".
#' @param grid a data.table with estimation options (see below)
#' @param max_cores integer, maximum number of parallels cores allowed to run at
#'  once
#'  
#' @return data.table with the estimation options (from grid), RF estimates, 
#'  SEs, p-values, CIs, etc...
#'  
#' @importFrom estimatr lm_robust
#' @importFrom data.table as.data.table setnames copy
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom stats as.formula
#'  
#' @export
iterate_rf <- function(DT, grid, max_cores) {
  cores <- min(max_cores, nrow(grid))
  registerDoParallel(cores = cores)
  dtp  <- foreach(instrument = grid$instrument, 
                  deg = grid$deg, 
                  outcome = grid$outcome,
                  outcome_period = grid$outcome_period,
                  x_var = grid$x_var, 
                  initial_days = grid$initial_days,
                  keep_age = grid$keep_age, 
                  keep_jan = grid$keep_jan, 
                  keep_join_month = grid$keep_join_month, 
                  keep_same = grid$keep_same, 
                  time_interact = grid$time_interact, 
                  se_type = grid$se_type, 
                  risk_type = grid$risk_type, 
                  risk_cut = grid$risk_cut, 
                  inc_var = grid$inc_var, 
                  inc_cut = grid$inc_cut,
                  max_inst = grid$max_inst,
                  .combine = "rbind",
                  .multicombine = TRUE) %dopar% 
    {
      
      # Prep data table
      DT_fit <- prep_2sls_data(DT, initial_days,  outcome, 
                               outcome_period, x_var, instrument, keep_age, keep_jan, 
                               keep_join_month, keep_same, risk_type, risk_cut, 
                               inc_var, inc_cut, max_inst)
      
      # Make forumla
      form <- stats::formula(outcome ~ instrument:factor(pred_cut1) + factor(pred_cut1))
      
      # Fits reduced form
      fit_iv <- lm_robust(form, data = DT_fit, se_type = se_type)
      
      # Format output
      dtp1 <- tidy(fit_iv) %>% 
        as.data.table() %>% 
        .[grepl("x1", term)] %>%
        setnames(c("conf.low", "conf.high"), 
                 c("lb", "ub")) %>% 
        .[, .(term, estimate, std.error, statistic, p.value, lb, ub)] %>% 
        .[, obs := nrow(DT_fit)]
    }
  stopImplicitCluster()
  
  # Append the estimation options as some lead to multiple estimates
  grid %<>%
    as.data.table() %>% 
    .[, risk_inc := ifelse(risk_cut != 0 & inc_cut == 0, 1, 
                           ifelse(inc_cut != 0 & risk_cut == 0, 2, 
                                  ifelse(risk_cut != 0 & inc_cut != 0, 3, 0)))] %>% 
    .[, ord := seq(1, .N)]
  
  risk_inc_grid <- grid %>% 
    .[risk_inc %in% c(1, 2)]
  
  both_grid <- grid %>% 
    .[risk_inc == 3, ] %>% 
    rbind(., ., .)
  
  grid %<>% 
    rbind(., grid, grid) %>% 
    .[order(ord), ]
  
  dtp %<>% 
    cbind(grid, .)
  
  return(dtp)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("cut_int", "deg", "instrument", "iv_est", 
                           "iv_se", "keep_age", "keep_jan", "keep_join_month", 
                           "keep_same", "keep_join_month", "lb", "outcome", 
                           "outcome_period", "x_var", "p.value", "pred_cut", 
                           "pred_type", "se_type", "statistic", "time_interact",
                           "ub", "obs", "first_mo", "pre_mort", "spend_pred", "x1", 
                           "year_cut", "rfrnc_yr", "risk", "high_risk", 
                           "risk_cut", "risk_type", "ord", "inc", "high_inc", 
                           "inc_var", "risk_inc", "max_inst"))
}
