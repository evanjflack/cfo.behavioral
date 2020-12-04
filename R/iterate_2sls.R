#' Estimate 2SLS Mortality Model
#' 
#' Iterate estimation of the 2SLS mortality model over multiple specificiations.
#' 
#' @param DT a data.table with relevant model/sample option variables. Must have 
#'  all the "keep" variables in grid, as well as the variables for all the 
#'  options specified in grid. For example, if outcome = "mort" and 
#'  outcome_period = 30, DT must have a column named "mort_30".
#' @param grid a data.table with estimation options (see below)
#' @param max_cores integer, maximum number of parallels cores allowed to run at
#'  once
#'  
#' @details grid
#' \itemize{
#'  \item{outcome: }{character, name of outcome variable, ex: "mort"}
#'  \item{outcome_period: }{integer, period of outcome, ex: 30}
#'  \item{x_var: }{character, name of the endogenous variable}
#'  \item{instrument: }{character, name of instumental variable, ex: "first_mo"}
#'  \item{deg: }{integer, degree polynomial of instrument to include, ex: 2}
#'  \item{cut_int: }{numeric, size of predicted spending bins, ex: 500}
#'  \item{pred_type: }{character, type of spending prediction to use, ex: "ensemble}
#'  \item{time_interact: }{character, name of the time variable to interact with
#'  #'    instrument and predicted spending, ex: "rfrnc_yr"}
#'  \item{initial_days: }{integer, number of initial_days used in prediction}
#'  \item{se_type: }{character, type of SEs to calculate, inherits from estimatr 
#'   iv_robust, ex: "stata"}
#'  \item{keep_age: }{boolean, if 1 then subsets sample to only those enrolling 
#'   at age 65}
#'  \item{keep_jan: }{boolean, if 1 then removes January enrollees from sample}
#'  \item{keep_join_month: }{boolean, if 1 then restricts sample to those 
#'   enrolling in initial enrollment period (IEP)}
#'  \item{keep_same: }{boolean, if 1 then restricts sample to those enrolling in
#'   theor birth month}
#' }
#'  
#' @return data.table with the estimation options (from grid), 2SLS estimates, 
#'  SEs, p-values, CIs, etc...
#'  
#' @importFrom estimatr iv_robust
#' @importFrom data.table as.data.table setnames copy
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom stats as.formula
#'  
#' @export
iterate_2sls <- function(DT, grid, max_cores) {
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
                  bin_type = grid$bin_type, 
                  exc_nc = grid$exc_nc,
                  .combine = "rbind",
                  .multicombine = TRUE) %dopar% 
    {
      
      # Prep data table
      DT_fit <- prep_2sls_data(DT, initial_days,  outcome, 
                               outcome_period, x_var, instrument, keep_age, keep_jan, 
                               keep_join_month, keep_same, risk_type, risk_cut, 
                               inc_var, inc_cut, max_inst, bin_type, exc_nc)
      
      # Make forumla
      controls <- c("race", "sex")
      form <- make_2sls_formula(controls, time_interact, deg, risk_cut, inc_cut)
      
      # Fit 2SLS
      fit_iv <- iv_robust(form, data = DT_fit, se_type = se_type)
      
      # Format output
      dtp1 <- tidy(fit_iv) %>% 
        as.data.table() %>% 
        .[grepl("x1", term)] %>%
        setnames(c("estimate", "std.error", "conf.low", "conf.high"), 
                 c("iv_est", "iv_se", "lb", "ub")) %>% 
        .[, .(term, iv_est, iv_se, statistic, p.value, lb, ub)] %>% 
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
    rbind(risk_inc_grid) %>% 
    rbind(both_grid) %>% 
    .[order(ord), ]
  
  dtp %<>% 
    cbind(grid, .)
  
  return(dtp)
}

prep_2sls_data <- function(DT, initial_days, outcome, outcome_period, x_var, 
                           instrument, keep_age, keep_jan, keep_join_month, 
                           keep_same, risk_type, risk_cut, inc_var, inc_cut, 
                           max_inst, bin_type, exc_nc) {
  
  DT_fit <- copy(DT) %>%
    .[, x1 := get(x_var)] %>% 
    .[, outcome := get(paste(outcome, outcome_period, sep = "_"))] %>% 
    .[, instrument := get(instrument)] %>% 
    .[, spend_pred := get(paste0("ensemble_pred_", initial_days))] %>%
    .[, year_cut := ifelse(rfrnc_yr <= 2010, "2007-2010", "2011-2012")]
  
  DT_fit %<>% 
    .[instrument <= max_inst, ]

  
  crit_grid <- c("keep_age" = keep_age, 
                 "keep_jan" = keep_jan, 
                 "keep_join_month" = keep_join_month, 
                 "keep_same" = keep_same)
  crit <- names(crit_grid)[(crit_grid == 1)]
  for (k in crit) {
    DT_fit %<>%
      .[get(k) == 1, ]
  }
  # remove those that dies in earlier periods 
  if (outcome_period > 1 & outcome == "mort") {
    DT_fit %<>% 
      .[, pre_mort := rowSums(.SD), .SDcols = paste0("mort_", seq(1, outcome_period - 1))] %>% 
      .[pre_mort == 0, ]
  }

  if (outcome == "fill_days") {
    DT_fit %<>% 
      .[outcome < 9999]
  }
  
  if (inc_cut != 0) {
    DT_fit %<>% 
      .[, inc := get(inc_var)] %>% 
      .[!is.na(inc)] %>% 
      .[, high_inc := ifelse(inc >= quantile(inc, inc_cut), 1, 0)]
  }
  
  if (initial_days > 30) {
    DT_fit %<>% 
      .[, pred_cut := cut(spend_pred, 
                          breaks = c(-Inf, quantile(spend_pred, c(seq(.1, .7, .1), seq(.71, .99, .01))), Inf), 
                          labels = c(seq(10, 70, 10), seq(71, 100, 1))), 
        by = first_mo]
  } else if (initial_days == 30) {
    DT_fit %<>% 
      .[, pred_cut := cut(spend_pred, 
                          breaks = c(-Inf, quantile(spend_pred, c(.2, seq(.4, .7, .1), seq(.71, .99, .01))), Inf), 
                          labels = c(20, seq(40, 70, 10), seq(71, 100, 1))), 
        by = first_mo]
  }

  DT_fit %<>% 
    .[, pred_cut1 := ifelse(spend_pred <= quantile(spend_pred, .7), 
                            1, ifelse(spend_pred <= 
                                        quantile(spend_pred, .97), 2, 3)), 
      by = first_mo]
  
  if (exc_nc == T) {
    DT_fit %<>% 
      .[!(pred_cut %in% c(94, 95, 96, 97, 98))]
  }
  
  if (bin_type == "three") {
    DT_fit %<>% 
      .[, pred_cut := pred_cut1]
  }
  
  if (risk_cut != 0) {
    DT_fit %<>% 
      .[, risk := get(paste0("ensemble_pred_", risk_type, "_", initial_days))] %>% 
      .[, high_risk := ifelse(risk >= quantile(risk, risk_cut), 1, 0), by = first_mo]
  }
  
  return(DT_fit)
}

paste_factor <- function(x) {
  paste0("factor(", x, ")")
}

controls <- c("race", "sex")
time_interact <- "rfrnc_yr"
deg <- 1
risk_cut <- .5
inc_cut <- .5

make_2sls_formula <- function(controls, time_interact, deg, risk_cut, inc_cut) {
  inst1 <- paste0("poly(instrument, ", deg, ")")
  interacts <- c("pred_cut", time_interact)
  interacts %<>% 
    lapply(paste_factor) %>% 
    unlist() %>% 
    paste(collapse = "*")
  if (risk_cut != 0) {
    interacts %<>% 
      paste0("*", "factor(high_risk)")
  }
  
  if (inc_cut != 0) {
    interacts %<>% 
      paste0("*", "factor(high_inc)")
  }
  
  interacts
  
  controls %<>% 
    lapply(paste_factor) %>% 
    unlist() %>% 
    paste(collapse = " + ")
  
  if (risk_cut == 0 & inc_cut == 0) {
    ss_form <- paste0("outcome ~ x1 + ", interacts, " + ", controls)
  } else if (risk_cut != 0 & inc_cut == 0){
    ss_form <- paste0("outcome ~ x1*factor(high_risk) + ", interacts, " + ", 
                      controls)
  } else if (risk_cut == 0 & inc_cut != 0) {
    ss_form <- paste0("outcome ~ x1*factor(high_inc) + ", interacts, " + ", 
                      controls)
  } else if (risk_cut != 0 & inc_cut != 0) {
    ss_form <- paste0("outcome ~ x1*factor(high_risk)*factor(high_inc) + ", 
                      interacts, " + ", controls)
  }
  
  inst_form <- paste(inst1, interacts, sep = "*") %>% 
    paste(controls, sep = " + ")
  form <- paste(ss_form, " | ", inst_form) %>% 
    as.formula()
  return(form)
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
                           "inc_var", "risk_inc", "max_inst", "pred_cut1"))
}
