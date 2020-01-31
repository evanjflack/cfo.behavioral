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
#'  \item{instrument: }{character, name of instumental variable, ex: "first_mo"}
#'  \item{deg: }{integer, degree polynomial of instrument to include, ex: 2}
#'  \item{cut_int: }{numeric, size of predicted spending bins, ex: 500}
#'  \item{pred_type: }{character, type of spending prediction to use, ex: "ensemble}
#'  \item{time_interact: }{character, name of the time variable to interact with 
#'    instrument and predicted spending, ex: "rfrnc_yr"}
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
#' @export
iterate_2sls <- function(DT, grid, max_cores) {
  cores <- min(config$max_cores, nrow(grid))
  registerDoParallel(cores = cores)
  dtp  <- foreach(instrument = grid$instrument, 
                  deg = grid$deg, 
                  outcome = grid$outcome,
                  outcome_period = grid$outcome_period, 
                  cut_int = grid$cut_int, 
                  pred_type = grid$pred_type, 
                  keep_age = grid$keep_age, 
                  keep_jan = grid$keep_jan, 
                  keep_join_month = grid$keep_join_month, 
                  keep_same = grid$keep_same, 
                  time_interact = grid$time_interact, 
                  se_type = grid$se_type, 
                  .combine = "rbind",
                  .multicombine = TRUE) %dopar% 
    {
      # Prep data table
      DT_fit <- prep_2sls_data(DT, pred_type, cut_int, outcome, outcome_period, 
                               instrument, keep_age, keep_jan, keep_join_month, 
                               keep_same)
      
      # Make forumla
      controls <- c("race", "sex")
      form <- make_2sls_formula(controls, time_interact, deg)
      
      # Fit 2SLS
      fit_iv <- iv_robust(form, data = DT_fit, se_type = se_type)
      
      # Format output
      tidy(fit_iv) %>% 
        .[2, ] %>% 
        as.data.table() %>% 
        setnames(c("estimate", "std.error", "conf.low", "conf.high"), 
                 c("iv_est", "iv_se", "lb", "ub")) %>% 
        .[, .(iv_est, iv_se, statistic, p.value, lb, ub)] %>% 
        .[, obs := nrow(DT_fit)]
    }
  stopImplicitCluster()
  
  # Append the estimation options
  dtp %<>%
    cbind(grid, .)
  
  return(dtp)
}

prep_2sls_data <- function(DT, pred_type, cut_int, outcome, outcome_period, 
                      instrument, keep_age, keep_jan, keep_join_month, 
                      keep_same) {
  DT_fit <- copy(DT) %>%
    .[, pred_cut := bin_variable(get(paste0(pred_type, "_pred")), 0, 10000, 
                                 cut_int)] %>% 
    .[, outcome := get(paste(outcome, outcome_period, sep = "_"))] %>% 
    .[, instrument := get(instrument)]
  
  crit_grid <- c("keep_age" = keep_age, 
                 "keep_jan" = keep_jan, 
                 "keep_join_month" = keep_join_month, 
                 "keep_same" = keep_same)
  crit <- names(crit_grid)[(crit_grid == 1)]
  for (k in crit) {
    DT_fit %<>%
      .[get(k) == 1, ]
  }
  return(DT_fit)
}

paste_factor <- function(x) {
  paste0("factor(", x, ")")
}

make_2sls_formula <- function(controls, time_interact, deg) {
  inst1 <- paste0("poly(instrument, ", deg, ")")
  interacts <- c("pred_cut", time_interact)
  interacts %<>% 
    lapply(paste_factor) %>% 
    unlist() %>% 
    paste(collapse = "*")
  controls %<>% 
    lapply(paste_factor) %>% 
    unlist() %>% 
    paste(collapse = " + ")
  ss_form <- paste0("outcome ~ cost100 + ", interacts, " + ", controls)
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
                           "outcome_period", "p.value", "pred_cut", 
                           "pred_type", 
                           "se_type", "statistic", "time_interact", "ub"))
}
