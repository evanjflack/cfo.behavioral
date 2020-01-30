#' Estimate 2SLS Mortality Model
#' 
#' @param DT a data.table, 
#' @param grid a data.table, estimation grid
#' @param max_cores integer, maximum number of parallels cores allowed to run at
#'  once
#'  
#' @return a data.table with the estimation options, 2SLS estimates, SEs, 
#'  p-values, CIs, etc...
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
