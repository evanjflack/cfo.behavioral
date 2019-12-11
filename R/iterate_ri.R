#' Iterate Randomization Inference
#'
#' iterate_ri() is a function that performs randomization inference on 
#'  instrumental variable models, and iterates over multiple sample
#'  specifications.
#'
#' @param DT a data.table, 
#' @param grid a data.table, 
#' @param max_cores an integer, 
#' @param controls, a character vector
#' 
#' @return a data.table
#' 
#' @importFrom foreach %dopar%
#' @importFrom dplyr %>% 
#' @importFrom magrittr %<>% 
#' @importFrom data.table := 
#' 
#' 
#' @export
iterate_ri <- function(DT, grid, max_cores, controls) {
  cores <- min(max_cores, nrow(grid))
  registerDoParallel(cores = cores)
  dtp  <- foreach::foreach(instrument = grid$instrument, 
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
                  .combine = "rbind",
                  .multicombine = TRUE) %dopar% {
                    DT_fit <- create_vars(DT, pred_type, cut_int, outcome, 
                                          outcome_period, instrument)
                    
                    DT_fit <- subset_data(DT_fit, keep_age, keep_jan, 
                                          keep_join_month, keep_same)
                    
                    form <- make_formula(controls, deg, time_interact, 
                                         keep_same)
                    
                    data.table(coeff = estimate_null(form, DT_fit))
                  }
  stopImplicitCluster()
  dtp <- cbind(grid, dtp)
  return(dtp)
}

make_formula <- function(controls = c("race", "sex"), deg = 1, 
                         time_interact = "rfrnc_yr", keep_same = 0) {
  paste_factor <- function(x) {
    paste0("factor(", x, ")")
  }
  
  # If using people that join before/after birth month, include control for
  # number of months procrastinated
  if (keep_same == 0) {
    contrcols <- c(controls, "rel_join_month")
  }
  
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
    stats::as.formula()
  return(form)
}

create_vars <- function(DT, pred_type, cut_int, outcome, outcome_period, 
                        instrument) {
  # Prep data table
  DT_fit <- copy(DT) %>%
    .[, pred_cut := bin_variable(get(paste0(pred_type, "_pred")), 
                                 0, 10000, cut_int)] %>% 
    .[, outcome := get(paste(outcome, outcome_period, sep = "_"))] %>% 
    .[, instrument := get(instrument)]
  return(DT_fit)
}

# Fxn: subset_data
subset_data <- function(DT, keep_age, keep_jan, keep_join_month, keep_same) {
  crit_grid <- c("keep_age" = keep_age, 
                 "keep_jan" = keep_jan, 
                 "keep_join_month" = keep_join_month, 
                 "keep_same" = keep_same)
  crit <- names(crit_grid)[(crit_grid == 1)]
  for (k in crit) {
    DT %<>%
      .[get(k) == 1, ]
  }
  return(DT)
}


estimate_null <- function(form, DT) {
  DT_null <- DT[, instrument := sample(DT$instrument, nrow(DT))]
  fit_iv <- estimatr::iv_robust(form, data = DT_null, se_type = "classical")
  coeff <- fit_iv$coefficients[2]
  return(coeff)
}