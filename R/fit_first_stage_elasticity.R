#' Estimate Percentage Change
#' 
#' Estimate the first stage (y ~ first_mo), where y is a binary variable (e.g. 
#'  statin fill indiator) in terms of percentage change and bootstap the 
#'  standard errors
#'  
#' @param DT a data.table
#' @param y character vector, name of response variable(s) without months suffix
#' @param months character vector (default = "1_12"), months of y variable to 
#'  estimate. Should be in the form "YEAR_MONTH" where year is between 1 and 2 
#'  and month is between 1 and 12. 
#' @param x_main character (default = "first_mo"), name of instrument
#' @param x_int character vector, names of variables to interact instrument with
#' @param keep_vars character vector, column names for the binary keep variables
#' @param cont_risk_var character (default = NULL), name of the continuous 
#'  predicted risk variable
#' @param n_quant integer (default = 5), number of quntiles to break predicted 
#'  risk in to. High risk is defined as being in the top quantile. 
#' @param B integer, number of bootstrap samples to use when calculating 
#'  standard errors
#' 
#' @return a data.table of the first stage estimates (and their standard errors) 
#'  in percentage terms by the specified interactions
#'  
#' @export
fit_first_stage_perc_change <- function(DT, y,  months = "1_12", 
                                        x_main = "first_mo", x_int = NULL, 
                                        keep_vars, cont_risk_var = NULL, 
                                        n_quant = 5, B = 10) {
  
  # Prep Data
  DT <- prep_data(DT, keep_vars, cont_risk_var, n_quant)
  
  x_int <- c("pred_cut1", x_int)
  
  # Make Formula
  form <- make_formula(y = "l_mean", x_main, x_int)
  
  dt_est <- data.table()
  for (j in months) {
    print(j)
    # Estimates
    dt_est1 <- estimate_fs_perc_change(DT, y, month = j, x_main, x_int, form)
    # Standard errors
    dt_se1 <- bootstrap_fs_perc_change(DT, y, month = j, x_main, x_int, form, B)
    # labels
    dt_est1 %<>% 
      merge(dt_se1, by = x_int) %>% 
      .[, variable := paste0(y, "_", j)] %>% 
      .[, year := as.numeric(str_split_fixed(j, "_", 2)[1])] %>% 
      .[, month := as.numeric(str_split_fixed(j, "_", 2)[2])] %>% 
      .[, month1 := (year - 1)*12 + month]
    dt_est %<>% rbind(dt_est1)
  }
  # Confidence intervals
  dt_est %<>% 
    .[, `:=`(lb = estimate - 1.96*se, ub = estimate + 1.96*se)]
  
  # labels
  if ("high_risk_abs" %in% x_int) {
    dt_est %<>%
      .[, high_risk_abs := factor(high_risk_abs, labels = c("Low", "High"))]
  }
  
  return(dt_est)
}

#' Estimate First Stage
#' 
#' Estimate the first stage (y ~ first_mo), where y is a binary variable (e.g. 
#'  statin fill indiator) in gross terms
#'  
#' @param DT a data.table
#' @param y character vector, name of response variable(s) without months suffix
#' @param months character vector (default = "1_12"), months of y variable to 
#'  estimate. Should be in the form "YEAR_MONTH" where year is between 1 and 2 
#'  and month is between 1 and 12. 
#' @param x_main character (default = "first_mo"), name of instrument
#' @param x_int character vector, names of variables to interact instrument with
#' @param keep_vars character vector, column names for the binary keep variables
#' @param cont_risk_var character (default = NULL), name of the continuous 
#'  predicted risk variable
#' @param n_quant integer (default = 5), number of quntiles to break predicted 
#'  risk in to. High risk is defined as being in the top quantile.
#' 
#' @return a data.table of the first stage estimates (and their standard errors) 
#'  
#' @export
fit_first_stage_raw <- function(DT, y,  months = "1_12", 
                                x_main = "first_mo", x_int = NULL, 
                                keep_vars, cont_risk_var = NULL, 
                                n_quant = 5) {
  
  # Prep Data
  DT <- prep_data(DT, keep_vars, cont_risk_var, n_quant)
  
  x_int <- c("pred_cut1", x_int)
  
  # Make Formula
  form <- make_formula("y", x_main, x_int)
  
  dt_est <- data.table()
  for (j in months) {
    print(j)
    # Estimates
    dt_est1 <- estimate_fs_raw(DT, y, month = j, x_main, x_int, form)
    # Labels
    dt_est1 %<>% 
      .[, variable := paste0(y, "_", j)] %>% 
      .[, year := as.numeric(str_split_fixed(j, "_", 2)[1])] %>% 
      .[, month := as.numeric(str_split_fixed(j, "_", 2)[2])] %>% 
      .[, month1 := (year - 1)*12 + month]
    dt_est %<>% rbind(dt_est1)
  }
  
  # Labels
  if ("high_risk_abs" %in% x_int) {
    dt_est %<>%
      .[, high_risk_abs := factor(high_risk_abs, labels = c("Low", "High"))]
  }
  
  return(dt_est)
}

#' Prep Data
#' 
#' Prep data for first stage estimation by subsettig based on binary "keep" 
#'  variables and defining "high_risk" if requested
#'  
#' @param DT a data.table
#' @param keep_vars character vector, column names of binary keep variables
#' @param cont_risk_var character, name of continuous predicted risk variable
#' @param n_quant interger, numer of quantiles to cut the contrinuous risk 
#'  variable in to. High risk is defined as the top quantile.
#' 
#' @return subsetted data.table with new variables
prep_data <- function(DT, keep_vars, cont_risk_var = NULL, n_quant = 5) {
  # subet data based on specified "keep variables" 
  for (i in keep_vars) {
    DT <- DT[get(i) == 1, ]
  }
  if (!is.null(cont_risk_var)) {
    DT %<>% 
      .[, risk_cut_abs := bin_variable(get(cont_risk_var), quant = n_quant)] %>% 
      .[, high_risk_abs := ifelse(risk_cut_abs == n_quant, 1, 0)]
  }
  return(DT)
}

paste_factor <- function(x) {
  paste0("factor(", x, ")")
}

make_formula <- function(y, x_main, x_int) {
  int1 <- paste0(c(x_main, paste_factor(x_int)), collapse = ":")
  int2 <- paste0(paste_factor(x_int), collapse = "*")
  form <- as.formula(paste(y, "~", paste(int1, int2, sep = " + ")))
  return(form)
}

estimate_fs_perc_change <- function(DT, y, month, x_main, x_int, form) {
  if (month != "1_12") {
    DT %<>% 
      .[get(paste0("alive_", month)) == 1, ]
  }
  var <- y
  DT[, y := get(paste0(var, "_", month))]
  
  dtp <- calc_cmean(DT, paste0(var, "_", month), c(x_main, x_int), se = T) %>% 
    .[, l_mean := log(mean)]
  
  fit_perc <- lm(form, data = dtp, weights = obs)
  
  dt_est <- fit_to_dt(fit_perc, x_main, x_int) %>% 
    .[, c(x_int, "estimate"), with = F]
  return(dt_est)
}

bootstrap_fs_perc_change <- function(DT, y, month, x_main, x_int, form, B) {
  if (month != "1_12") {
    DT %<>% 
      .[get(paste0("alive_", month)) == 1, ]
  }
  dtp_boot <- data.table()
  var <- y
  for (i in 1:B) {
    print(i)
    samp <- sample(1:nrow(DT), nrow(DT), replace = T)
    DT1 <- DT[samp, ]
    dtp1 <- calc_cmean(DT1, paste0(var, "_", month), c(x_main, x_int), se = T) %>% 
      .[, l_mean := log(mean)]
    
    fit_perc1 <- lm(form, data = dtp1, weights = obs)
    dtp_boot1 <- fit_to_dt(fit_perc1, x_main, x_int) %>%
      .[, c(x_int, "estimate"), with = F]
    dtp_boot %<>% rbind(dtp_boot1)
  }
  dt_se <- dtp_boot[, .(se = sd(estimate)), by = x_int]
  return(dt_se)
}

estimate_fs_raw <- function(DT, y, month, x_main, x_int, form) {
  if (month != "1_12") {
    DT %<>% 
      .[get(paste0("alive_", month)) == 1, ]
  }
  var <- y
  DT[, y := get(paste0(var, "_", month))]
  
  fit <- lm(form, data = DT)
  
  dt_est <- fit_to_dt(fit, x_main, x_int)
  return(dt_est)
}
