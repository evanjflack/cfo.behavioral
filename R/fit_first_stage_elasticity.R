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
#' @param quiet logical (default = FALSE), if TRUE then does not print progress
#'
#' @return List with 2 elements:
#' \item{dt_est}{a data.table of the first stage estimates (and their standard 
#'               errors) in percentage terms by the specified interactions}
#' \item{dtp}{a data.table with the conditional means of y by enrollment month, 
#'            spending bin, any any other x_int}
#'
#' @importFrom data.table data.table
#' @importFrom stringr str_split_fixed
#' @importFrom stats as.formula median
#'
#' @export
fit_first_stage_perc_change <- function(DT, y,  months = "1_12",
                                        x_main = "first_mo", x_int = NULL,
                                        keep_vars, cont_risk_var = NULL,
                                        n_quant = 5, B = 10, quiet = FALSE) {
  
  # Prep Data
  DT <- prep_fs_elasticity_data(DT, keep_vars, cont_risk_var, n_quant)
  
  x_int <- c("pred_cut1", x_int)
  
  # Make Formula
  form <- make_formula(y = "l_mean", x_main, x_int)
  
  dt_est <- data.table()
  dtp <- data.table()
  for (j in months) {
    if (quiet == FALSE) {
      print(j)
    }
    DT_fit <- copy(DT)
    # Deal with people being alive
    if (j != "1_12") {
      DT_fit %<>% 
        .[get(paste0("alive_", j)) == 1, ]
    }
    
    # Estimates
    r_list <- estimate_fs_perc_change(DT_fit, form, y = paste0(y, "_", j), x_main, 
                                      x_int)
    dt_est1 <- r_list$dt_est
    dtp1 <- r_list$dtp %>%
      .[, month := j]
    dtp %<>% rbind(dtp1)
    # Standard errors
    dt_se1 <- bootstrap_fs_perc_change(DT_fit, form = form, y = paste0(y, "_", j), 
                                       x_main, x_int, B, quiet = quiet)
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
  
  r_list <- list(dt_est = dt_est, dtp = dtp)
  return(r_list)
}

#' Estimate First Stage
#'
#' Estimate the first stage (y ~ first_mo), where y is a binary variable (e.g.
#'  statin fill indiator) in gross terms
#'
#' @inheritParams fit_first_stage_perc_change
#' 
#' @return a data.table of the first stage estimates (and their standard errors)
#'
#' @export
fit_first_stage_raw <- function(DT, y,  months = "1_12",
                                x_main = "first_mo", x_int = NULL,
                                keep_vars, cont_risk_var = NULL,
                                n_quant = 5, quiet = FALSE) {
  
  # Prep Data
  DT <- prep_fs_elasticity_data(DT, keep_vars, cont_risk_var, n_quant)
  
  x_int <- c("pred_cut1", x_int)
  
  # Make Formula
  form <- make_formula("y", x_main, x_int)
  
  dt_est <- data.table()
  for (j in months) {
    if (quiet == FALSE) {
      print(j)
    }
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
#' @inheritParams fit_first_stage_perc_change
#'
#' @return subsetted data.table with new variables
#'
#' @export
prep_fs_elasticity_data <- function(DT, keep_vars, cont_risk_var = NULL, 
                                    n_quant = 5) {
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

#' Make a variable a factor for a formula
#' 
#' @param x string, name of variable in data.table
#' 
#' @return string, "factor(x)"
paste_factor <- function(x) {
  paste0("factor(", x, ")")
}

#' Make Formula
#' 
#' Make formula for the first stage estimation
#' 
#' @inheritParams fit_first_stage_perc_change
#' 
#' @export
make_formula <- function(y, x_main, x_int) {
  int1 <- paste0(c(x_main, paste_factor(x_int)), collapse = ":")
  int2 <- paste0(paste_factor(x_int), collapse = "*")
  form <- as.formula(paste(y, "~", paste(int1, int2, sep = " + ")))
  return(form)
}

#' Estimate FS Coefficient (Percentage Change)
#' 
#' @inheritParams fit_first_stage_perc_change
#' @param form first stage formula
#' 
#' @return List woth two elements
estimate_fs_perc_change <- function(DT, form, y, x_main, x_int) {
  var <- y
  DT[, y := get(paste0(var))]
  
  dtp <- calc_cmean(DT, paste0(var), c(x_main, x_int), se = T) %>%
    .[, l_mean := log(mean)]
  
  fit_perc <- lm(form, data = dtp, weights = obs)
  
  dt_est <- fit_to_dt(fit_perc, x_main, x_int) %>%
    .[, c(x_int, "estimate"), with = F]
  
  r_list <- list(dtp = dtp, dt_est = dt_est)
  return(r_list)
}

#' Bootstrap SE Percentage Change Models
#' 
#' @inheritParams fit_first_stage_perc_change
#' @param form fomula, first stage formula
#' 
#' @return data.table with standard errors
bootstrap_fs_perc_change <- function(DT, form, y, x_main, x_int, B, 
                                     quiet) {
  dtp_boot <- data.table()
  var <- y
  for (i in 1:B) {
    if (quiet == FALSE) {
      print(i)
    }
    samp <- sample(1:nrow(DT), nrow(DT), replace = T)
    DT1 <- DT[samp, ]
    dtp1 <- calc_cmean(DT1, paste0(var), c(x_main, x_int), se = T) %>%
      .[, l_mean := log(mean)]
    
    fit_perc1 <- lm(form, data = dtp1, weights = obs)
    dtp_boot1 <- fit_to_dt(fit_perc1, x_main, x_int) %>%
      .[, c(x_int, "estimate"), with = F]
    dtp_boot %<>% rbind(dtp_boot1)
  }
  dt_se <- dtp_boot[, .(se = sd(estimate)), by = x_int]
  return(dt_se)
}

#' Estimate First Stage Coefficients
#' 
#' @inheritParams estimate_fs_perc_change
#' @param month character, month of analysis (in form "year_month")
#' 
#' @return data.table with first stage estimates and standard errors
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

#' Xwalk ATC4 to Generic RXCUI
#' 
#' Find all generic rxcui codes associated with a vector of atc codes
#' 
#' @param atc_codes character vector of atc code(s)
#' @param rxcui_xwalk data.table with 2 columns lab_prod (9 digit ndc) and 
#'  g_rxcui (the generic rxcui code)
#' @param atc_ind data.table, xwalk between lab_prod and atc levels, atc levels
#'  are indicators by column
#'  
#' @return character vector of all generic rxcui codes associated with the
#'  given atc codes
return_g_rxcui <- function(atc_codes, rxcui_xwalk, atc_ind) {
  rxcui_codes <- atc_ind %>% 
    .[, c("lab_prod", atc_codes), with = F] %>% 
    .[, any_code := rowSums(.SD), .SDcols = atc_codes] %>% 
    .[any_code >= 1, ] %>% 
    .[, .(lab_prod)] %>% 
    merge(rxcui_xwalk, by = "lab_prod") %>% 
    .[!is.na(g_rxcui), ] %>% 
    .[, g_rxcui] %>% 
    unique()
  return(rxcui_codes)
}

#' Prep Price Data
#' 
#' @param price_by_drug data.table with average price and number of observations
#'  by g_rxcu code/coverage arm. Columns should be named as follows: g_rxcui, 
#'  arm, oop_cost, obs
#' @param rxcui_codes character vector of g_rxcui code(s) of interest
#' @param n_quant number of quantiles to cut the price variable into
#' @param price_var price variable to use for cutting price into quantiles
#' 
#' @return data.table
prep_price_data <- function(price_by_drug, rxcui_codes, n_quant, 
                            price_var) {
  price_change <- price_by_drug %>% 
    .[g_rxcui %in% rxcui_codes] %>% 
    dcast(g_rxcui ~ arm, value.var = c("oop_cost", "obs")) %>% 
    .[, price_diff := oop_cost_gap - oop_cost_pre] %>% 
    .[, price_perc_diff := price_diff/oop_cost_pre] %>% 
    .[order(-obs_pre), ] %>% 
    .[!is.na(price_diff), ] %>% 
    .[order(-obs_pre), ] %>% 
    .[, price_diff_cut := bin_variable(get(price_var), quant = n_quant)]
  return(price_change)
}

#' Iterate Price
#' 
#' Iterate through different priced drugs in first stage percentage change 
#'  estimation
#' 
#' @inheritParams fit_first_stage_perc_change
#' @param price_var character, name of price variable
#' @param price_change data.table of price changes by drugs
#' @param form formula, first stage formula
#' 
#' @return data.table of first stage estimates by price of drug
iterate_price <- function(DT, form,  price_change, B, price_var, x_main, x_int, 
                          quiet) {
  n_quant_price <- uniqueN(price_change$price_diff_cut)
  dt_est <- data.table()
  for (i in 1:n_quant_price) {
    price_rxcui <- price_change[price_diff_cut == i, g_rxcui] %>% 
      as.character() %>% 
      paste0("g_rxcui_", .)
    
    price_lab_med <- median(price_change[price_diff_cut == i, get(price_var)])
    price_lab_min <- min(price_change[price_diff_cut == i, get(price_var)])
    price_lab_max <- max(price_change[price_diff_cut == i, get(price_var)])
    
    DT %<>% 
      .[, fills := rowSums(.SD), .SDcols = price_rxcui] %>% 
      .[, any_fill := ifelse(fills > 0, 1, 0)]
    
    
    r_list <- estimate_fs_perc_change(DT, form, y = "any_fill", x_main, x_int)
    dt_se <- bootstrap_fs_perc_change(DT, form, y = "any_fill", x_main, x_int, 
                                      B = B, quiet = quiet)
    dt_est1 <- r_list$dt_est %>% 
      merge(dt_se, by = x_int) %>% 
      .[, price := i] %>% 
      .[, price_lab_med := price_lab_med] %>% 
      .[, price_lab_min := price_lab_min] %>% 
      .[, price_lab_max := price_lab_max]
    
    dt_est %<>% rbind(dt_est1)
    
  }
  dt_est %<>% 
    .[, lb := estimate - 1.96*se] %>% 
    .[, ub := estimate + 1.86*se]
  return(dt_est)
}

#' Fit first stage price
#' @inheritParams fit_first_stage_perc_change
#' @param atc_codes character vector, atc codes to run models on
#' @param rxcui_xwalk data.table, xwalk of atc code to rxcui
#' @param price_by_drug data.table, prices by drug and coverage arm
#' @param atc_ind data.table, atc indicators for rxcui codes
#' @param n_quant_price integer, number of quantiles to break price in to
#' @param n_quant_risk integer, number of quantiles to break predicted risk
#'  in to
#' @param price_var character, name of price variable to use
#' @param return_data logical (default = FALSE), if TRUE returns model data
#' @param return_rxcui logical (default = TRUE), if TRUE then returns vector
#'  of rxcui codes that were sued in estimation. 
#' 
#' @return data.table
#' 
#' @export
fit_first_stage_price <- function(DT, keep_vars, cont_risk_var, 
                                  n_quant_risk, atc_codes, rxcui_xwalk, 
                                  price_by_drug, atc_ind, n_quant_price,
                                  x_main = "first_mo", 
                                  x_int = c("pred_cut1", "hgih_risk_abs"), 
                                  price_var, B = 100,
                                  return_data = FALSE, 
                                  return_rxcui = FALSE, 
                                  quiet = FALSE) {
  
  DT <- prep_fs_elasticity_data(DT = DT, 
                                keep_vars = keep_vars, 
                                cont_risk_var = cont_risk_var, 
                                n_quant = n_quant_risk)
  
  rxcui_codes <- return_g_rxcui(atc_codes, rxcui_xwalk, atc_ind)
  price_change <- prep_price_data(price_by_drug, rxcui_codes, n_quant_price, 
                                  price_var)
  
  form <- make_formula(y = "l_mean", x_main, x_int)
  
  dtp_est <- iterate_price(DT, form = form, price_change, B = B, price_var, 
                           x_int = x_int, x_main = x_main, 
                           quiet = quiet)
  
  if (return_data == FALSE & return_rxcui == FALSE) {
    return(dtp_est)
  } else if (return_data | return_rxcui) {
    r_list <- list(dtp_est = dtp_est)
    if (return_data) {
      r_list$DT <- DT
    }
    if (return_rxcui) {
      r_list$rxcui_codes <- rxcui_codes
    }
    return(r_list)
  }
}

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("l_mean", "obs", "estimate", "variable", "month1",
                           "se", "high_risk_abs", "risk_cut_abs", "any_code", 
                           "any_fill", "fills", "g_rxcui", "obs_pre", 
                           "oop_cost", "price", "price_diff", "price_diff_cut", 
                           "price_perc_diff", "oop_cost_gap", "oop_cost_pre"))
}
