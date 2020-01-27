#' Subset to analystic sample
#' 
#' @param DT a data.table
#' @param subset_vars variables to subset
#' @param progress logical, if TRUE prints where in subsetting process the
#'  function is
#' @param balance_vars string character, if non-NULL, then estimates balance
#' 
#' @return a list with three elements
#' 
#' @export
subset_sample <- function(DT, subset_vars, progress = F, balance_vars = NULL) {
  DT_subset <- DT
  obs <- data.frame(subset = "all", obs = nrow(DT_subset),
                    u_obs = uniqueN(DT_subset$bene_id))
  dt_fit <- data.table()
  for (var in subset_vars) {
    if (progress == T) {
      print(var)
    }
    DT_subset  <- DT_subset[get(var) == 1, ]
    obs1 <- data.table(subset = var, obs = nrow(DT_subset),
                       u_obs = uniqueN(DT_subset$bene_id))
    obs <- rbind(obs, obs1)
    if (!is.null(balance_vars)) {
      dt_fit1 <- iter_balance_fit(DT_subset, balance_vars) %>% 
        .[, subset_var := var]
      dt_fit %<>% rbind(dt_fit1)
    }
  }
  return_list <- list(DT_subset = DT_subset, obs = obs, dt_bal = dt_fit)
  print(obs)
  return(return_list)
}

iter_balance_fit <- function(DT, balance_vars) {
  dt_fit <- data.table()
  for (b in balance_vars) {
    DT_fit <- copy(DT) %>% 
      .[, y := get(b)]
    fit <- lm(y ~ first_mo, data = DT_fit)
    dt_fit1 <- fit_to_dt(fit, "first_mo") %>% 
      .[, variable := b]
    dt_fit %<>% rbind(dt_fit1)
  }
  return(dt_fit)
}