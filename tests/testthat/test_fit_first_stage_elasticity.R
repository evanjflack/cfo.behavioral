# Data generation
N <- 100000

DT <- data.table(first_mo = sample(1:9, N, replace = T), 
                 pred_cut1 = sample(c(rep("0-2500", 7), 
                                      rep("2500-6500", 2),
                                      "6500+"), N, replace = TRUE), 
                 ensemble_pred_risk = exp(rnorm(N))) %>% 
  .[, risk_cut := bin_variable(ensemble_pred_risk, quant = 5)] %>% 
  .[, high_risk_abs := ifelse(risk_cut == 5, 1, 0)]

mm <- model.matrix(~ first_mo:factor(pred_cut1):factor(high_risk_abs) + 
                     factor(pred_cut1):factor(high_risk_abs) - 1, 
                   data = DT)

betas <- c(100, 200, 300, 150, 250, 350, 0, 10, -10, 0, 15, -15)
y <- mm %*% betas + rnorm(N, 0, 50)

DT[, cost_1_12 := y]

DT %<>% 
  .[, .(first_mo, pred_cut1, ensemble_pred_risk, cost_1_12)] %>% 
  .[, keep_jan := ifelse(first_mo != 1, 1, 0)]


# hi <- data.table(first_mo = seq(2, 9)) %>% 
#   .[, mean := 250 + 15*first_mo]
# 
# lm(log(mean) ~ first_mo, data = hi)

# Run entire function ----------------------------------------------------------
r_list <- fit_first_stage_perc_change(DT = DT, 
                                      y = "cost", 
                                      months = "1_12", 
                                      x_main = "first_mo", 
                                      x_int = "high_risk_abs", 
                                      keep_vars = "keep_jan", 
                                      cont_risk_var = "ensemble_pred_risk", 
                                      n_quant = 5, 
                                      B = 10, 
                                      quiet = TRUE)

dt_est <- r_list$dt_est
dtp <- r_list$dtp

test_that("fit_first_stage_perc_change returns corect object", {
  expect_equal(typeof(r_list), "list")
  expect_equal(length(r_list), 2)
  expect_equal(nrow(dtp), 6*8)
  expect_equal(nrow(dt_est), 6)
})
