library(polypharmacy)


# Test that the correct formula is returned
form <- make_formula(controls = c("race", "sex"), deg = 1, 
                     time_interact = "rfrnc_yr", keep_same = 0)

exp_form <- outcome ~ cost100 + factor(pred_cut)*factor(rfrnc_yr) + 
  factor(race) + factor(sex) | 
  poly(instrument, 1)*factor(pred_cut)*factor(rfrnc_yr) + 
  factor(race) + factor(sex)
  
test_that("IV formula returned", {
  expect_equal(form, exp_form)
})