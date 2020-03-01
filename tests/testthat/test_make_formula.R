# ------------------------------------------------------------------------------
# Tests the make_formula function from the script fir_first_stage_elasticity.R

form <- make_formula("y", "x1", c("x2", "x3"))

exp_form <- y ~ x1:factor(x2):factor(x3) + factor(x2)*factor(x3)

test_that("Test make_formula", {
  expect_equal(form, exp_form)
})








