library(polypharmacy)

data <- data.table(x1 = sample(1:5, 1000, replace = TRUE),
                   x2 = sample(1:2, 1000, replace = TRUE)) %>%
  .[, y := 2*x1 + 4*x2 + rnorm(mean = 0, sd = 2, n = 1000)]

DT_cmean <- calc_cmean(data, y = c("y"), x = c("x1", "x2"))

expect_cmean <- data %>%
  .[, .(x1, x2)] %>% 
  unique() %>% 
  .[, expect_mean := 2*x1 + 4*x2]

DT_cmean <- DT_cmean %>% 
  merge(expect_cmean, by = c("x1", "x2")) %>% 
  .[, abs_diff := abs(mean - expect_mean)]

test_that("Close to expected mean", {
  expect_equal(round(DT_cmean$abs_diff), rep(0, nrow(DT_cmean)))
})
