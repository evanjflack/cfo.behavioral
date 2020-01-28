# This checks to see that calc cmean converges to the true conditional mean

N <- 1000000
library(data.table)
library(dplyr)
library(tidyr)

x1 <- seq(1, 5)
x2 <- seq(0, 1)
DT <- data.table(x1 = sample(x1, N, replace = T), 
                 x2 = sample(x2, N, replace = T)) %>% 
  .[, y := 2*x1 + -1*x2 + rnorm(N, 0, 2)]

DT_cmean <- calc_cmean(DT, y = "y", x = c("x1", "x2")) %>% 
  .[order(x1, x2)] %>% 
  .[, mean := round(mean, 1)]

DT_cmean_exp <- crossing(x1, x2) %>% 
  as.data.table() %>% 
  .[, variable := "y"] %>% 
  .[, mean := 2*x1 + -1*x2]

test_that("Test calc_cmean", {
  expect_equal(mean(DT_cmean == DT_cmean_exp), 1)
})
