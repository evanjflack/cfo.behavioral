# Tests that subset sample returns the correct

N <- 10000
K1 <- 1000
K2 <- 5000

DT <- data.table(bene_id = seq(1, N),
                 keep_1= sample(c(rep(0, K1), rep(1, N - K1)), N), 
                 keep_2= sample(c(rep(0, K2), rep(1, N - K2)), N)) %>% 
  .[, keep_any := ifelse(rowSums(.SD) == 2, 1, 0), 
    .SDcols =c("keep_1", "keep_2")]

exp_row <- sum(DT$keep_any)

ret <- subset_sample(DT, c("keep_1", "keep_2"))

test_that("subset_sample works", {
  expect_equal(length(ret), 3)
  expect_equal(nrow(ret$DT_subset), exp_row)
  expect_equal(nrow(ret$DT_subset), ret$obs[, obs[.N]])
})
