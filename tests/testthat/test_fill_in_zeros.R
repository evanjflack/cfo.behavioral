# ------------------------------------------------------------------------------
# Tests the fill_in_zeros function

DT_id <- data.table(id = c("1", "2", "3"))

DT <- data.table(id = c("1", "2"), 
                 x = rnorm(2))
DT_exp <- DT %>% 
  rbind(data.table(id = "3", x = 0)) %>%
  .[order(id), ]

DT_new <- fill_in_zeros(DT, DT_id, "id") %>% 
  .[order(id), ]


test_that("Test fill_in_zeros", {
  expect_equal(mean(DT_new == DT_exp), 1)
})