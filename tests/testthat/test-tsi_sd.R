test_that("Fails on negative values", {
  a <- c(0, 1, 2, 3)
  b <- c(0.001, 0.002, -0.003)

  expect_error(tsi_sd(a, b))
  expect_error(tsi_sd(b, b))
})

test_that("Rounding correctly", {
  x <- tsi_sd(1.4, digits = 1)
  n <- n_decimals(x)

  expect_equal(n, 1)
})
