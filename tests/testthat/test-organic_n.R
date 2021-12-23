test_that("Fails on negative value", {
  a <- 0.001
  b <- 0.002
  c <- -0.003

  expect_error(organic_n(a, b, c))
})

test_that("Rounding correctly", {
  conc <- organic_n(0.001, 0.002, 0.001, digits = 3)
  n <- n_decimals(conc)

  expect_equal(n, 3)
})
