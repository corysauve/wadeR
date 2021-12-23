test_that("Stop if there are negative values", {
  a <- c(0, 1, 2, 3)
  b <- c(0, 1, -2, 3)

  expect_error(one_percent_light(a, b))
  expect_error(one_percent_light(b, b))
})
