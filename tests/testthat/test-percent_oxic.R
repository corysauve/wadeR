test_that("Negative DO values are found", {
  depths <- c(0, 1, 2, 3)
  do <- c(12, -10, 8.0, 6)

  expect_error(percent_oxic(depths, do))
})

test_that("Catch unequal vectors", {
  depths <- c(0, 1, 2, 3)
  do <- c(10, 9, 8, 7, 6)

  expect_error(percent_oxic(depths, do))
})

test_that("NA's are found", {
  depths <- c(0, 1, NA, 3)
  do <- c(10, 9, 8, 7, 6)

  expect_error(percent_oxic(depths, do))
})

test_that("NA's are found", {
  depths_a <- c(0, 1, NA, 3)
  do_a <- c(10, 9, 8, 7, 6)

  depths_b <- c(0, 1, 2, 3)
  do_b <- c(10, NA, 8, 7, 6)

  expect_error(percent_oxic(depths_a, do_a))
  expect_error(percent_oxic(depths_b, do_b))
  expect_error(percent_oxic(depths_a, do_b))
})

test_that("Only numeric measurements are accepted", {
  depths <- c(0, 1, 2, 3)
  do <- c(10, 9, 8, "7")

  expect_error(percent_oxic(depths, do))
})
