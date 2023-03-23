test_that("repsd returns list of length 4", {
  expect_equal(4, length(repsd()))
})

test_that("repsd repsd_each_item defaults return 20 repsd values", {
  test_repsd_output =
    repsd()

  expect_equal(20, length(test_repsd_output$repsd_each_item))
})

test_that('repsd returns the correct values overall', {
  test_repsd_output =
    repsd()

  expect_identical(canon_repsd_output, test_repsd_output)
})

test_that('repsd returns errors with improper arguments', {
  expect_error(
    repsd(responses = list(timmsData))
  )
  expect_error(
    repsd(focalColumn = 22)
  )
  expect_error(
    repsd(focalColumn = 'a')
  )
  expect_error(
    repsd(focalGroupID = 13)
  )
  expect_error(
    repsd(focalGroupID = 'a')
  )
  expect_error(
    repsd(numStrata = 0)
  )
  expect_error(
    repsd(numStrata = 'a')
  )
})

# Create a test data set
set.seed(123)
responses <- data.frame(matrix(rnorm(2000), ncol = 20))

# Test that the function correctly handles the focalColumn argument
test_that("repsd handles focalColumn argument correctly", {
  expect_error(repsd(focalColumn = 'not_a_column'))
  expect_error(repsd(focalColumn = 25))
})

# Test that the function correctly handles the focalGroupID argument
test_that("repsd handles focalGroupID argument correctly", {
  expect_error(repsd(focalGroupID = 'not_a_value'))
  expect_error(repsd(focalGroupID = 25))
})

# Test that the function correctly handles the numStrata argument
test_that("repsd handles numStrata argument correctly", {
  expect_error(repsd(numStrata = 1))
  expect_error(repsd(numStrata = 'not_a_number'))
})

