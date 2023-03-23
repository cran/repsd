set.seed(123)
# Test that the function returns the expected output
test_that("null_repsd returns expected output", {
  expect_type(null_repsd(iterations = 2), "double")
  expect_equal(ncol(null_repsd(iterations = 1)), 20)
  expect_equal(nrow(null_repsd(iterations = 1)), 1)
})

# Test that the function correctly handles the focal_sample argument
test_that("null_repsd handles focal_sample argument correctly", {
  expect_error(null_repsd(focal_sample = 'not_a_number'))
  expect_error(null_repsd(focal_sample = -10))
})

# Test that the function correctly handles the focal_prop argument
test_that("null_repsd handles focal_prop argument correctly", {
  expect_error(null_repsd(focal_prop = -0.1))
  expect_error(null_repsd(focal_prop = 1.1))
  expect_error(null_repsd(focal_prop = 'not_a_number'))
})

# Test that the function correctly handles the numStrata argument
test_that('null_repsd handles numStrata argument correctly', {
  expect_error(
    null_repsd(numStrata = 0)
  )
  expect_error(
    null_repsd(numStrata = 'a')
  )
})

# Test that the function correctly handles the impact argument
test_that("null_repsd handles impact argument correctly", {
  expect_error(null_repsd(impact = 'not_a_number'))
})

# Test that the function correctly handles the item_params_a argument
test_that("null_repsd handles item_params_a argument correctly", {
  expect_error(null_repsd(item_params_a = c(1, 2, 'not_a_number')))
  expect_error(null_repsd(item_params_a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 'not_a_number')))
})

# Test that the function correctly handles the item_params_b argument
test_that("null_repsd handles item_params_b argument correctly", {
  expect_error(null_repsd(item_params_b = c(1, 2, 'not_a_number')))
  expect_error(null_repsd(item_params_b = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 'not_a_number')))
})

# Test that the function correctly handles the iterations argument
test_that("null_repsd handles iterations argument correctly", {
  expect_error(null_repsd(iterations = 'not_a_number'))
  expect_error(null_repsd(iterations = -10))
})
