# Create a test data set
set.seed(123)
responses <- data.frame(matrix(rnorm(2000), ncol = 20))

# Create a test function for the null_repsd() function
null_repsd_test <- function() {
  data.frame(matrix(rnorm(2000), ncol = 20))
}

# Create a test function for the repsd() function
repsd <- function() {
  list(repsd_each_item = rnorm(20))
}

# Test that the function returns the expected output
test_that("repsd_pval returns expected output", {
  expect_type(repsd_pval(null_dist = null_repsd_test(), verbose = FALSE), "list")
  expect_equal(ncol(repsd_pval(null_dist = null_repsd_test(), verbose = FALSE)), 4)
  expect_equal(colnames(repsd_pval(null_dist = null_repsd_test(), verbose = FALSE)), c("items", "repsd", "p.value", "sig"))
})

# Test that the function correctly calculates p-values
test_that("repsd_pval calculates correct p-values", {
  p_values <- repsd_pval(null_dist = null_repsd_test(), verbose = FALSE)$`p-value`
  expect_true(all(p_values >= 0 & p_values <= 1))
})

# Test that the function correctly identifies significant items
test_that("repsd_pval correctly identifies significant items", {
  sig_values <- repsd_pval(null_dist = null_repsd_test(), verbose = FALSE)$sig
  expect_true(all(sig_values == 0 | sig_values == 1))
})

# Test that the function correctly handles the verbose argument
test_that("repsd_pval handles verbose argument correctly", {
  expect_output(repsd_pval(null_dist = null_repsd_test(), verbose = TRUE), "")
  expect_invisible(repsd_pval(null_dist = null_repsd_test(), verbose = FALSE))
})
