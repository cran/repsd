set.seed(123)
# Create example inputs for the function
example_repsd <- c(0.1, 0.2, 0.3, 0.4)
example_null <- matrix(rnorm(100), nrow=100, ncol=4)
example_pvalues <- c(0.05, 0.01, 0.02, 0.03)
which_item <- 2
bins <- 30

# Test 1: Check if function returns an error if inputs are of incorrect class or dimensions
expect_error(plot_repsd(repsd_values = example_repsd,
                        null_values = example_null,
                        pvalues = example_pvalues,
                        which_item = 5,
                        bins = 30))

expect_error(plot_repsd(repsd_values = "example_repsd",
                        null_values = example_null,
                        pvalues = example_pvalues,
                        which_item = 1,
                        bins = 30))

expect_error(plot_repsd(repsd_values = example_repsd,
                        null_values = example_null,
                        pvalues = example_pvalues,
                        which_item = 1,
                        bins = "30"))
