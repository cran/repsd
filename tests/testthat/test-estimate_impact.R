test_that("estimate_impact by default returns the canonical value", {
  expect_equal(-0.421, estimate_impact())
})

test_that('estimate_impact correctly checks the function arguments', {
  expect_error(
    estimate_impact(responses = list(timmsData))
  )
  expect_error(
    estimate_impact(focal_column = 44)
  )
  expect_error(
    estimate_impact(focal_column = 'a')
  )
  expect_error(
    estimate_impact(focal_id = 13)
  )
  expect_error(
    estimate_impact(focal_id = 'b')
  )
})
