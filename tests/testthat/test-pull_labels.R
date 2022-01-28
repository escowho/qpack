test_that("Not specifying data results in error",{
  expect_error(
    test <- pull_labels(),
    'Data must be specified.'
  )
})

test_that("Clean Run",{
  expect_silent(test <- pull_labels(test1))

  expect_equal(names(test), c("variable_labels", "value_labels"))
  expect_equal(names(test$variable_labels), c("variable", "variable_label"))
  expect_equal(test$variable_labels[[2,1]], "q1")
  expect_equal(test$variable_labels[[6,1]], "q5")
  expect_equal(test$variable_labels[[8,1]], "region")
  expect_equal(test$variable_labels[[2,2]], "Please indicate your age (in years).")
  expect_equal(test$variable_labels[[6,2]], "Thinking back to 2020, which of the following categories best describes your total household income for that year?")
  expect_equal(test$variable_labels[[8,2]], "Region")
  expect_equal(names(test$value_labels), c("value", "response_id", "q1", "q2", "q3", "q4", "q5", "q6", "region", "prob1", "prob2", "prob3"))
  expect_equal(test$value_labels[[1,1]], 1)
  expect_equal(test$value_labels[[2,3]], "45-64")
  expect_equal(test$value_labels[[3,4]], "Other")
  expect_equal(test$value_labels[[7,7]], "$200,000 or more")
  expect_equal(is.na(test$value_labels[[8,8]]), TRUE)
  on.exit(rm(test))
})

