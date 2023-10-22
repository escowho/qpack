test_that("Not specifying data results in error",{
  expect_error(
    test <- describe(),
    'Data must be specified.'
  )
})

test_that("Clean Run",{
  expect_no_error(
    test <- describe(qpack::test1)
  )

  expect_equal("tbl_df" %in% class(test), TRUE)
  expect_equal(names(test), c("Variable", "Mean", "SD", "n", "Missing",
                              "Min","Q10", "Q25", "Med", "Q75", "Q90", "Max"))
  expect_equal(names(qpack::test1), test$Variable)
  expect_equal(test[[2]], c(NA, 1.94, 1.70, 1.54, 1.44, 3.40, 2.47, 2.39, 2.08, NA, 2.01), tolerance=.01)
  expect_equal(test[[5]], c(NA, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0))
  expect_equal(test[[9]], c(NA, 2, 2, 1, 1, 3, 2, 3, 2, NA, 2))
})

