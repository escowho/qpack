test_that("Not specifying data results in error",{
  expect_error(
    test <- flip(),
    'Data must be specified.'
  )
})

test_that("Clean Run",{
  expect_silent(
    test <- flip(test1)
  )

  expect_equal("tbl_df" %in% class(test), TRUE)
  expect_equal(names(test)[1:5], c("column", "row_1", "row_2", "row_3", "row_4"))
  expect_equal(names(test1), test$column)
  on.exit(rm(test))
})

