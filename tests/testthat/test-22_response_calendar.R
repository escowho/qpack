
# Expected Errors ---------------------------------------------------------

test_that("No data specified results in error",{
  expect_error(
    response_calendar(),
    'Data must be specified.'
  )
})

test_that("Mismatched number of names to variables results in error",{
  expect_error(
    response_calendar(test3, date_var=not_there),
    'Date must be either start_date, end_date, or recorded_date.'
  )
})


# Clean runs with no errors -----------------------------------------------

test_that("Files output correctly with no error",{
  expect_silent(response_calendar(test3))
})



test_that("Compare 1",{
  rc1 <- response_calendar(test3, year=2023)
  vdiffr::expect_doppelganger("c1", rc1)
})




