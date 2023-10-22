# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- get_dates(),
    'Data must be specified.'
  )
})

# Clean Run with No Errors ------------------------------------------------

test_that("clean run 1",{

  expect_no_error(
    test1 <- get_dates(qpack::test3)
  )

  expect_equal(test1$date, c("start_date", "end_date", "recorded_date"))
  expect_equal(test1$min, c(lubridate::as_date("2021-11-01"), lubridate::as_date("2021-11-01"), lubridate::as_date("2021-11-01")))
  expect_equal(test1$percent, c(.221, .223, .223))
})

test_that("clean run 2",{

  expect_no_error(
    test2 <- qpack::test3 %>%
      dplyr::select(start_date, recorded_date) %>%
      get_dates(.)
  )

  expect_equal(test2$date, c("start_date", "recorded_date"))
  expect_equal(test2$max, c(lubridate::as_date("2023-04-05"), lubridate::as_date("2023-04-05")))
})
