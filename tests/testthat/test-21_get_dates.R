# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- get_dates(),
    'Data must be specified.'
  )
})

# Clean Run with No Errors ------------------------------------------------

test_that("clean run 1",{

  expect_silent(
    test1 <- get_dates(test3)
  )

  expect_equal(test1$date, c("start_date", "end_date", "recorded_date"))
  expect_equal(test1$min, c(lubridate::as_date("2021-11-01"), lubridate::as_date("2021-11-01"), lubridate::as_date("2021-11-01")))
  expect_equal(test1$percent, c(.221, .223, .223))
  on.exit(rm(test1))
})

test_that("clean run 2",{

  expect_silent(
    test2 <- test3 %>%
      dplyr::select(start_date, recorded_date) %>%
      get_dates(.)
  )

  expect_equal(test2$date, c("start_date", "recorded_date"))
  expect_equal(test2$max, c(lubridate::as_date("2023-04-05"), lubridate::as_date("2023-04-05")))
  on.exit(rm(test2))
})
