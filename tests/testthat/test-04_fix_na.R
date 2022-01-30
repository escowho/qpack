# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- fix_na(),
    'Must specify a replacement value or the quoted string mean.'
  )
})

test_that("Not specifying variable results in error",{
  expect_error(
    test <- fix_na("mean"),
    'Must specify a replacement value or the quoted string mean.'
  )
})

test_that("File not found will result in error",{
  expect_error(
    test <- fix_na(caddat, "median"),
    "Replace must be either a numeric value or the quoted string mean."
  )
})


# Clean Run with No Errors ------------------------------------------------

test_that("Fill with 0 results in no error",{

  test <- caddat %>%
    dplyr::select(speclty, q19_1) %>%
    dplyr::mutate(q19_1 = ifelse(speclty==1, NA, q19_1))

  test <- fix_na(test, 0)

  expect <- mean(test$q19_1)
  actual <- 4.024862

  expect_equal(actual, expect, tol=.01)

})

test_that("Fill with mean results in no error",{

  test <- caddat %>%
    dplyr::select(speclty, q19_1) %>%
    dplyr::mutate(q19_1 = ifelse(speclty==1, NA, q19_1))

  test <- test %>%
    dplyr::mutate(q19_1 = fix_na(q19_1, "mean"))

  expect <- mean(test$q19_1)
  actual <- 5.691406

  expect_equal(actual, expect, tol=.01)

})

