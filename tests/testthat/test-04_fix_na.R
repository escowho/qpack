# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- fix_na(),
    'Data must be specified.'
  )
})

test_that("File not found will result in error",{
  expect_error(
    test <- fix_na(caddat, "nope"),
    "Replace must be either a numeric value, \"mean\" or \"median\"."
  )
})

test_that("No replacement specified warning",{
  expect_warning(
    test <- fix_na(caddat),
    "Replace not specified; choosing replace=\"mean\" by default."
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

test_that("Fill entire dataframe with mean results in no error",{

  test <- caddat %>%
    dplyr::select(speclty, q19_1) %>%
    dplyr::mutate(q19_1 = ifelse(speclty==1, NA, q19_1))

  test <- test %>%
    fix_na(., "mean")

  expect <- mean(test$q19_1)
  actual <- 5.691406

  expect_equal(actual, expect, tol=.01)
})

test_that("Fill with mean results in no error",{

  test <- caddat %>%
    dplyr::select(speclty, q19_1) %>%
    dplyr::mutate(q19_1 = ifelse(speclty==1, NA, q19_1))

  test <- test %>%
    dplyr::mutate(q19_1 = fix_na(q19_1, replace="mean"))

  expect <- mean(test$q19_1)
  actual <- 5.691406

  expect_equal(actual, expect, tol=.01)

})

test_that("Fill with median results in no error",{

  test <- caddat %>%
    dplyr::select(speclty, q19_1) %>%
    dplyr::mutate(q19_1 = ifelse(speclty==1, NA, q19_1))

  test <- test %>%
    dplyr::mutate(q19_1 = fix_na(q19_1, "median"))

  expect <- mean(test$q19_1)
  actual <- 5.781768

  expect_equal(actual, expect, tol=.01)

})

