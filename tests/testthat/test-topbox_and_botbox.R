# Expect Errors -----------------------------------------------------------
context("topbox: Testing Assertion Errors")

test_that("Not specifying replace results in error",{
  expect_error(
    test <- topbox(),
    'Data must be specified.'
  )
})

test_that("Not specifying numeric top value results in error",{
  expect_error(
    test <- test <- topbox(caddat$q19_1, top="j"),
    'Top variable must be a number.'
  )
})


# Expect Warning ----------------------------------------------------------
test_that("Not specifying maxval results in warning",{
  expect_warning(
    test <- topbox(caddat$q19_1),
    'Determining maxval from the data.'
  )
})


# Clean Run with No Errors ------------------------------------------------
context("topbox: Clean Run with No Errors")

test_that("Topbox runs ok",{

  expect_silent(
  test <- caddat %>%
    dplyr::select(q19_1, q19_2) %>%
    dplyr::mutate(q19_2 = ifelse(q19_2<=3, NA, q19_2),
                  test1 = topbox(q19_1, maxval=7, top=2),
                  test2 = topbox(q19_1, maxval=7),
                  test3 = topbox(q19_1, maxval=7, top=1),
                  test4 = topbox(q19_2, maxval=7, top=2),
                  test5 = topbox(q19_2, maxval=7, top=2, replacena=TRUE)) %>%
    dplyr::select(test1, test2, test3, test4, test5)
)

  expect1 <- mean(test$test1)
  expect2 <- mean(test$test2)
  expect3 <- mean(test$test3)
  expect4 <- mean(test$test4)
  expect5 <- mean(test$test5)
  actual1 <- 0.671
  actual2 <- 0.671
  actual3 <- 0.108
  actual5 <- 0.818
  expect_equal(actual1, expect1, tol=.001)
  expect_equal(actual2, expect2, tol=.001)
  expect_equal(actual3, expect3, tol=.001)
  expect_true(is.na(expect4))
  expect_equal(actual5, expect5, tol=.001)

})

# Expect Errors -----------------------------------------------------------
context("botbox: Testing Assertion Errors")

test_that("Not specifying replace results in error",{
  expect_error(
    test <- botbox(),
    'Data must be specified.'
  )
})

test_that("Not specifying numeric top value results in error",{
  expect_error(
    test <- test <- botbox(caddat$q19_1, bot="j"),
    'Bot variable must be a number.'
  )
})


# Expect Warning ----------------------------------------------------------
test_that("Not specifying maxval results in warning",{
  expect_warning(
    test <- test <- botbox(caddat$q19_1),
    'Determining minval from the data.'
  )
})


# Clean Run with No Errors ------------------------------------------------
context("botbox: Clean Run with No Errors")

test_that("Botbox runs ok",{

  expect_silent(
    test <- caddat %>%
      dplyr::select(q19_1, q19_2) %>%
      dplyr::mutate(q19_2 = ifelse(q19_2<=3, NA, q19_2),
                    test1 = botbox(q19_1, minval=1, bot=2),
                    test2 = botbox(q19_1, minval=1),
                    test3 = botbox(q19_1, minval=1, bot=1),
                    test4 = botbox(q19_2, minval=1, bot=2),
                    test5 = botbox(q19_2, minval=1, bot=2, replacena=TRUE)) %>%
      dplyr::select(test1, test2, test3, test4, test5)
  )

  expect1 <- mean(test$test1)
  expect2 <- mean(test$test2)
  expect3 <- mean(test$test3)
  expect4 <- mean(test$test4)
  expect5 <- mean(test$test5)
  actual1 <- 0.005525
  actual2 <- 0.005525
  actual3 <- 0.002762
  actual5 <- 0
  expect_equal(actual1, expect1, tol=.001)
  expect_equal(actual2, expect2, tol=.001)
  expect_equal(actual3, expect3, tol=.001)
  expect_true(is.na(expect4))
  expect_equal(actual5, expect5, tol=.001)

})

