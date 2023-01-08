# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- create_na(),
    'Data must be specified.'
  )
})


# Clean Run with No Errors ------------------------------------------------

test_that("Remove -99 with no error",{

  set.seed(1234)
  test_1 <- matrix(sample(c(-99, 1:10), 100, replace = TRUE), 10) %>%
    as.data.frame()

  expect <- sum(test_1$V5)
  actual <- -254
  expect_equal(actual, expect, tol=.1)

  test_2 <- test_1 %>%
    create_na()
  expect <- sum(test_2$V5, na.rm=TRUE)
  actual <- 43

  expect_equal(actual, expect, tol=.1)

})

