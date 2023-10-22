# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- rescale(min_val=1, max_val=7),
    'Data must be specified.'
  )
})

# Expect Warning ----------------------------------------------------------

test_that("Warning when min and max not specified ut clean run 1",{
  set.seed(1234)
  test <- data.frame(matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10))

  expect_warning(
    test1 <- test %>%
      dplyr::select(X1) %>%
      dplyr::mutate(new = rescale(X1)),
    'Using MIN_VAL'
  )

  expect_equal(test1$X1, c(9, 5, 4, 8, 4, 5, 3, 1, 6, 5))
  expect_equal(test1$new, c(1.000, 0.500, 0.375, 0.875, 0.375, 0.500, 0.250, 0.000, 0.625, 0.500))
})

# Clean Run with No Errors ------------------------------------------------

test_that("clean run 2",{
  set.seed(1234)
  test <- data.frame(matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10))

  expect_silent(
    test2 <- test %>%
      dplyr::select(X2) %>%
      dplyr::mutate(new = rescale(X2, min_val=1, max_val=100))
  )

  expect_equal(test2$X2, c(9, 5, 3, 7, 3, 3, 4, 7, 3, 7))
  expect_equal(test2$new, c(100.0, 34.0, 1.0, 67.0, 1.0, 1.0, 17.5, 67.0, 1.0, 67.0))
})


test_that("clean run 3",{
  set.seed(1234)
  test <- data.frame(matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10))

  expect_no_error(
    test3 <- test %>%
      dplyr::select(X3) %>%
      dplyr::mutate(new = rescale(X3, min_val=1, max_val=7)) %>%
      dplyr::mutate(new = round(new, 1))
  )

  expect_equal(test3$X3, c(2, 3, 9, 4, 1, 7, 10, 3, 2, 6))
  expect_equal(test3$new, c(1.7, 2.3, 6.3, 3.0, 1.0, 5.0, 7.0, 2.3, 1.7, 4.3))
})

test_that("clean run 4",{

  set.seed(1234)
  test <- data.frame(matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10))

  expect_no_error(
    test4 <- test %>%
      dplyr::select(X1) %>%
      dplyr::mutate(new = rescale(X1, flip=TRUE))
  )

  expect_equal(test4$X1, c(9, 5, 4, 8, 4, 5, 3, 1, 6, 5))
  expect_equal(test4$new, c(1, 5, 6, 2, 6, 5, 7, 9, 4, 5))
})
