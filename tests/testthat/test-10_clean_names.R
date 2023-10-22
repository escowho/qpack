# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- clean_names(),
    'Data must be specified.'
  )
})


# Clean Run with No Errors ------------------------------------------------

test_that("clean_names returns correct variables sheet",{

  test <- tibble::tribble(~`Variable 1`, ~`variableTwoNext`, ~`321`,
                          1, 2, 3, 4, 5, 6)

  expect_no_error(
    test1 <- clean_names(test, create_list=TRUE)$vars
  )

  expect_equal(test1$clean_vars[[1]], "variable_1")
  expect_equal(test1$clean_vars[[2]], "variable_two_next")
  expect_equal(test1$clean_vars[[3]], "x321")
})

test_that("clean_names returns correct data sheet",{

  test <- tibble::tribble(~`Variable 1`, ~`variableTwoNext`, ~`321`,
                          1, 2, 3, 4, 5, 6)

  expect_no_error(
    test1 <- clean_names(test, create_list=TRUE)$data
  )

  expect_equal(names(test1), c("variable_1", "variable_two_next", "x321"))
})
