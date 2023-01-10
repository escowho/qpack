# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- week_number(),
    'Date column must be specified.'
  )
})

set.seed(43792)
testdat <- sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 12) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(value) %>%
  dplyr::mutate(x=1:dplyr::n()) %>%
  qpack::set_colnames(c("date", "number"))

#test_that("Not date data results in error",{
#  expect_error(
#    test <- testdat %>%
#      dplyr::mutate(week_number = week_number(number)),
#    'Date data not found in specified column.'
#  )
#})

# Clean Run with No Errors ------------------------------------------------

test_that("clean run 2",{

  expect_silent(
    test2 <- testdat %>%
      mutate(week_number = week_number(date))
  )

  expect_equal(test2$week_number, c(1,2,3,4,5,6,6,7,8,9,10,11))
  on.exit(rm(test2))
})


on.exit(rm(testdat))
