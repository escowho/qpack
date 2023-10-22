# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- nps_score(),
    'Data column must be specified.'
  )
})

# Clean Run with No Errors ------------------------------------------------

test_that("clean run 1",{

  set.seed(423234)
  testdat <- tibble(ltr=sample(0:11, 20, replace=TRUE)) %>%
    dplyr::mutate(score = nps_score(ltr))

  expect_no_error(
    test1 <- testdat %>%
      mutate(score = nps_score(ltr))
  )

  expect_equal(test1$score, c(100, 0, -100, 100, -100, -100, 0, 100, -100, -100,
                              -100, -100, -100, 100, 0, -100, 0, 100, 0, -100))
  on.exit(rm(test1))
})

