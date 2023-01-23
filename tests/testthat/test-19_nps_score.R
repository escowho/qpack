# Expect Errors -----------------------------------------------------------

test_that("Not specifying replace results in error",{
  expect_error(
    test <- nps_score(),
    'Data column must be specified.'
  )
})

set.seed(423234)
testdat <- tibble(ltr=sample(0:11, 20, replace=TRUE)) %>%
  dplyr::mutate(score = nps_score(ltr))

# Clean Run with No Errors ------------------------------------------------

test_that("clean run 1",{

  expect_silent(
    test1 <- testdat %>%
      mutate(score = nps_score(ltr))
  )

  expect_equal(test1$score, c(100, 0, -100, 100, -100, -100, 0, 100, -100, -100,
                              -100, -100, -100, 100, 0, -100, 0, 100, 0, -100))
  on.exit(rm(test1))
})


on.exit(rm(testdat))
