# Expect Errors -----------------------------------------------------------

test_that("Not specifying data results in error",{
  expect_error(
    test <- set_colnames()
  )
})

# Clean Run with No Errors ------------------------------------------------

test_that("Minimum specifications results in no error",{

    expect_no_error(
      test <- qpack::caddat %>%
        dplyr::select(1:2) %>%
        set_colnames(c("ralph", "stephanie"))
    )
  expect_true(("ralph" %in% names(test)))
  expect_true(("stephanie" %in% names(test)))
  })
