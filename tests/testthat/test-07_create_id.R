
testdat1 <- data.frame(v1 = c(4,3,2,1,NA,NA,NA,NA,NA,NA),
                       v2 = c(NA,NA,NA,NA,NA,NA,8,7,6,5),
                       v3 = c(5,4,3,2,1,NA,NA,NA,NA,NA),
                       v4 = c(NA,NA,NA,NA,NA,9,8,7,6,5))

# Expect Errors -----------------------------------------------------------

test_that("Not specifying data results in error",{
  expect_error(
    test <- create_id(),
    'Data must be specified'
  )
})

test_that("File not found will result in error",{
  expect_error(
    test <- create_id(not_there)
  )
})

test_that("File not a dataframe will result in error",{
  testdat2 <- list()
  expect_error(
    test <- create_id(testdata2)
  )
})

test_that("id already in dataframe will result in error",{
  expect_error(
    test <- create_id(testdat1, name=v1)
  )
})

# Expect Warning ----------------------------------------------------------

test_that("create_id:  Duplicates in sort variables warning",{
  expect_warning(
    test <- create_id(testdat1, v1),
    'Duplicates found'
  )
  on.exit(rm(test))
})

test_that("create_id:  Duplicates in sort variables warning",{
  expect_warning(
    test <- create_id(testdat1, v1, v2),
    'Duplicates found'
  )
  on.exit(rm(test))
})

# Clean Run with No Errors ------------------------------------------------

test_that("Minimum specifications results in no error",{
    expect_silent(
      test <- create_id(testdat1)
    )
  expect_true(("id" %in% names(test)))
  expect_true((names(test)[1] == "id"))
  on.exit(rm(test))
  })

test_that("Specifying nonduplicate sorts results in no error",{
  expect_silent(
    test <- create_id(testdat1, v3, v4)
  )
  expect_true(("id" %in% names(test)))
  expect_true((names(test)[1] == "id"))
  on.exit(rm(test))
})

test_that("Specifying drop=TRUE drops sorting variables",{
  expect_silent(
    test <- create_id(testdat1, v3, v4, remove=TRUE)
  )
  expect_true("id" %in% names(test))
  expect_true(names(test)[1] == "id")
  expect_false("v3" %in% names(test))
  expect_false("v4" %in% names(test))
  on.exit(rm(test))
})
