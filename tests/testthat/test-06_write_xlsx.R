# Expect Errors -----------------------------------------------------------

test_that("Not specifying data results in error",{
  expect_error(
    write_xlsx(file=file.path(tempdir(), "test.xlsx")),
    'Nothing specified to save.'
  )
})

test_that("Not specifying file results in error",{
  test <- freq(caddat, speclty)
  expect_error(
    write_xlsx(test),
    'File name and path must be specified.'
  )
})

# Clean Run with No Errors ------------------------------------------------

test_that("Minimum specifications results in no error",{

  test <- freq(caddat, speclty)

  expect_silent(
    write_xlsx(test, file=file.path(tempdir(), "test.xlsx"))
  )

  expect_true(
    file.exists(file.path(tempdir(), "test.xlsx"))
  )

  on.exit(rm(test))
})

test_that("Minimum specifications results in no error",{

  test <- readxl::read_excel(file.path(tempdir(), "test.xlsx"))
  good <- tibble::tribble(~`speclty`, ~`n`, ~`percent`,
                          "1", 106, "29.3%",
                          "2", 92, "25.4%",
                          "3", 79, "21.8%",
                          "4", 50, "13.8%",
                          "5", 35, "9.7%",
                          "Total", 362, "100.0%")

  expect_equivalent(test, good)
  on.exit(rm(test))
})

