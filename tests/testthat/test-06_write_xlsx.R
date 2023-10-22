# Expect Errors -----------------------------------------------------------

test_that("Not specifying data results in error",{
  expect_error(
    write_xlsx(file=file.path(tempdir(), "test.xlsx"))
  )
})

test_that("Not specifying file results in error",{
  test <- freq(qpack::caddat, speclty)
  expect_error(
    write_xlsx(test)
  )
})

# Clean Run with No Errors ------------------------------------------------

test_that("Minimum specifications results in no error",{

  test1 <- freq(qpack::caddat, speclty)

  expect_no_error(
    write_xlsx(test1, file=file.path(tempdir(), "test.xlsx"))
  )

  expect_true(
    file.exists(file.path(tempdir(), "test.xlsx"))
  )

  expect_no_error(
    write_xlsx(test1, file=file.path(tempdir(), "test.xlsx"), over=TRUE)
  )

  expect_no_error(
    write_xlsx(test1, file=file.path(tempdir(), "test2.xlsx"), sheet="test")
  )

  expect_error(
    write_xlsx(test1, file=file.path(tempdir(), "test2.xlsx"), sheet="test", oversheet=FALSE)
  )

  expect_no_error(
    write_xlsx(test1, file=file.path(tempdir(), "test2.xlsx"), sheet="test")
  )

  test2 <- readxl::read_excel(file.path(tempdir(), "test.xlsx"))

  good <- tibble::tribble(~`speclty`, ~`n`, ~`percent`,
                          "1", 106, "29.3%",
                          "2", 92, "25.4%",
                          "3", 79, "21.8%",
                          "4", 50, "13.8%",
                          "5", 35, "9.7%",
                          "Total", 362, "100.0%")

  expect_equal(test2, good)

  unlink(file.path(tempdir(), "test.xlsx"))
})

