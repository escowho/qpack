current_wd <- getwd()
current_home <- Sys.getenv("HOME")
current_setup_start <- Sys.getenv("QPACK_SETUP_ROOT")
current_setup_folders <- Sys.getenv("QPACK_SETUP_FOLDERS")
current_setup_outside <- Sys.getenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR")

back_to_normal <- function(){

  Sys.setenv("HOME"=current_home)
  if (current_setup_start != ""){
    Sys.setenv("QPACK_SETUP_ROOT"=current_setup_start)
  }
  if (current_setup_folders != ""){
    Sys.setenv("QPACK_SETUP_FOLDERS"=current_setup_folders)
  }
  if (current_setup_outside !=""){
    Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"=current_setup_outside)
  }
  setwd(current_wd)
}


Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
Sys.setenv("QPACK_SETUP_FOLDERS"="")
Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="TRUE")
Sys.setenv("OVERRIDE_FOR_TESTING"="TRUE")

test_that("Not specifying data results in error",{
  expect_error(
    test <- create_codebook(),
    'Data must be specified.'
  )
})

test_that("Not specifying data results in error",{
  expect_error(
    test <- create_frequencies(),
    'Data must be specified.'
  )
})

test_that("Clean Run 1 - Codebook",{
  expect_silent(test1 <- create_codebook(test1))

  expect_equal("tbl_df" %in% class(test1), TRUE)
  expect_equal(names(test1), c("Number", "Variable", "Description", "Example", "Type", "Unique", "Missing", "Note"))
  expect_equal(test1[[2,2]], "q1")
  expect_equal(test1[[6,2]], "q5")
  expect_equal(test1[[8,2]], "region")
  expect_equal(test1[[2,3]], "Please indicate your age (in years).")
  expect_equal(test1[[6,3]], "Thinking back to 2020, which of the following categories best describes your total household income for that year?")
  expect_equal(test1[[8,3]], "Region")
  expect_equal(test1[[6]], c(200, 3, 3, 4, 3, 8, 4, 4, 3, 3, 3))
  on.exit(rm(test1))
})

test_that("Not specifying data results in error",{
  expect_error(
    test <- create_frequencies(),
    'Data must be specified.'
  )
})

test_that("Clean Run 2 - Frequencies",{
  expect_silent(test2 <- create_frequencies(test1))

  expect_equal(names(test2), c("key", "frequencies"))
  expect_equal(names(test2$key), c("Number", "Variable"))
  expect_equal(names(test2$frequencies), c("response_id", "q1", "q2", "q3", "q4", "q5", "q6",
                                           "region", "prob1", "prob2", "prob3"))

  test2_q6 <- test2$frequencies[["q6"]]

  expect_equal(test2_q6[[2]], c("1", "2", "3", "4", "Total"))
  expect_equal(test2_q6[[1,3]], "High School or Less")
  expect_equal(test2_q6[[5,3]], "Total")
  expect_equal(test2_q6[[4]], c(27, 88, 49, 36, 200))
  on.exit(rm(test2, test2_q6))
})

test_that("Clean Run 3 - Crosstab, freqs=THREE",{
  expect_silent(test3 <- create_codebook(test1, freqs=TRUE))

  expect_equal(names(test3), c("codebook", "frequencies"))
  expect_equal(names(test3$codebook), c("Number", "Variable", "Description", "Example", "Type", "Unique", "Missing", "Note"))
  expect_equal(names(test3$frequencies), c("response_id", "q1", "q2", "q3", "q4", "q5", "q6", "region", "prob1", "prob2", "prob3"))

  test3_q6 <- test3$frequencies[["q6"]]

  expect_equal(test3_q6[[2]], c("1", "2", "3", "4", "Total"))
  expect_equal(test3_q6[[1,3]], "High School or Less")
  expect_equal(test3_q6[[5,3]], "Total")
  expect_equal(test3_q6[[4]], c(27, 88, 49, 36, 200))
  on.exit(rm(test3, test3_q6))
})

test_that("Clean Run 4 - Files",{
  setwd(tempdir())
  expect_silent(create_codebook(test1, "test4-1.xlsx"))
  expect_true(file.exists(file.path(tempdir(), "test4-1.xlsx")))
  test4_1 <- readxl::read_xlsx(file.path(tempdir(), "test4-1.xlsx"), sheet=1)
  expect_equal(names(test4_1), c("Number", "Variable", "Description", "Example", "Type", "Unique", "Missing", "Note"))
  expect_equal(test4_1$Variable, c("response_id", "q1", "q2", "q3", "q4", "q5", "q6", "region", "prob1", "prob2", "prob3"))
  expect_equal(test4_1$Unique, c(200, 3, 3, 4, 3, 8, 4, 4, 3, 3, 3))


  expect_silent(create_frequencies(test1, "test4-2.xlsx"))
  expect_true(file.exists(file.path(tempdir(), "test4-2.xlsx")))
  test4_2 <- readxl::read_xlsx(file.path(tempdir(), "test4-2.xlsx"), sheet=2)
  expect_equal(names(test4_2), c("response_id", "value", "label", "n", "percent"))
  expect_equal(test4_2$value, c("More than 55 levels detected, frequency not generated", "Total"))
  expect_equal(test4_2$n, c(200, 200))


  expect_silent(create_codebook(test1, "test4-3.xlsx", freqs=TRUE))
  expect_true(file.exists(file.path(tempdir(), "test4-3.xlsx")))
  test4_3 <- readxl::read_xlsx(file.path(tempdir(), "test4-3.xlsx"), sheet=3)
  expect_equal(names(test4_3), c("q1", "value", "label", "n", "percent"))
  expect_equal(test4_3$label, c("18-44", "45-64", "65+", "Total"))
  expect_equal(test4_3$n, c(52, 108, 40, 200))
})

back_to_normal()

