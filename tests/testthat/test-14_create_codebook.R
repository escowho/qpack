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

test_that("Clean Run 1",{
  expect_silent(test1 <- create_codebook(test1))

  expect_equal("tbl_df" %in% class(test1), TRUE)
  expect_equal(names(test1), c("Column", "Description", "Example", "Type", "Unique", "Missing", "Note"))
  expect_equal(test1[[2,1]], "q1")
  expect_equal(test1[[6,1]], "q5")
  expect_equal(test1[[8,1]], "region")
  expect_equal(test1[[2,2]], "Please indicate your age (in years).")
  expect_equal(test1[[6,2]], "Thinking back to 2020, which of the following categories best describes your total household income for that year?")
  expect_equal(test1[[8,2]], "Region")
  expect_equal(test1[[5]], c(200, 3, 3, 4, 3, 8, 4, 4, 3, 3, 3))
  on.exit(rm(test1))
})

test_that("Clean Run 2 - Frequencies",{
  expect_silent(test2 <- create_codebook(test1, freqs=TRUE))

  expect_equal(names(test2), c("codebook", "frequencies"))
  expect_equal(names(test2$codebook), c("Column", "Description", "Example", "Type", "Unique", "Missing", "Note"))
  expect_equal(names(test2$frequencies), c("response_id", "q1", "q2", "q3", "q4", "q5", "q6",
                                           "region", "prob1", "prob2", "prob3"))

  test2_q6 <- test2$frequencies[["q6"]]

  expect_equal(test2_q6[[2]], c("1", "2", "3", "4", "Total"))
  expect_equal(test2_q6[[1,3]], "High School or Less")
  expect_equal(test2_q6[[5,3]], "Total")
  expect_equal(test2_q6[[4]], c(27, 88, 49, 36, 200))
  on.exit(rm(test2, test2_q6))
})

setwd(tempdir())

test_that("Clean Run 3 - Files",{
  expect_silent(create_codebook(test1, "test3.xlsx", freqs=TRUE))
  expect_true(file.exists(file.path(tempdir(), "test3.xlsx")))
  expect_true(file.exists(file.path(tempdir(), "test3 - Frequencies.xlsx")))
})

test_that("Clean Run 4 - Long Names",{
  names(test1) <- paste0(names(test1), "__asdfghjklqwertyuiopzxcvbnmas")

  expect_silent(create_codebook(test1, "test4.xlsx", freqs=TRUE))
  expect_true(file.exists(file.path(tempdir(), "test4.xlsx")))
  expect_true(file.exists(file.path(tempdir(), "test4 - Frequencies.xlsx")))

  test4_1 <- readxl::read_xlsx(file.path(tempdir(), "test4 - Frequencies.xlsx"), sheet=1)

  expect_equal(test4_1[[2,1]], "q1__")
  expect_equal(test4_1[[7,2]], "q6__asdfghjklqwertyuiopzxcvbnmas")

  test4_3 <- readxl::read_xlsx(file.path(tempdir(), "test4 - Frequencies.xlsx"), sheet=3)

  expect_equal(names(test4_3), c("q1__asdfghjklqwertyuiopzxcvbnmas",
                                 "value", "label", "n", "percent"))
  expect_equal(test4_3[[2]], c("1", "2", "3", "Total"))
  expect_equal(test4_3[[2,3]], "45-64")
  expect_equal(test4_3[[4,3]], "Total")
  expect_equal(test4_3[[4]], c(52, 108, 40, 200))
  on.exit(rm(test4_1, test4_3))
})





back_to_normal()

