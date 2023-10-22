
test_that("Not specifying data results in error",{
  expect_error(
    test <- create_codebook(),
    'Data must be specified.'
  )
})

test_that("Clean Run 1 - Codebook",{
  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  expect_no_error(test1 <- create_codebook(qpack::test1))

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

test_that("Clean Run 2 - Crosstab, freqs=THREE",{
  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  expect_no_error(test3 <- create_codebook(qpack::test1, freqs=TRUE))

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

test_that("Clean Run 3 - Files",{
  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  withr::local_dir(tempdir())

  expect_no_error(create_codebook(qpack::test1, output="test4-1.xlsx"))
  expect_true(file.exists(file.path(tempdir(), "test4-1.xlsx")))
  test4_1 <- readxl::read_xlsx(file.path(tempdir(), "test4-1.xlsx"), sheet=1)
  expect_equal(names(test4_1), c("Number", "Variable", "Description", "Example", "Type", "Unique", "Missing", "Note"))
  expect_equal(test4_1$Variable, c("response_id", "q1", "q2", "q3", "q4", "q5", "q6", "region", "prob1", "prob2", "prob3"))
  expect_equal(test4_1$Unique, c(200, 3, 3, 4, 3, 8, 4, 4, 3, 3, 3))
})

test_that("Clean Run 4 - Files with freqs",{
  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  withr::local_dir(tempdir())

  expect_no_error(create_codebook(qpack::test2, output="test4-3.xlsx", freqs=TRUE))
  expect_true(file.exists(file.path(tempdir(), "test4-3.xlsx")))
  test4_3 <- readxl::read_xlsx(file.path(tempdir(), "test4-3.xlsx"), sheet=3)
  expect_equal(names(test4_3), c("q2", "VALUE", "label", "n", "percent"))
  expect_equal(test4_3$label, c(rep(NA, 5)))
  expect_equal(test4_3$n, c(57,41,49,53,200))
})

test_that("Clean Run 5 - Metadata",{
  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  withr::local_dir(tempdir())

  expect_no_error(create_codebook(qpack::test2, metadata=qpack::meta2, output="test5-1.xlsx", freqs=TRUE))
  expect_true(file.exists(file.path(tempdir(), "test5-1.xlsx")))
  test5_1 <- readxl::read_xlsx(file.path(tempdir(), "test5-1.xlsx"), sheet=1)
  expect_equal(names(test5_1), c("Number", "Variable", "Description", "Example", "Type", "Unique", "Missing", "Note"))
  expect_equal(test5_1$Variable, c("q1", "q2", "q3_nps_group", "q3"))
  expect_equal(test5_1$Unique, c(4,4,3,11))
})

