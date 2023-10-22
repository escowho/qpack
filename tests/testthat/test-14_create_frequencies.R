test_that("Not specifying data results in error",{
  expect_error(
    test <- create_frequencies(),
    'Data must be specified.'
  )
})

test_that("Clean Run 1 - Frequencies",{

  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  expect_no_error(test2 <- create_frequencies(qpack::test1))

  expect_equal(names(test2), c("key", "frequencies"))
  expect_equal(names(test2$key), c("Number", "Variable"))
  expect_equal(names(test2$frequencies), c("response_id", "q1", "q2", "q3", "q4", "q5", "q6",
                                           "region", "prob1", "prob2", "prob3"))

  test2_q6 <- test2$frequencies[["q6"]]

  expect_equal(test2_q6[[2]], c("1", "2", "3", "4", "Total"))
  expect_equal(test2_q6[[1,3]], "High School or Less")
  expect_equal(test2_q6[[5,3]], "Total")
  expect_equal(test2_q6[[4]], c(27, 88, 49, 36, 200))
})

test_that("Clean Run 2 - Files",{
  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  withr::local_dir(tempdir())

  expect_no_error(create_frequencies(qpack::test2, output="test4-2.xlsx"))
  expect_true(file.exists(file.path(tempdir(), "test4-2.xlsx")))
  test4_2 <- readxl::read_xlsx(file.path(tempdir(), "test4-2.xlsx"), sheet=2)
  expect_equal(names(test4_2), c("q1", "VALUE", "label", "n", "percent"))
  expect_equal(test4_2$VALUE, c("1", "2", "3", "4","Total"))
  expect_equal(test4_2$n, c(44,58,51,47,200))
})

test_that("Clean Run 5 - Metadata",{
  withr:::local_envvar(
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  withr::local_dir(tempdir())
  expect_no_error(create_frequencies(qpack::test2, metadata=qpack::meta2, output="test5-1.xlsx"))
  expect_true(file.exists(file.path(tempdir(), "test5-1.xlsx")))
  test5_1 <- readxl::read_xlsx(file.path(tempdir(), "test5-1.xlsx"), sheet=1)
  expect_equal(test5_1$Variable, c("q1", "q2", "q3_nps_group", "q3"))

  test5_2 <- readxl::read_xlsx(file.path(tempdir(), "test5-1.xlsx"), sheet=2)
  expect_equal(names(test5_2), c("q1", "VALUE", "label", "n", "percent"))
  expect_equal(test5_2$label, c("18 to 24", "25 to 39", "40 to 59", "60 Plus", "Total"))
  expect_equal(test5_2$n, c(44, 58, 51, 47, 200))
})
