# addins ------------------------------------------------------------------

#NOTE:  These are run interactively only, they do not count in COVR report

test_that("Default project addin results in corect text",{
  skip_if_not(interactive())

  withr:::local_envvar(
    QPACK_ADDIN_MATT = FALSE,
    .local_envir = parent.frame())

  file_path <- fs::path(tempdir(), "test1.txt")
  fs::file_create(file_path)
  invisible(rstudioapi::navigateToFile(file_path))
  addin_1_project_setup()
  invisible(rstudioapi::documentSave())
  invisible(rstudioapi::documentClose())

  test <- readLines(file_path)
  expect_equal(length(test), 8)
  expect_equal(test[7], "#==============================================================================#")
})

test_that("Matt project addin results in corect text",{
  skip_if_not(interactive())

  withr:::local_envvar(
    QPACK_ADDIN_MATT = TRUE,
    .local_envir = parent.frame())

  file_path <- fs::path(tempdir(), "test2.txt")
  fs::file_create(file_path)
  invisible(rstudioapi::navigateToFile(file_path))
  addin_1_project_setup()
  invisible(rstudioapi::documentSave())
  invisible(rstudioapi::documentClose())

  test <- readLines(file_path)
  expect_equal(length(test), 23)
  expect_equal(test[12], "# Cleanup -----------------------------------------------------------------")
})
