
# addins ------------------------------------------------------------------

test_that("project addin results in corect text",{
  skip_if_not(interactive())

  file_path <- fs::path(tempdir(), "test1.txt")
  fs::file_create(file_path)
  invisible(rstudioapi::navigateToFile(file_path))
  addin_1_project_setup()
  invisible(rstudioapi::documentSave())
  invisible(rstudioapi::documentClose())

  test1 <- readLines(file_path)
  expect_equal(test1[2], "qpack::set_up(project = \"\"," )
})

test_that("project addin results in corect text",{
  skip_if_not(interactive())

  file_path <- fs::path(tempdir(), "test2.txt")
  fs::file_create(file_path)
  invisible(rstudioapi::navigateToFile(file_path))
  addin_2_project_setup()
  invisible(rstudioapi::documentSave())
  invisible(rstudioapi::documentClose())

  test2 <- readLines(file_path)
  expect_equal(test2[2], "qpack::set_up(client = \"\"," )
})


# utils -------------------------------------------------------------------

test_that("version returns current version", {
  expect_equal(version(), packageVersion("qpack"))
})

test_that("skeleton returns correct text",{

  x <- capture.output(skeleton())
  expect_equal(x[2], "qpack::set_up(client = \"\",")
  expect_equal(x[3], "              project = \"\",")
  expect_equal(x[4], "              task = \"\",")
  expect_equal(x[5], "              root = \"\",")

})
