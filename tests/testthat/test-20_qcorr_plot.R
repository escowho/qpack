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
Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="TRUE")
Sys.setenv("OVERRIDE_FOR_TESTING"="TRUE")

setwd(tempdir())

# Expected Errors ---------------------------------------------------------

test_that("No data specified results in error",{
  expect_error(
    qcor_plot(),
    'Data must be specified.'
  )
})

test_that("Mismatched number of names to variables results in error",{
  expect_error(
    test1 %>%
      select(q1:q6) %>%
      qcor_plot(.,
                names=c("Age", "Gender", "Race", "Insurance", "Income", "Education",
                        "nope")),
    'Number of names specified not equal to number of columns in data'
  )
})


# Expected Warnings -------------------------------------------------------

test_that("Use of first option results in warning about sort",{
  expect_warning(
    test1 %>%
      select(q1:q6) %>%
      qcor_plot(data=., first="q5", sort=TRUE),
    'Can\'t sort output since the first option is being used.'
  )
})


# Clean runs with no errors -----------------------------------------------

test_that("Files output correctly with no error",{
  expect_silent(test1 %>%
                  select(q1:q6) %>%
                  qcor_plot(data=., output="test1.jpg"))
  expect_true(file.exists(file.path(tempdir(), "test1.xlsx")))
  expect_true(file.exists(file.path(tempdir(), "test1.jpg")))
  test1 <- readxl::read_xlsx(file.path(tempdir(), "test1.xlsx"), sheet=1)
  expect_equal(names(test1), c("variable", "q1", "q2", "q3", "q4", "q5", "q6"))
  expect_equal(test1$q1, c(1.00, .035, -.168, .144, -.001, .044), tol=.01)
  test2 <- readxl::read_xlsx(file.path(tempdir(), "test1.xlsx"), sheet=2)
  expect_equal(test2$q3, c(.212, 1, 0, .715, .198, .706), tol=.01)
  on.exit(rm(test1, test2))
})

test_that("Supply names results in no error",{
  expect_silent(
    test1 %>%
      select(q1:q6) %>%
      qcor_plot(data=.,
                names=c("Age", "Gender", "Race", "Insurance", "Income", "Education"))
  )
})

test_that("Use of first results in no error with sort=FALSE",{
  expect_silent(
    test1 %>%
      select(q1:q6) %>%
      qcor_plot(data=., first="q5", sort=FALSE)
  )
})

test_that("Supplied colors results in no error",{
  expect_silent(
    test1 %>%
      select(q1:q6) %>%
      qcor_plot(data=.,
                colors=c("#1a9635", "#62af6c", "#99c69f", "#d0dcd2",
                         "#ffffff",
                         "#fde1e1", "#ffaba8", "#ff7269", "#ff2424"))
  )
})

test_that("Compare 1",{
  p1 <- test1 %>%
    select(q1:q6) %>%
    qcor_plot(data=.)
  vdiffr::expect_doppelganger("c1", p1)
})

test_that("Compare 2",{
  p2 <- test1 %>%
    select(q1:q6) %>%
    qcor_plot(data=.,
              names=c("Age", "Gender", "Race", "Insurance", "Income", "Education"))
  vdiffr::expect_doppelganger("c2", p2)
})

test_that("Compare 3",{
  p3 <- test1 %>%
    select(q1:q6) %>%
  qcor_plot(data=.,
            colors=c("#1a9635", "#62af6c", "#99c69f", "#d0dcd2",
                              "#ffffff",
                              "#fde1e1", "#ffaba8", "#ff7269", "#ff2424"))
  vdiffr::expect_doppelganger("c3", p3)
})

test_that("Compare 4",{
  p4 <- test1 %>%
    select(q1:q6) %>%
    qcor_plot(data=., first="q5", sort=FALSE)
  vdiffr::expect_doppelganger("c4", p4)
})

on.exit(back_to_normal())


