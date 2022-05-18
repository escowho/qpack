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


test_that("Clean Run",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  Sys.setenv("OVERRIDE_FOR_TESTING"="TRUE")

  set_up(project="p1", descriptor="xyz", qpack=FALSE)

  expect_equal(Sys.getenv("QUALTRICS_API_KEY"), "")
  expect_equal(Sys.getenv("QUALTRICS_BASE_URL"), "")

  expect_silent(create_qconfig(key="12345", url="abcde"))
  expect_true(file.exists(".qconfig"))

  set_up(project="p1", descriptor="xyz", qpack=FALSE)

  expect_equal(Sys.getenv("QUALTRICS_API_KEY"), "12345")
  expect_equal(Sys.getenv("QUALTRICS_BASE_URL"), "abcde")

  expect_message(delete_qconfig(), "qconfig deleted.")
  expect_false(file.exists(file.path(tempdir(), ".qconfig")))

})

back_to_normal()
Sys.unsetenv("QUALTRICS_API_KEY")
Sys.unsetenv("QUALTRICS_BASE_URL")

