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
    create_dictionary(),
    'Data must be specified.'
  )
})

test_that("Not specifying data results in error",{
  expect_warning(
    create_dictionary(test1),
    'Output not specified; will use \'dictionary.html\'.'
  )
})

test_that("Output location not fund results in error",{
  expect_error(
    create_dictionary(test1, output="./zdkjfkldklajkl/test.html"),
    'Specified path does not exist'
  )
})

test_that("Unknown type results in warning",{
  expect_warning(
    create_dictionary(test1, output="output.html", type="uknown"),
    'Type specified not known, will use default instead:'
  )
})

#file.exists keeps failing during test but not when run individually
#setwd(tempdir())
#create_dictionary(test1, output=file.path(tempdir(), "test1.html"))

#test_that("Find test1.html",{
#  expect_true(
#    file.exists(file.path(tempdir(), "test1.html"))
#    )
#})

back_to_normal()

