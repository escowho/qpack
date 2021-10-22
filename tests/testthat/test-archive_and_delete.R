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



qpack::set_up(project="TAD-0001",
              descriptor="Testing Archive and Delete",
              qpack=FALSE)

qpack::set_up(client="TAD",
              project="0001",
              descriptor="Testing Archive and Delete",
              qpack=FALSE)

# Archive Expect Errors ---------------------------------------------------
context("archive_project: Testing Archive Assertion Errors")

test_that("No project specified results in error",{
    expect_error(
      archive_project(project=, silent=TRUE),
      'A project must be specified.'
    )
  })

# Delete Expect Errors ----------------------------------------------------
context("delete_project: Testing Delete Assertion Errors")
test_that("No project specified results in error",{
  expect_error(
    delete_project(project=),
    'A project must be specified.'
  )
})

# Archive Clean Run with No Errors ----------------------------------------
context("archive_project: Archive Clean Run with No Errors")

test_that("Minimum specifications results in no error",{
  expect_silent(
    archive_project(project="TAD-0001", silent=TRUE)
  )
})

test_that("Client/Number specifications results in no error",{
  expect_silent(
    archive_project(client="TAD", project="0001", silent=TRUE)
  )
})

test_that("Project .zip file created",{
  expect_true(
    file.exists(file.path(tempdir(), "TAD-0001.zip"))
  )
})

test_that("Client/Number .zip file created",{
  expect_true(
    file.exists(file.path(tempdir(), "TAD", "TAD 0001.zip"))
  )
})

setwd(tempdir())

# Delete Clean Run with No Errors -----------------------------------------
context("delete_project: Delete Clean Run with No Errors")
test_that("Minimum specifications results in no error",{
  expect_silent(
    delete_project(project="TAD-0001", override_archive_check=TRUE)
  )
})

test_that("Client/Number specifications results in no error",{
  expect_silent(
    delete_project(client="TAD", project="0001", override_archive_check=TRUE)
  )
})

test_that("Project .zip file created",{
  expect_false(
    file.exists(file.path(tempdir(), "TAD-0001"))
  )
})

test_that("Client/Number .zip file created",{
  expect_false(
    file.exists(file.path(tempdir(), "TAD", "0001"))
  )
})

on.exit(back_to_normal())


