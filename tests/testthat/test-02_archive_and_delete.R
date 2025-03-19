set_normal <- function() {

  if (interactive()==TRUE) {
    current_settings <<- list(
      current_wd=rprojroot::find_root("DESCRIPTION"),
      current_home=Sys.getenv("HOME"),
      current_setup_test=Sys.getenv("QPACK_SETUP_TEST"),
      current_setup_start=Sys.getenv("QPACK_SETUP_ROOT"),
      current_setup_folders=Sys.getenv("QPACK_SETUP_FOLDERS"),
      current_setup_outside=Sys.getenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"))
  } else {
    current_settings <<- list(
      current_wd=getwd(),
      current_home=Sys.getenv("HOME"),
      current_setup_test=Sys.getenv("QPACK_SETUP_TEST"),
      current_setup_start=Sys.getenv("QPACK_SETUP_ROOT"),
      current_setup_folders=Sys.getenv("QPACK_SETUP_FOLDERS"),
      current_setup_outside=Sys.getenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"))
  }
}
set_normal()

back_to_normal <- function(){
  Sys.setenv("HOME"=current_settings$current_home)
  if (current_settings$current_setup_start != ""){
    Sys.setenv("QPACK_SETUP_ROOT"=current_settings$current_setup_start)
  }
  if (current_settings$current_setup_test != ""){
    Sys.setenv("QPACK_SETUP_TEST"=current_settings$current_setup_test)
  }
  if (current_settings$current_setup_folders != ""){
    Sys.setenv("QPACK_SETUP_FOLDERS"=current_settings$current_setup_folders)
  }
  if (current_settings$current_setup_outside !=""){
    Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"=current_settings$current_setup_outside)
  }
  setwd(current_settings$current_wd)
}


test_that("Archive & Delete Function for Project File",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_no_error(
     qpack::set_up(
                project="TAD-0001",
                descriptor="Testing Archive and Delete",
                qpack=FALSE)
  )

  caddat <- qpack::caddat
  saveRDS(caddat, "./data/test.rds")

  path <- getwd()
  setwd(tempdir())

  expect_error(
    archive_project(project=)
    )

  expect_no_error(
    archive_project(project="TAD-0001")
  )

  unlink("TAD-0001.zip")

  expect_error(
    delete_project(project="TAD-0001"),
  )

  expect_no_error(
    archive_project(project="TAD-0001")
  )

  expect_true(
    file.exists("TAD-0001.zip")
  )

  expect_error(
    delete_project(project=),
    )

  expect_error(
    delete_project(project="TAD-0001", root="./notthere/"),
  )

  expect_silent(
    delete_project(project="TAD-0001", override_archive_check=TRUE)
  )

  unlink("TAD-0001.zip")
  back_to_normal()
})

test_that("Archive Data Option works",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_no_error(
    qpack::set_up(
      project="ZAD-0001",
      descriptor="Testing Archive and Delete",
      qpack=FALSE)
  )

  caddat <- qpack::caddat
  saveRDS(caddat, "./data/test.rds")

  path <- getwd()
  setwd(tempdir())

  expect_no_error(
    archive_project(project="ZAD-0001", data=TRUE)
  )

  expect_true(
    file.exists("ZAD-0001.zip")
  )

  expect_no_error(
    delete_project(project="ZAD-0001", override_archive_check=TRUE)
  )

  unzip("ZAD-0001.zip")

  expect_true(
    file.exists("ZAD-0001/data/test.rds")
    )

  expect_no_error(
    delete_project(project="ZAD-0001", override_archive_check=TRUE)
  )

  unlink("ZAD-0001.zip")

  expect_no_error(
    qpack::set_up(
      project="ZAD-0001",
      descriptor="Testing Archive and Delete",
      qpack=FALSE)
  )

  caddat <- qpack::caddat
  saveRDS(caddat, "./data/test.rds")

  path <- getwd()
  setwd(tempdir())

  expect_no_error(
    archive_project(project="ZAD-0001")
  )

  expect_true(
    file.exists("ZAD-0001.zip")
  )

  expect_no_error(
    delete_project(project="ZAD-0001", override_archive_check=TRUE)
  )

  unzip("ZAD-0001.zip")

  expect_false(
    file.exists("./ZAD-0001/data/test.rds")
  )

  expect_no_error(
    delete_project(project="ZAD-0001", override_archive_check=TRUE)
  )

  unlink("TAD-0001.zip")
  back_to_normal()

})

test_that("Archive & Delete Function for Project File with External File",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = TRUE,
    .local_envir = parent.frame())

  expect_no_error(
    qpack::set_up(
      project="TAD-0001",
      descriptor="Testing Archive and Delete",
      qpack=FALSE)
  )

  path <- getwd()
  setwd(tempdir())

  expect_no_error(
    archive_project(project="TAD-0001")
  )

  expect_true(
    file.exists("TAD-0001.zip")
  )

  expect_silent(
    delete_project(project="TAD-0001", override_archive_check=TRUE)
  )

  unlink("TAD-0001.zip")
  back_to_normal()
})

test_that("Archive Function for Client File",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_no_error(
    qpack::set_up(client="TAD",
                project="0001",
                descriptor="Testing Archive and Delete",
                qpack=FALSE)
  )

  caddat <- qpack::caddat
  saveRDS(caddat, "./data/test.rds")

  path <- getwd()
  setwd(tempdir())

  expect_error(
    archive_project(client="TAD")
  )

  expect_no_error(
    archive_project(client="TAD", project="0001")
  )

  expect_true(
    file.exists(file.path(tempdir(), "TAD", "TAD 0001.zip"))
  )

  expect_silent(
    delete_project(client="TAD", project="0001", override_archive_check=TRUE)
  )

  unlink("TAD")
  back_to_normal()
})

test_that("Archive & Delete Function for No Default Root",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = "",
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_warning(
    qpack::set_up(
      project="test",
      descriptor="test",
      qpack=FALSE)
  )

  x <- fs::path_split(fs::path_wd())[[1]]
  setwd(fs::path_join(x[1:length(x)-1]))

  expect_warning(archive_project("test"))
  expect_warning(delete_project("test"))

  back_to_normal()
})

