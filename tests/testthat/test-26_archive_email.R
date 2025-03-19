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

test_that("Archive Email works",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_no_error(
    qpack::set_up(
      project="EMAIL-0003",
      folders=c("data", "output"),
      descriptor="Testing Email Archive",
      qpack=FALSE)
  )

  expect_error(
    archive_email()
  )

  expect_no_error(
    qpack::set_up(
      project="EMAIL-0004",
      folders=c("data", "email", "output"),
      descriptor="Testing Email Archive",
      qpack=FALSE)
  )

  expect_error(
    archive_email()
  )

  caddat <- qpack::caddat
  write.table(caddat, "./email/email1.txt")

  expect_no_error(
    archive_email(delete=FALSE)
  )

  expect_true(
    file.exists("./email/email1.txt")
  )

  expect_true(
    file.exists("./email/email.zip")
  )

  unlink("./email/email.zip")

  expect_no_error(
    archive_email()
  )

  expect_false(
    file.exists("./email/email1.rds")
  )

  expect_true(
    file.exists("./email/email.zip")
  )

  back_to_normal()
})

