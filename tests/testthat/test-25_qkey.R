
#NOTE: Most qkey tests are included in test-01-set_up, this just fills out covr

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


# show_token --------------------------------------------------------------

test_that("Testing qkey file",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_silent(
    set_up(root=tempdir(),
           project="X0001",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  expect_silent(
    create_qkey(name="X0001", url="test_url", token="12345")
  )

  expect_error(show_token())

  expect_match(show_token("X0001"), "12345")

  if (sum(stringr::str_detect(keyring::key_list()$service, "X0001")==1)){
    keyring::key_delete("X0001")
  }

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})



# get_qkey ----------------------------------------------------------------

test_that("error messages from get_key",{
  expect_error(get_qkey())
  expect_error(get_qkey("ZzZzZzZz"))
})
