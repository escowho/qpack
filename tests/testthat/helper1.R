test_package_root <- function() {
  x <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  pkg <- testthat::testing_package()
  x <- tryCatch(
    rprojroot::find_package_root_file(
      path = file.path("..", "..", "00_pkg_src", pkg)),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  stop("Cannot find package root")
}

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
