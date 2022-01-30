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

# Expect Warning ----------------------------------------------------------

test_that("No PROJECT_PATH results in warning",{
  Sys.setenv("QPACK_SETUP_ROOT"="")
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_warning(
    set_up(project="XXX-0001",
           descriptor="Expect Warning",
           qpack=FALSE)
  )
  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

test_that("No folders specified & no QPACK_SETUP_FOLDERS in Renviron results in warning",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_warning(
    set_up(project="XXX-0001",
           descriptor="Expect Warning",
           qpack=FALSE)
  )
  on.exit(back_to_normal())
})


# Expect Errors -----------------------------------------------------------

test_that("No project specified results in error",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_error(
    set_up(project=,
           descriptor="Expect Error",
           qpack=FALSE)
  )
  on.exit(back_to_normal())
})

Sys.setenv("QPACK_SETUP_ROOT"=TRUE)

test_that("No descriptor specified results in error",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_error(
    set_up(project="XXX-0003",
           descriptor=,
           qpack=FALSE)
  )
  on.exit(back_to_normal())
})

test_that("ROOT not found results in error",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_error(
    set_up(project="XXX-0001",
           descriptor="Expect Warning",
           qpack=FALSE,
           manual_start="./notthere/")
  )
  on.exit(back_to_normal())
})

test_that("Sources not found results in error",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_error(
    set_up(project="XXX-0003",
           descriptor="Expect Error",
           source="notthere.R",
           qpack=FALSE)
  )
  on.exit(back_to_normal())
})

# Clean Run with No Errors ------------------------------------------------

test_that("Project specification results in no error",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_silent(
    set_up(project="XXX-0004",
           descriptor="Clean Run 1",
           qpack=FALSE)
  )
  on.exit(back_to_normal())
})

test_that("Client specification results in no error",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_silent(
    set_up(client="XXXXX",
           project="0005",
           descriptor="Clean Run 2",
           qpack=FALSE)
  )
  on.exit(back_to_normal())
})

test_that("Listing sources results in no error",{
  Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
  Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
  Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")
  expect_silent(
    set_up(project="XXX-0005",
           descriptor="Clean Run 3",
           qpack=FALSE)
  )
  write("x <- 25", "test.R", append=TRUE)
  expect_silent(
    set_up(project="XXX-0005",
           descriptor="Clean Run 3",
           source="test.R",
           qpack=FALSE)
  )
  on.exit(back_to_normal())
})

# Created Files & Directories ---------------------------------------------

Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="TRUE")

set_up(client="XXXXX",
       project="0005",
       descriptor="Clean Run 2",
       folders=c("data", "output"),
       qpack=FALSE)

set_up(project="XXX-0006",
       descriptor="Clean Run 6",
       folders=c("data", "output"),
       qpack=FALSE)

set_up(project="XXX-0007",
       descriptor="Clean Run 7",
       task="task1",
       folders=c("data", "output"),
       qpack=FALSE)

test_that("Project .Rproj file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0006", "XXX-0006.Rproj"))
  )
})

test_that("Project inside descriptor file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0006", "_Clean Run 6.txt"))
  )
})

test_that("Project outside descriptor file created",{
  expect_true(
    file.exists(file.path(tempdir(), "_XXX-0006 - Clean Run 6.txt"))
  )
})

test_that("Client/Project .Rproj file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "0005", "0005.Rproj"))
  )
})

test_that("Client/Number inside descriptor file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "0005", "_Clean Run 2.txt"))
  )
})

test_that("Client/Number outside descriptor file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "_0005 - Clean Run 2.txt"))
  )
})

test_that("Task inside descriptor file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0007/task1", "_Clean Run 7 (task1).txt"))
  )
})

test_that("Data directory created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0006", "data"))
  )
})

test_that("Output directory created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0006", "output"))
  )
})


test_that("Task .Rproj file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0007/task1", "XXX-0007 (task1).Rproj"))
  )
})

test_that("Task inside descriptor file created",{
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0007/task1", "_Clean Run 7 (task1).txt"))
  )
})

back_to_normal()

# Working Directory & Packages --------------------------------------------

Sys.setenv("QPACK_SETUP_ROOT"=tempdir())
Sys.setenv("QPACK_SETUP_FOLDERS"="data, output")
Sys.setenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR"="FALSE")

set_up(project="XXX-0008",
       descriptor="Clean Run 8",
       pack_load="fs", qpack=FALSE)

test_that("Working Directory Changes Appropriately",{
  skip_on_cran()
  if(assertive::is_windows()){
    exp <- fs::path(tempdir(), "XXX-0008")
  } else if (assertive::is_osx()){
    exp <- fs::path("/private", tempdir(), "XXX-0008")
  } else if (assertive::is_linux()) {

  }
  act <- fs::path(getwd())
  expect_match(exp, act)
})

test_that("fs Packaged Loaded",{
  expect_true(("fs" %in% .packages()))
  detach("package:fs")
})

expect_warning(
  set_up(project="XXX-0008",
         descriptor="Clean Run 8",
         pack_load="fs", pack_check="thereisnosuchpackage", qpack=FALSE)
)

# Manual Path -------------------------------------------------------------

dir.create(paste0(tempdir(), "/MP/"), showWarnings = FALSE)

set_up(project="XXX-00MP",
       descriptor="Manual Path Test",
       root=paste0(tempdir(), "/MP/"),
       qpack=FALSE)

test_that("Manual Path Working Directory Changes Appropriately",{
  skip_on_cran()
  if(assertive::is_windows()){
    exp <- fs::path(tempdir(), "MP", "XXX-00MP")
  } else if (assertive::is_osx()){
    exp <- fs::path("/private", tempdir(), "MP", "XXX-00MP")
  } else if (assertive::is_linux()) {

  }
  act <- fs::path(getwd())
  expect_match(exp, act)
})

back_to_normal()


