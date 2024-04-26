
# Missing requirements result in error ------------------------------------

test_that("No project specified results in error",{
  skip_on_cran()
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_error(
    set_up(project=,
           descriptor="Expect Error",
           qpack=FALSE)
  )

  expect_error(
    set_up(project=NULL,
           descriptor="Expect Error",
           qpack=FALSE)
  )

  expect_error(
    set_up(project="",
           descriptor="Expect Error",
           qpack=FALSE)
  )

  back_to_normal()
})

test_that("No descriptor specified results in error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_error(
    set_up(project="XXX-0003",
           descriptor=,
           qpack=FALSE)
  )

  expect_error(
    set_up(project="XXX-0003",
           descriptor="",
           qpack=FALSE)
  )

  expect_error(
    set_up(project="XXX-0003",
           descriptor=NULL,
           qpack=FALSE)
  )

  back_to_normal()
})

test_that("ROOT not found results in error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_error(
    set_up(project="XXX-0004",
           descriptor="Expect Error",
           qpack=FALSE,
           root="./notthere/")
  )

  back_to_normal()
})

# Missing options that result in warning ----------------------------------

test_that("No PROJECT_PATH results in warning",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = "",
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

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
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_warning(
    set_up(project="XXX-0001",
           descriptor="Expect Warning",
           qpack=FALSE)
  )

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

# Clean Project Run -------------------------------------------------------

test_that("Project with external descriptor results in no error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = TRUE,
    .local_envir = parent.frame())

  expect_silent(
    set_up(project="XXX-0001",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  expect_true(
    file.exists(file.path(tempdir(), "XXX-0001", "XXX-0001.Rproj"))
  )
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0001", "_Clean Run.txt"))
  )
  expect_true(
    file.exists(file.path(tempdir(), "_XXX-0001 - Clean Run.txt"))
  )

  skip_on_cran()
  if(Sys.info()[1]=="Windows"){
    expected_path <- fs::path(tempdir(), "XXX-0001")
  } else if (Sys.info()[1]=="Darwin"){
    expected_path <- fs::path("/private", tempdir(), "XXX-0001")
  } else {

  }

  actual_path <- fs::path(getwd())
  expect_match(expected_path, actual_path)

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
  unlink(paste0(tempdir(), "_XXX-0001 - Clean Run.txt"))
})



test_that("Project without external descriptor results in no error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_silent(
    set_up(project="XXX-0002",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  expect_true(
    file.exists(file.path(tempdir(), "XXX-0002", "XXX-0002.Rproj"))
  )
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0002", "_Clean Run.txt"))
  )
  expect_false(
    file.exists(file.path(tempdir(), "_XXX-0002 - Clean Run.txt"))
  )

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})


# Clean Client Run --------------------------------------------------------

test_that("Client specification results in no error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = TRUE,
    .local_envir = parent.frame())

  expect_silent(
    set_up(client="XXXXX",
           project="0001",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "0001", "0001.Rproj"))
  )
  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "0001", "_Clean Run.txt"))
  )
  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "_0001 - Clean Run.txt"))
  )

  skip_on_cran()
  if(Sys.info()[1]=="Windows"){
    expected_path <- fs::path(tempdir(), "XXXXX", "0001")
  } else if (Sys.info()[1]=="Darwin"){
    expected_path <- fs::path("/private", tempdir(), "XXXXX", "0001")
  } else {

  }

  actual_path <- fs::path(getwd())
  expect_match(expected_path, actual_path)

  back_to_normal()

  path <- paste0(file.path(tempdir(), "XXXXX"))
  unlink(path, recursive=TRUE)

  path <- paste0(file.path(tempdir(), "_XXX-0001 - Clean Run.txt"))
  unlink(path, recursive=TRUE)
})


# Clean Task Run ----------------------------------------------------------

test_that("Marker files are created correctly when project and task",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = TRUE,
    .local_envir = parent.frame())

  set_up(project="XXX-0009",
         task="eda",
         descriptor="Clean Run 9",
         folders=c("data", "output"),
         qpack=FALSE)

  expect_true(
    file.exists(file.path(tempdir(), "XXX-0009", "eda", "XXX-0009 (eda).Rproj"))
  )
  expect_true(
    file.exists(file.path(tempdir(), "XXX-0009", "eda", "_Clean Run 9 (eda).txt"))
  )

  #NOTE:  external descriptor not possible for tasks?
  #expect_true(
  #  file.exists(file.path(tempdir(), "_XXX-0009 - Clean Run 9 (eda).txt"))
  #)

  skip_on_cran()
  if(Sys.info()[1]=="Windows"){
    expected_path <- fs::path(tempdir(), "XXX-0009", "eda")
  } else if (Sys.info()[1]=="Darwin"){
    expected_path <- fs::path("/private", tempdir(), "XXX-0009", "eda")
  } else {

  }

  actual_path <- fs::path(getwd())
  expect_match(expected_path, actual_path)

  back_to_normal()
  path <- paste0(file.path(tempdir(), "XXX-0009"))
  unlink(path, recursive=TRUE)

})


# Testing Client & Task at same time --------------------------------------

test_that("Client specification with Task results in no error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = TRUE,
    .local_envir = parent.frame())

  expect_silent(
    set_up(client="XXXXX",
           project="0001",
           task="eda",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "0001", "eda", "0001 (eda).Rproj"))
  )
  expect_true(
    file.exists(file.path(tempdir(), "XXXXX", "0001", "eda", "_Clean Run (eda).txt"))
  )

  skip_on_cran()
  if(Sys.info()[1]=="Windows"){
    expected_path <- fs::path(tempdir(), "XXXXX", "0001", "eda")
  } else if (Sys.info()[1]=="Darwin"){
    expected_path <- fs::path("/private", tempdir(), "XXXXX", "0001", "eda")
  } else {

  }

  actual_path <- fs::path(getwd())
  expect_match(expected_path, actual_path)

  back_to_normal()

  path <- paste0(file.path(tempdir(), "XXXXX"))
  unlink(path, recursive=TRUE)

  path <- paste0(file.path(tempdir(), "_XXX-0001 - Clean Run.txt"))
  unlink(path, recursive=TRUE)
})

# Testing source files ----------------------------------------------------

test_that("Listing sources results in no error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_silent(
    set_up(project="XXX-0001",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  write("x <- 25", paste0(tempdir(), "/XXX-0001/test.R"), append=FALSE)

  expect_silent(
    set_up(project="XXX-0001",
           descriptor="Clean Run",
           source="test.R",
           qpack=FALSE)
  )

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})


test_that("Sources not found results in error",{
  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_FOLDERS = "data, output",
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_warning(
    set_up(project="XXX-0001",
           descriptor="Expect Error",
           source="notthere.R",
           qpack=FALSE)
  )

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

# Testing Package Load ----------------------------------------------------

test_that("pack_load correctly loads package",{

  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  invisible(
    qpack::set_up(project="XXX-0001",
         descriptor="Clean Run",
         pack_load="fs", qpack=FALSE)
    )

  expect_true("fs" %in% (.packages()))
  detach("package:fs")

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

test_that("pack_load correctly loads package + qpack",{

  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  set_up(project="XXX-0001",
         descriptor="Clean Run",
         pack_load="fs", qpack=TRUE, qmod=TRUE, qplot=TRUE)

  expect_true("fs" %in% (.packages()))
  expect_true("qpack" %in% (.packages()))
  expect_true("qmod" %in% (.packages()))
  expect_true("qplot" %in% (.packages()))
  detach("package:fs")
  detach("package:qpack")
  detach("package:qmod")
  detach("package:qplot")

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

test_that("pack_load throws warning if not package found",{

  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  expect_warning(
    set_up(project="XXX-0001",
           descriptor="Clean Run",
           pack_load="thereisnosuchpackage", qpack=FALSE)
  )

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

test_that("pack_check runs clean if package installed",{

  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  expect_silent(
    set_up(project="XXX-0001",
           descriptor="Clean Run",
           pack_check="fs", qpack=FALSE)
  )

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

test_that("pack_check throws warning if package not found",{

  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    OVERRIDE_FOR_TESTING = TRUE,
    .local_envir = parent.frame())

  expect_warning(
    set_up(project="XXX-0001",
           descriptor="Clean Run",
           pack_check="thereisnosuchpackage", qpack=FALSE)
  )

  expect_warning(
    set_up(project="XXX-0001",
           descriptor="Clean Run",
           pack_check=c("thereisnosuchpackage", "alsonotapackage"), qpack=FALSE)
  )

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})

# Manual Path -------------------------------------------------------------

test_that("Manual Path Not Found Triggers Error",{

  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  #dir.create(paste0(tempdir(), "/MP/"), showWarnings = FALSE)

  expect_error(
    set_up(project="XXX-00MP",
           descriptor="Manual Path Test",
           root=paste0(tempdir(), "/MP/"),
           qpack=FALSE)
  )

  back_to_normal()
})

test_that("Manual Path Working Directory Changes Appropriately",{

  withr:::local_envvar(
    QPACK_SETUP_TEST = TRUE,
    QPACK_SETUP_ROOT = tempdir(),
    QPACK_SETUP_EXTERNAL_DESCRIPTOR = FALSE,
    .local_envir = parent.frame())

  dir.create(paste0(tempdir(), "/MP/"), showWarnings = FALSE)

  expect_silent(
    set_up(project="XXX-00MP",
           descriptor="Manual Path Test",
           root=paste0(tempdir(), "/MP/"),
           qpack=FALSE)
  )

  skip_on_cran()
  if(Sys.info()[1]=="Windows"){
    expected_path <- fs::path(tempdir(), "MP", "XXX-00MP")
  } else if (Sys.info()[1]=="Darwin"){
    expected_path <- fs::path("/private", tempdir(), "MP", "XXX-00MP")
  } else {

  }
  actual_path <- fs::path(getwd())
  expect_match(expected_path, actual_path)

  back_to_normal()
  path <- paste0(file.path(tempdir(), "MP"))
  unlink(path, recursive=TRUE)
})


# qkey tests --------------------------------------------------------------

test_that("Testing qkey file",{
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
    create_qkey(name="test_ring", url="test_url", token="12345")
  )

  expect_warning(
    create_qkey(name="test_ring", url="test_url", token="12345")
  )

  back_to_normal()

  expect_silent(
    set_up(root=tempdir(),
           project="X0001",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  expect_match("12345", Sys.getenv("QUALTRICS_API_KEY"))
  expect_match("test_url", Sys.getenv("QUALTRICS_BASE_URL"))

  clear_qkey()

  expect_match("", Sys.getenv("QUALTRICS_API_KEY"))
  expect_match("", Sys.getenv("QUALTRICS_BASE_URL"))

  expect_silent(
    create_qkey(name="test_ring2", url="test_url", token="12345")
  )

  expect_warning(
    set_up(root=tempdir(),
           project="X0001",
           descriptor="Clean Run",
           qpack=FALSE)
  )

  if (sum(stringr::str_detect(keyring::key_list()$service, "test_ring")==1)){
    keyring::key_delete("test_ring")
  }

  if (sum(stringr::str_detect(keyring::key_list()$service, "test_ring2")==1)){
    keyring::key_delete("test_ring2")
  }

  path <- getwd()
  back_to_normal()
  unlink(path, recursive=TRUE)
})
