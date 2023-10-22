
#NOTE: Most qkey tests are included in test-01-set_up, this just fills out covr


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
