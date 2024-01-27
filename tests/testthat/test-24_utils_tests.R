# utils -------------------------------------------------------------------

test_that("version returns current version", {
  expect_equal(qpack:::version(), packageVersion("qpack"))
})

test_that("skeleton returns correct text",{

  x <- capture.output(qpack::skeleton())
  expect_equal(x[2], "qpack::set_up(client = \"\",")
  expect_equal(x[3], "              project = \"\",")
  expect_equal(x[4], "              task = \"\",")
  expect_equal(x[5], "              root = \"\",")

})

# refresh -----------------------------------------------------------------

test_that("update works correctly using QPACK_TEST option", {
  withr:::local_envvar(
    QPACK_TEST = TRUE,
    .local_envir = parent.frame())

  expect_equal(qpack:::refresh(), "update_pack")
  expect_equal(qpack:::refresh(dev=TRUE), "update_dev")

})
