
# archive_email and achive_email within archive ---------------------------


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

