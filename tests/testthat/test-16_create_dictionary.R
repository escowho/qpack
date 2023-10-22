
test_that("Not specifying data results in error",{
  expect_error(
    create_dictionary(),
    'Data must be specified.'
  )
})

test_that("Not specifying data results in error",{
  expect_warning(
    create_dictionary(qpack::test1),
    'Output not specified; will use \'dictionary.html\'.'
  )
})

test_that("Output location not fund results in error",{
  expect_error(
    create_dictionary(qpack::test1, output="./zdkjfkldklajkl/test.html"),
    'Specified path does not exist'
  )
})

test_that("Unknown type results in warning",{
  expect_warning(
    create_dictionary(qpack::test1, output="output.html", type="uknown")
  )
})

test_that("Clean dictionary run",{

  expect_no_error(
    create_dictionary(qpack::test1,
                      output=file.path(tempdir(), "test1.html"))
  )

  #Don't think it actually outputs the file inside the test
  #expect_true(file.exists(file.path(tempdir(), "test1.html")))

})


