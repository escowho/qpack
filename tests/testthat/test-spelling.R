context('Spelling')

#NOTE: ../.. is necessary to point to the location of the directory for spelling
#spell_test <- spelling::spell_check_package(pkg="../..")

pkgroot <- test_package_root()
spell_test <- spelling::spell_check_package(pkgroot)

test_that("No Spelling Errors", {
  skip_on_cran()
  expect_equal(nrow(spell_test), 0)
})
