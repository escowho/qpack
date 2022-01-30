# Expect Errors -----------------------------------------------------------

test_that("Not specifying data results in error",{
  expect_error(
    test <- freq()
  )
})

test_that("Not specifying variable results in error",{
  expect_error(
    test <- freq(caddat)
  )
})

test_that("File not found will result in error",{
  expect_error(
    test <- freq(caddat2, speclty)
  )
})


# Clean Run with No Errors ------------------------------------------------

test_that("Minimum specifications results in no error",{
  expect_silent(
    test <- freq(caddat, speclty)
  )
  good <- tibble::tribble(~`speclty`, ~`n`, ~`percent`,
                          "1", 106, "29.3%",
                          "2", 92, "25.4%",
                          "3", 79, "21.8%",
                          "4", 50, "13.8%",
                          "5", 35, "9.7%",
                          "Total", 362, "100.0%")

  expect_equivalent(test, good)
  on.exit(rm(test))
})


# Expect Errors -----------------------------------------------------------

test_that("Not specifying data results in error",{
  expect_error(
    test <- crosstab()
  )
})

test_that("Not specifying variable results in error",{
  expect_error(
    test <- crosstab(caddat)
  )
})

test_that("File not found will result in error",{
  expect_error(
    test <- crosstab(caddat2, q19_1, q19_2)
  )
})


# Clean Run with No Errors ------------------------------------------------

test_that("Minimum specifications results in no error",{
  expect_silent(
    test <- crosstab(caddat, q19_1, q19_2)
  )
  expect_equivalent(test[c(1:9), 2], c("2", "0", "0", "0", "0", "0", "1", "0", "1"))
  expect_equivalent(test[c(11:19), 5], c("5", "0.0%", "0.0%", "20.0%", "18.2%", "22.2%", "7.4%", "5.1%", "11.6%"))
  expect_equivalent(test[c(21:29), 7], c("7", "0.0%", "1.1%", "1.1%", "7.9%", "25.8%", "64.0%", "0.0%", "100.0%"))
  expect_equivalent(test[c(31:39), 8], c("Total", "0.3%", "0.3%", "1.4%", "6.1%", "24.9%", "56.4%", "10.8%", "100.0%"))

  on.exit(rm(test))
})

# Expect Errors -----------------------------------------------------------

test_that("Not specifying data results in error",{
  expect_error(
    test <- threeway()
  )
})

test_that("Not specifying variable results in error",{
  expect_error(
    test <- threeway(caddat)
  )
})

test_that("File not found will result in error",{
  expect_error(
    test <- threeway(caddat2, q19_1, q19_2, speclty)
  )
})


# Clean Run with No Errors ------------------------------------------------

test_that("Minimum specifications results in no error",{
  expect_silent(
    test <- threeway(caddat, q19_1, q19_2, speclty)
  )

  expect_equivalent(test[c(2:10), 2],c("2","0","0","0","0","0","0","0","0"))
  expect_equivalent(test[c(2:10), 8],c("Total","0","1","3","8","24","57","13","106"))
  expect_equivalent(test[c(2:10), 13],c("4","0","0","0","0","1","1","1","3"))
  expect_equivalent(test[c(2:10), 17],c("Total","0","0","2","3","15","62","10","92"))
  expect_equivalent(test[c(2:10), 21],c("3","0","0","0","0","0","0","1","1"))
  expect_equivalent(test[c(2:10), 29],c("2","0","0","0","0","0","0","0","0"))
  expect_equivalent(test[c(2:10), 26],c("Total","1","0","0","3","30","40","5","79"))
  expect_equivalent(test[c(2:10), 35],c("Total","0","0","0","0","4","36","10","50"))
  expect_equivalent(test[c(2:10), 43],c("7","0","0","0","3","4","2","0","9"))
  expect_equivalent(test[c(2:10), 44],c("Total","0","0","0","8","17","9","1","35"))

  expect_equivalent(test[c(12:20), 5],c("5","-","0.0%","33.3%","12.5%","25.0%","8.8%","0.0%","12.3%"))
  expect_equivalent(test[c(12:20), 14],c("5","-","-","0.0%","33.3%","13.3%","8.1%","10.0%","9.8%"))

  expect_equivalent(test[c(12:20), 22],c("4","0.0%","-","-","0.0%","10.0%","0.0%","40.0%","6.3%"))
  expect_equivalent(test[c(12:20), 34],c("7","-","-","-","-","25.0%","33.3%","0.0%","26.0%"))

  expect_equivalent(test[c(22:30), 29],c("2","-","-","-","-","-","-","-","-"))
  expect_equivalent(test[c(22:30), 33],c("6","0.0%","0.0%","0.0%","0.0%","5.9%","67.6%","26.5%","100.0%"))

  expect_equivalent(test[c(32:40), 8],c("Total","0.0%","0.9%","2.8%","7.5%","22.6%","53.8%","12.3%","100.0%"))
  expect_equivalent(test[c(32:40), 44],c("Total","0.0%","0.0%","0.0%","22.9%","48.6%","25.7%","2.9%","100.0%"))

  on.exit(rm(test))
})
