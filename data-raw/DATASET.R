caddat <- readRDS("./data-raw/caddat.rds")
usethis::use_data(caddat, overwrite=TRUE)

test1 <- haven::read_spss(system.file("extdata", "test1.sav", package = "cxm"))
usethis::use_data(test1, overwrite=TRUE)

test3 <- readRDS("./data-raw/test3.rds")
usethis::use_data(test3, overwrite=TRUE)
