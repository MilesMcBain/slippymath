context("Test conversion of lonlat to mercator")

test_that("conversion functions work", {
  uluru_merc <- matrix(c(14586472.958481, -2918162.223463), nrow = 1)
  uluru_lonlat <- matrix(c(131.0325162, -25.3448562), nrow = 1)

  expect_true(all.equal(merc_to_lonlat(uluru_merc), uluru_lonlat))
  expect_true(all.equal(lonlat_to_merc(uluru_lonlat), uluru_merc))
})
