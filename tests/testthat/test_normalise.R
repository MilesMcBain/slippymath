context("test normalisation for PNG write")

test_that("raster normalisation works", {

  dummy_values <-
    rep(c(0, 64, 128, 255), 25)

  mat_5_20 <-
    raster::raster(nrows = 5,
                   ncols = 20,
                   vals = dummy_values)

  mat_20_5 <-
    raster::raster(nrows = 20,
                   ncols = 5,
                   vals = dummy_values)
  mat_10_10 <-
    raster::raster(nrows = 10,
                   ncols = 10,
                   vals = dummy_values)
  ## 3 layer
  expect_equal(
  raster::brick(mat_5_20,
                mat_5_20 * 0.1,
                mat_5_20 * 10) %>%
  normalise_raster() %>%
  range(),
  c(0, 1))

  ## 4 layer
  expect_equal(
    raster::brick(mat_5_20,
                  mat_5_20 * 0.1,
                  mat_5_20 * 10,
                  mat_5_20) %>%
    normalise_raster() %>%
    range(),
    c(0, 1))

  ## 2 layer
  expect_equal(
    raster::brick(mat_5_20,
                  mat_5_20 * 10) %>%
    normalise_raster() %>%
    range(),
    c(0, 1))

  ## 3 layer long
  expect_equal(
    raster::brick(mat_20_5,
                  mat_20_5 * 0.1,
                  mat_20_5 * 10) %>%
    normalise_raster() %>%
    range(),
    c(0, 1))

  ## 3 layer square
  expect_equal(
    raster::brick(mat_10_10,
                  mat_10_10 * 0.1,
                  mat_10_10 * 10) %>%
    normalise_raster() %>%
    range(),
    c(0, 1))

})
