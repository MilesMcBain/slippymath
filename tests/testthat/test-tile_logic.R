context("test-tile_logic")


test_that("tile cascade works", {
  bbox <-  c(xmin = -180, xmax = 180, ymin = -85.0511, ymax = 85.0511)
  levs <- slippymath::bb_tile_query(bbox, zoom_levels = 0:19)

  expect_equal(levs$zoom, 0:19)
  expect_equal(levs$total_tiles, (2^seq(0, 19))^2)
  expect_true(all(levs$x_min == 0))
  expect_true(all(levs$y_min == 0))
  expect_equal(levs$x_max,  2^seq(0, 19)-1)
  expect_equal(levs$x_max, levs$y_max)
  expect_equal(levs$x_max + 1, levs$x_dim)
  expect_equal(levs$x_dim, levs$y_dim)
})


test_that("tile cascade smaller extent works", {
  bbox <-  c(xmin = 140, xmax = 150,
                       ymin = -43, ymax = -41)
  levs_now <- slippymath::bb_tile_query(bbox, zoom_levels = 0:19)
  #dput(levs)
  levs <- structure(list(x_min = c(0, 1, 3, 7, 14, 28, 56, 113, 227, 455,
                                   910, 1820, 3640, 7281, 14563, 29127, 58254, 116508, 233016, 466033),
                         y_min = c(0, 1, 2, 5, 10, 20, 40, 80, 160, 320, 640, 1280,
                                   2560, 5120, 10241, 20482, 40964, 81929, 163859, 327718),
                         x_max = c(0,1, 3, 7, 14, 29, 58, 117, 234, 469, 938, 1877, 3754, 7509, 15018,
                                  30037, 60074, 120149, 240298, 480597),
                         y_max = c(0, 1, 2, 5, 10, 20, 40, 80, 161, 323, 647, 1295, 2590, 5181, 10363, 20727,
                                   41454, 82909, 165819, 331638),
                         y_dim = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 8, 16, 31, 62, 123, 246, 491, 981, 1961, 3921),
                         x_dim = c(1, 1, 1, 1, 1, 2, 3, 5, 8, 15, 29, 58, 115, 229, 456, 911, 1821, 3642, 7283, 14565),
                         total_tiles = c(1, 1, 1, 1, 1, 2, 3, 5, 16, 60, 232, 928, 3565, 14198, 56088, 224106, 894111,
                                         3572802, 14281963, 57109365),
                         zoom = 0:19), row.names = c(NA, -20L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                               "tbl", "data.frame"))
  expect_equal(levs_now, levs)
})

test_that("whole zoom 0 tile logic works", {
  bbox <-  c(xmin = -180, xmax = 180, ymin = -85.0511, ymax = 85.0511)
  tg <- slippymath::bb_to_tg(bbox, zoom = 0)
  tg %>% expect_s3_class("tile_grid") %>% expect_named(c("tiles", "zoom"))
  expect_true(nrow(tg$tiles) == 1L)
  expect_true(tg$zoom == 0)
  })
