context("test tile math against OSM wiki examples")

test_that("conversion to tile coordinates matches reference", {

  lon <- 13.37771496361961
  lat <- 52.51628011262304
  zoom <- 17

  tile_coords <- latlon_to_tilenum(lat, lon, zoom)

  expect_true({
    tile_coords$x == 70406 &&
      tile_coords$y == 42987
  })
})


test_that("conversion from tile coordinates to lat lon matches reference", {

  lon <- 13.375854492
  lat <- 52.517892228
  zoom <- 17
  x <- 70406
  y <- 42987

  latlon_coords <- tilenum_to_latlon(x, y, zoom)

  expect_true({ all.equal(latlon_coords$lat, lat, tolerance = 10^-8) &&
                  all.equal(latlon_coords$lon, lon, tolerance = 10^-8)

  })
})

test_that("bounding box calculation matches reference", {

  zoom <- 17
  x <- 70406
  y <- 42987

  lower_left <- sf::st_point(x = c(13.375854492,52.516220864))
  upper_right <-sf::st_point(x = c(13.378601074,52.517892228))

  points <- sf::st_sfc(lower_left, upper_right,
                       crs = .global_sm_env$LATLON_CRS)
  points_mercator <- sf::st_transform(points, crs = .global_sm_env$WEB_MERCATOR_CRS)

  reference_bbox <- sf::st_bbox(points_mercator)

  tile_bbox <- tile_bb(x, y, 17)

  expect_true(all.equal(reference_bbox, tile_bbox))
})

test_that("bounding box calculation for tiles at edge of domain works", {
  # A point in top right quadrant
  lat = 80
  lon = 150

  tile <- latlon_to_tilenum(lat, lon, 1)
  tile_bbox <- tile_bb(tile$x, tile$y, 1)

  expected_bbox <- sf::st_bbox(c(xmin = 0, ymin = 0,
                                 xmax = 20037508.34, ymax = 20037508.34),
                               crs = .global_sm_env$WEB_MERCATOR_CRS)

  expect_true({
    all.equal(tile_bbox, expected_bbox)
  })
})
