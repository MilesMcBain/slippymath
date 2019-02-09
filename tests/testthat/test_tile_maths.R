context("test tile math against OSM wiki examples")

test_that("conversion to tile coordinates matches reference", {

  lon <- 13.37771496361961
  lat <- 52.51628011262304
  zoom <- 17

  tile_coords <- lonlat_to_tilenum(lon, lat, zoom)

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

  lonlat_coords <- tilenum_to_lonlat(x, y, zoom)

  expect_true({ all.equal(lonlat_coords$lat, lat, tolerance = 10^-8) &&
                  all.equal(lonlat_coords$lon, lon, tolerance = 10^-8)

  })
})

test_that("bounding box calculation matches reference", {

  zoom <- 17
  x <- 70406
  y <- 42987

  reference_bbox <-
      structure(c(xmin = 1488993.31097436, ymin = 6894008.45510929,
                  xmax = 1489299.05908402, ymax = 6894314.20313973),
                class = "bbox", crs = structure(list(
                                    epsg = 3857L,
                                    proj4string = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"),
                                    class = "crs"))

  tile_bbox <- tile_bb(x, y, 17)

  expect_true(all.equal(reference_bbox, tile_bbox))
})

test_that("bounding box calculation for tiles at edge of domain works", {
  # A point in top right quadrant
  lat = 80
  lon = 150

  tile <- lonlat_to_tilenum(lon, lat, 1)
  tile_bbox <- tile_bb(tile$x, tile$y, 1)

  expected_bbox <-
      structure(c(xmin = 0, ymin = 0, xmax = 20037508.34, ymax = 20037508.34),
                class = "bbox",
                crs = structure(list(epsg = 3857L,
                                     proj4string = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"),
                                class = "crs"))

  expect_true({
    all.equal(tile_bbox, expected_bbox)
  })
})
