globalVariables(c(".global_sm_env"), "slippymath") #ignore this in R CMD checks
.global_sm_env <- new.env(parent=emptyenv())
.global_sm_env$WEB_MERCATOR_CRS <- sf::st_crs(3857)
.global_sm_env$LATLON_CRS <- sf::st_crs(4326)

##' Convert latitude and longitude to slippy tile numbers
##'
##' Returns the Open Street Map slippy map tile numbers (x, y) the
##' supplied latitude and longitude fall on, for a given zoom level.
##'
##' The point specified by `lat_deg` and `lon_deg` is assumed to be in ESPG:4326
##' coordinate reference system.
##'
##' @title latlon_to_tilenum
##' @param lat_deg degrees latitude for point
##' @param lon_deg degrees longitude for point
##' @param zoom zoom level for tile calculation. Increasing zoom increases the
##'   number of tiles.
##' @return a list containing `x` and `y` - the tile numbers.
##' @export
latlon_to_tilenum <- function(lat_deg, lon_deg, zoom){
  ## Implementing slippy map spec as per https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  lat_rad <- radians(lat_deg)
  lon_rad <- radians(lon_deg)

  x <- lon_rad
  y <- asinh(tan(lat_rad))

  x <- (1 + (x / pi))/2
  y <- (1 - (y / pi))/2

  n_tiles <- 2^zoom

  xtile <- floor(x * n_tiles)
  ytile <- floor(y * n_tiles)

  list(x = xtile, y = ytile)
}

##' Convert slippy map tiles numbers to latitude and longitude
##'
##' Returns the latitude and longitude of the top left corner of a slippy map tile
##' specified by `x`, `y` for a given zoom level.
##' @title  tilenum_to_latlon
##' @param x slippy map tile number in x domain (left to right)
##' @param y slippy map tile number in y domain (top to bottom)
##' @param zoom the zoom level for the calculation. Increasing zoom increases
##'   the number of tiles.
##' @return a list containing `lat` and `lon` - latitude and longitude.
##' @export
tilenum_to_latlon <- function(x, y, zoom){
  n_tiles <- 2^zoom

  lon_rad <- (((x / n_tiles) * 2) - 1) * pi

  merc_lat <- (1 - ((y / n_tiles) * 2)) * pi
  lat_rad <- atan(sinh(merc_lat))

  list(lat = degrees(lat_rad),
       lon = degrees(lon_rad))
}

##' Bounding box to tile grid
##'
##' Calculate a slippy map tile grid that will fit a supplied bounding box.
##'
##' The grid is returned as part of a tile_grid object that contains a
##' data.frame of x,y tile numbers and zoom level.
##'
##' The tile grid can be calculated for a given zoom level or for the deepest
##' zoom that ensures the number of tiles is less than or equal to `max_tiles`.
##'
##' If `zoom` and `max_tiles` are supplied together, then the max is still
##' enforced and the function will fail if more tiles are required for the given
##' zoom.
##'
##' @title bb_to_tg
##' @param bbox the bounding box to fit onto a grid of tiles. Must be either a
##'   'bbox' object created with sf::st_bbox or a vector of length 4 with names:
##'   'xmin', 'xmax', 'ymin', 'ymax'.
##' @param zoom Optional. The desired zoom level.
##' @param max_tiles Optional. The maximum number of tiles the grid may occupy.
##' @return a 'tile_grid' object containing 'tiles' and 'zoom'
##' @export
bb_to_tg <- function(bbox,
                     zoom = NULL,
                     max_tiles = NULL){

  if (purrr::is_null(zoom) && purrr::is_null(max_tiles)){
    stop("at least one of the zoom or max_tiles arugments must be supplied")
  }

  ## No zoom, we'll do a query and choose the best zoom for the max_tiles budget
  if (purrr::is_null(zoom)){
    tile_query <- bb_tile_query(bbox, zoom_levels = 0:19)
    suitable_zooms <- tile_query$total_tiles <= max_tiles
    zoom <- tile_query$zoom[max(which(suitable_zooms))]
  }

  tile_extent <- bb_tile_extent(bbox, zoom)

  x_tiles <- tile_extent$x_min:tile_extent$x_max
  y_tiles <- tile_extent$y_min:tile_extent$y_max

  if(!purrr::is_null(max_tiles) && (length(x_tiles) * length(y_tiles)) > max_tiles){
    stop("Bounding box needed more than max_tiles at specified zoom level. Check with bbox_tile_query(bbox)")
  }

  tile_grid <-
    list(
      tiles = expand.grid(x = x_tiles, y = y_tiles),
      zoom = zoom)
  class(tile_grid) <- "tile_grid"

  tile_grid
}

##' Bounding box tile query
##'
##' Determines how many tiles the bounding box would occupy for a range of zooms. Useful for working out what is a reasonable zoom to work at. Each tile is a separate request from the server.
##'
##' Tiles are typically 256x256 pixels and are tens of Kb in size, you can get some sense of the data from the query also.
##'
##' @title bb_tile_query
##' @param bbox a bbox object created by `sf::st_bbox`, or a vector with names
##'   'xmin', 'xmax', 'ymin', 'ymax'
##' @param zoom_levels a numeric vector of zoom levels to calculate tile usage for.
##' @return a data frame containing tile usage information for the bounding box
##'   at each zoom level.
##' @export
bb_tile_query <- function(bbox, zoom_levels = 2:18){

  extents_at_zooms <- purrr::map_dfr(zoom_levels,
                                 ~bb_tile_extent(bbox, .))

  extents_at_zooms$y_dim <-
    abs(extents_at_zooms$y_max - extents_at_zooms$y_min) + 1
  extents_at_zooms$x_dim <-
    abs(extents_at_zooms$x_max - extents_at_zooms$x_min) + 1
  extents_at_zooms$total_tiles <-
    extents_at_zooms$y_dim * extents_at_zooms$x_dim
  extents_at_zooms$zoom <- zoom_levels

  extents_at_zooms

}

##' Convert a bounding box from latitude and longitude to tile numbers
##'
##' This function creates an analog of a bounding box but in tile numbers. It
##' returns the min and max x and y tile numbers for a tile grid that would fit
##' the bounding box for a given zoom level.
##'
##' @title bb_tile_extent
##' @param bbox a bbox object created by `sf::st_bbox`, or a vector with names
##'   'xmin', 'xmax', 'ymin', 'ymax'
##' @param zoom zoom level to calculate the tile grid on.
##' @return a list of `x_min`, `y_min`, `x_max`, `y_max`
##' @export
bb_tile_extent <- function(bbox, zoom){
  assert_bbox(bbox)

  min_tile <- latlon_to_tilenum(lat_deg = bbox["ymin"],
                                lon_deg = bbox["xmin"], zoom)
  max_tile <- latlon_to_tilenum(lat_deg = bbox["ymax"],
                                lon_deg = bbox["xmax"], zoom)

  list(x_min = min_tile$x,
       y_min = max_tile$y,
       x_max = max_tile$x,
       y_max = min_tile$y)

  ## Note tile numbers start at 0 in the north and increase going south. The
  ## have the opposite polarity to latitude which increases going north. This is
  ## why y_min = max_tile$y here.
}

##' Calculate the bounding box for a tile in latitude and longitude
##'
##' Given a slippy maps tile specified by `x`, `y`, and `zoom`, return the
##' an `sf` bounding box object for the tile in degrees latitude and longitude using the EPSG:4326
##' coordinate reference system.
##'
##' @title tile_bb
##' @param x slippy map tile x number
##' @param y slippy map tile y number
##' @param zoom zoom level for tile
##' @return an sf bbox object.
##' @export
tile_bb <- function(x, y, zoom){
  bottom_left <- tilenum_to_latlon(x, y+1, zoom)
  top_right <- tilenum_to_latlon(x+1, y, zoom)

  bottom_left_point <- sf::st_point(c(bottom_left$lon, bottom_left$lat))
  top_right_point <-  sf::st_point(c(top_right$lon, top_right$lat))

  box_extent <- sf::st_sfc(bottom_left_point,
                       top_right_point,
                       crs = .global_sm_env$LATLON_CRS)

  box_mercator <- sf::st_transform(box_extent,
                               crs = .global_sm_env$WEB_MERCATOR_CRS)

  sf::st_bbox(box_mercator)
}

##' Get tile grid bounding boxes
##'
##' Given an tile_grid object like that returned from `bb_to_tg`, return a list
##' of sf bounding box objects one for each tile in the grid, in the same order
##' as tiles in `tile_grid$tiles`.
##'
##' The bounding boxes use degrees latitude and longitude in the EPSG:4326
##' coordinate reference system.
##'
##' @title tg_bbs
##' @param tile_grid a tile_grid object, likely returned from `bb_to_tg`
##' @return a list of sf bounding box objects in the corresponding order to the tiles in `tile_grid`
##' @export
tg_bbs <- function(tile_grid){
  if(!is_tile_grid(tile_grid)) stop("tile_grid must be of class tile_grid - output from bb_to_tg()")

  purrr::pmap(.l = tile_grid$tiles,
              .f = tile_bb,
              zoom = tile_grid$zoom)
}

##' Composite a list of images using tile_grid data.
##'
##' Given a tile_grid object and a list of images, composite the images into a spatially referenced RasterBrick object.
##'
##' The list of images is assumed to be in corresponding order to the tiles in
##' the tile_grid object.
##'
##' The returned object uses the Web Mercator projection, EPSG:3857, which is
##' the native crs of the tiles.
##'
##' @title tg_composite
##' @param tile_grid a tile_grid object, likely returned from `bb_to_tg`
##' @param images a list of character strings defining paths to images. Matched to tiles in tile_grid based on list position.
##' @return a spatially referenced raster.
##' @export
tg_composite <- function(tile_grid, images){

  bricks <-
    purrr::pmap(.l = list(x = tile_grid$tiles$x,
                          y = tile_grid$tiles$y,
                          image = images),
                .f = function(x, y, image, zoom){
                  tile_bbox <- tile_bb(x, y, zoom)
                  raster_img <-
                    raster::brick(image,
                                  crs = sf::st_crs(tile_bbox)$proj4string)
                  raster::extent(raster_img) <-
                    raster::extent(tile_bbox[c("xmin", "xmax", "ymin", "ymax")])
                  raster_img
                },
                zoom = tile_grid$zoom)

  geo_refd_raster <- do.call(raster::merge, bricks)

  geo_refd_raster
}
