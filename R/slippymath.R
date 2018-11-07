globalVariables(c(".global_sm_env"), "slippymath") #ignore this in R CMD checks
.global_sm_env <- new.env(parent=emptyenv())
.global_sm_env$WEB_MERCATOR_CRS <- sf::st_crs(3857)
.global_sm_env$LATLON_CRS <- sf::st_crs(4326)

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

tilenum_to_latlon <- function(x, y, zoom){
  n_tiles <- 2^zoom

  lon_rad <- (((x / n_tiles) * 2) - 1) * pi

  merc_lat <- (1 - ((y / n_tiles) * 2)) * pi
  lat_rad <- atan(sinh(merc_lat))

  list(lat = degrees(lat_rad),
       lon = degrees(lon_rad))
}

bb_to_tg <- function(bbox,
                     zoom = NULL,
                     max_tiles = NULL){

  if (is_null(zoom) && is_null(max_tiles)){
    stop("at least one of the zoom or max_tiles arugments must be supplied")
  }

  ## No zoom, we'll do a query and choose the best zoom for the max_tiles budget
  if (is_null(zoom)){
    tile_query <- bb_tile_query(bbox, zoom_levels = 1:20)
    suitable_zooms <- tile_query$total_tiles <= max_tiles
    zoom <- max(which(suitable_zooms))
  }

  tile_extent <- bb_tile_extent(bbox, zoom)

  x_tiles <- tile_extent$x_min:tile_extent$x_max
  y_tiles <- tile_extent$y_min:tile_extent$y_max

  if((length(x_tiles) * length(y_tiles)) > max_tiles){
    stop("Bounding box needed more than max_tiles at specified zoom level. Check with bbox_tile_query(bbox)")
  }

  tile_grid <-
    list(
      tiles = expand.grid(x = x_tiles, y = y_tiles),
      zoom = zoom)
  class(tile_grid) <- "tile_grid"

  tile_grid
}


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

bb_tile_extent <- function(bbox, zoom){
  assert_bbox(bbox)

  min_tile <- latlon_to_tilenum(lat = bbox["ymin"],
                                lon = bbox["xmin"], zoom)
  max_tile <- latlon_to_tilenum(lat = bbox["ymax"],
                                lon = bbox["xmax"], zoom)

  list(x_min = min_tile$x,
       y_min = max_tile$y,
       x_max = max_tile$x,
       y_max = min_tile$y)

  ## Note tile numbers start at 0 in the north and increase going south. The
  ## have the opposite polarity to latitude which increases going north. This is
  ## why y_min = max_tile$y here.
}

tile_bb <- function(x, y, zoom){
  bottom_left <- tilenum_to_latlon(x, y+1, zoom)
  top_right <- tilenum_to_latlon(x+1, y, zoom)

  bottom_left_point <- st_point(c(bottom_left$lon, bottom_left$lat))
  top_right_point <-  st_point(c(top_right$lon, top_right$lat))

  box_extent <- st_sfc(bottom_left_point,
                       top_right_point,
                       crs = .global_sm_env$LATLON_CRS)

  box_mercator <- st_transform(box_extent,
                               crs = .global_sm_env$WEB_MERCATOR_CRS)

  st_bbox(box_mercator)
}

tg_bbs <- function(tile_grid){
  if(!is_tile_grid(tile_grid)) stop("tile_grid must be of class tile_grid - output from bb_to_tg()")

  purrr::pmap(.l = tile_grid$tiles,
              .f = tile_bb,
              zoom = tile_grid$zoom)
}

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
