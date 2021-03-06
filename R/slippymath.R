globalVariables(c(".global_sm_env"), "slippymath") #ignore this in R CMD checks
.global_sm_env <- new.env(parent=emptyenv())

.global_sm_env$WEB_MERCATOR_CRS <- structure(list(epsg = 3857L, proj4string = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"), class = "crs")
## sf::st_crs(3857)

.global_sm_env$LONLAT_CRS <- structure(list(epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"), class = "crs")
## sf::st_crs(4326)

##' Convert longitude and latitude to slippy tile numbers
##'
##' Returns the Open Street Map slippy map tile numbers (x, y) the
##' supplied latitude and longitude fall on, for a given zoom level.
##'
##' The point specified by lon_deg` and `lat_deg` is assumed to be in ESPG:4326
##' coordinate reference system.
##'
##' @title lonlat_to_tilenum
##' @param lon_deg degrees longitude for point
##' @param lat_deg degrees latitude for point
##' @param zoom zoom level for tile calculation. Increasing zoom increases the
##'   number of tiles.
##' @return a list containing `x` and `y` - the tile numbers.
##' @examples
##' lonlat_to_tilenum(
##'  lon = 13.37771496361961,
##'  lat = 52.51628011262304,
##'  zoom = 17
##' )
##' @export
lonlat_to_tilenum <- function(lon_deg, lat_deg, zoom){
    ## Implementing slippy map spec as per https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    lon_rad <- radians(lon_deg)
    lat_rad <- radians(lat_deg)

    x <- lon_rad
    y <- asinh(tan(lat_rad))

    x <- (1 + (x / pi))/2
    y <- (1 - (y / pi))/2

    n_tiles <- 2^zoom

    ## The values are clamped to prevent problems at the extent boundaries. Eg 180
    ## degrees lon which would lon_rad of pi.
    xtile <- sm_clamp(floor(x * n_tiles), 0, n_tiles-1)
    ytile <- sm_clamp(floor(y * n_tiles), 0, n_tiles-1)

    list(x = xtile, y = ytile)
}

##' Convert slippy map tiles numbers to latitude and longitude
##'
##' Returns the latitude and longitude of the top left corner of a slippy map tile
##' specified by `x`, `y` for a given zoom level.
##' @title  tilenum_to_lonlat
##' @param x slippy map tile number in x domain (left to right)
##' @param y slippy map tile number in y domain (top to bottom)
##' @param zoom the zoom level for the calculation. Increasing zoom increases
##'   the number of tiles.
##' @return a list containing `lat` and `lon` - latitude and longitude.
##' @examples
##' tilenum_to_lonlat(
##'  x = 70406,
##'  y = 42987,
##'  zoom = 17
##' )
##' @export
tilenum_to_lonlat <- function(x, y, zoom){
    n_tiles <- 2^zoom

    lon_rad <- (((x / n_tiles) * 2) - 1) * pi

    merc_lat <- (1 - ((y / n_tiles) * 2)) * pi
    lat_rad <- atan(sinh(merc_lat))

    list(lon = degrees(lon_rad),
         lat = degrees(lat_rad))
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
##' @title bbox_to_tile_grid
##' @param bbox the bounding box to fit onto a grid of tiles. Must be either a
##'   'bbox' object created with sf::st_bbox or a vector of length 4 with names:
##'   'xmin', 'xmax', 'ymin', 'ymax'.
##' @param zoom Optional. The desired zoom level.
##' @param max_tiles Optional. The maximum number of tiles the grid may occupy.
##' @return a 'tile_grid' object containing 'tiles' and 'zoom'
##' @examples
##' tibrogargan<- c(xmin = 152.938485, ymin = -26.93345, xmax = 152.956467, 
##'                ymax = -26.921463)
##'
##' ## Get a grid of the minimum number of tiles for a given zoom.
##' bbox_to_tile_grid(tibrogargan, zoom = 15)
##'
##' ## get a grid of at most 12 tiles, choosing the most detailed zoom possible.
##' bbox_to_tile_grid(tibrogargan, max_tiles = 12)
##' @export
bbox_to_tile_grid <- function(bbox,
                     zoom = NULL,
                     max_tiles = NULL){

    if (purrr::is_null(zoom) && purrr::is_null(max_tiles)){
        stop("at least one of the zoom or max_tiles arugments must be supplied")
    }

    ## No zoom, we'll do a query and choose the best zoom for the max_tiles budget
    if (purrr::is_null(zoom)){
        tile_query <- bbox_tile_query(bbox, zoom_levels = 0:19)
        suitable_zooms <- tile_query$total_tiles <= max_tiles
        zoom <- tile_query$zoom[max(which(suitable_zooms))]
    }

    tile_extent <- bbox_tile_extent(bbox, zoom)

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
##' @title bbox_tile_query
##' @param bbox a bbox object created by `sf::st_bbox`, or a vector with names
##'   'xmin', 'xmax', 'ymin', 'ymax'
##' @param zoom_levels a numeric vector of zoom levels to calculate tile usage for.
##' @return a data frame containing tile usage information for the bounding box
##'   at each zoom level.
##' @examples
##' tibrogargan<- c(xmin = 152.938485, ymin = -26.93345, xmax = 152.956467, 
##'                ymax = -26.921463)
##'
##' bbox_tile_query(tibrogargan)
##' @export
bbox_tile_query <- function(bbox, zoom_levels = 2:18){

    extents_at_zooms <- purrr::map(zoom_levels,
                                   ~bbox_tile_extent(bbox, .))

    extents_at_zooms <- lol_to_df(extents_at_zooms)

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
##' @title bbox_tile_extent
##' @param bbox a bbox object created by `sf::st_bbox`, or a vector with names
##'   'xmin', 'xmax', 'ymin', 'ymax'
##' @param zoom zoom level to calculate the tile grid on.
##' @return a list of `x_min`, `y_min`, `x_max`, `y_max`
##' @examples
##' tibrogargan<- c(xmin = 152.938485, ymin = -26.93345, xmax = 152.956467, 
##'                ymax = -26.921463)
##' bbox_tile_extent(tibrogargan, zoom = 15)
##' @export
bbox_tile_extent <- function(bbox, zoom){
    assert_bbox(bbox)

    min_tile <- lonlat_to_tilenum(lat_deg = bbox["ymin"],
                                  lon_deg = bbox["xmin"], zoom)
    max_tile <- lonlat_to_tilenum(lat_deg = bbox["ymax"],
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
##' an `sf` bounding box object for the tile with units in metres using the
##' EPSG:3857 coordinate reference system (Web Mercator).
##'
##' @title tile_bbox
##' @param x slippy map tile x number
##' @param y slippy map tile y number
##' @param zoom zoom level for tile
##' @return an sf bbox object.
##' @examples
##' ## return an sf style bbox object in with epsg and proj4string
##' tile_bbox(x = 30304, y = 18929, zoom = 15)
##' @export
tile_bbox <- function(x, y, zoom){

    bottom_left <-
        lonlat_to_merc(t(as.matrix(unlist(tilenum_to_lonlat(x, y+1, zoom)))))
    
    top_right <-
        lonlat_to_merc(t(as.matrix(unlist(tilenum_to_lonlat(x+1, y, zoom)))))

    structure(c(xmin = bottom_left[[1]],
                ymin = bottom_left[[2]],
                xmax = top_right[[1]],
                ymax = top_right[[2]]),
              class = "bbox",
              crs = .global_sm_env$WEB_MERCATOR_CRS)
 }   




##' Get tile grid bounding boxes
##'
##' Given an tile_grid object like that returned from `bbox_to_tile_grid`, return
##' a list
##' of sf style bounding box objects, one for each tile in the grid, in the same order
##' as tiles in `tile_grid$tiles`.
##'
##' The bounding box units are metres in the EPSG:3857 coordinate reference
##' system (Web Mercator).
##'
##' @title tile_grid_bboxes
##' @param tile_grid a tile_grid object, likely returned from `bbox_to_tile_grid`
##' @return a list of sf bounding box objects in the corresponding order to the
##'   tiles in `tile_grid`
##' @examples
##' 
##' tibrogargan<- c(xmin = 152.938485, ymin = -26.93345, xmax = 152.956467, 
##'                ymax = -26.921463)
##' 
##' tibrogargan_grid <- bbox_to_tile_grid(tibrogargan, zoom = 15)
##'
##' tile_grid_bboxes(tibrogargan_grid)
##' @export
tile_grid_bboxes <- function(tile_grid){
    if(!is_tile_grid(tile_grid)) stop("tile_grid must be of class tile_grid - output from bbox_to_tile_grid()")

    purrr::pmap(.l = tile_grid$tiles,
                .f = tile_bbox,
                zoom = tile_grid$zoom)
}

##' Compose a list of images using tile_grid data.
##'
##' Given a tile_grid object and a list of images, compose the images into a
##' single spatially referenced RasterBrick object.
##'
##' The list of images is assumed to be in corresponding order to the tiles in
##' the tile_grid object.
##'
##' The returned object uses the Web Mercator projection, EPSG:3857, which is
##' the native crs of the tiles.
##'
##' @title compose_tile_grid
##' @param tile_grid a tile_grid object, likely returned from `bbox_to_tile_grid`
##' @param images a list of character strings defining paths to images. Matched to tiles in tile_grid based on list position.
##' @return a spatially referenced raster.
##' @export
compose_tile_grid <- function(tile_grid, images){

    bricks <-
        purrr::pmap(.l = list(x = tile_grid$tiles$x,
                              y = tile_grid$tiles$y,
                              image = images),
                    .f = function(x, y, image, zoom){
                        bbox <- tile_bbox(x, y, zoom)
                        raster_img <-
                            raster::brick(image,
                                          crs = attr(bbox, "crs")$proj4string)
                        raster::extent(raster_img) <-
                            raster::extent(bbox[c("xmin", "xmax", "ymin", "ymax")])
                        raster_img
                    },
                    zoom = tile_grid$zoom)

    geo_refd_raster <- do.call(raster::merge, bricks)
    
    geo_refd_raster
}
