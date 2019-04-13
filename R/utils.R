radians <- function(angle_deg) angle_deg * pi / 180

degrees <- function(angle_rad) (angle_rad * 180) / pi

is_bbox <- function(obj){
  inherits(obj, "bbox") ||
    is_bbox_vector(obj)
}

is_bbox_vector <- function(obj){
  is.vector(obj) &&
    length(obj) == 4 &&
    length(setdiff(names(obj), c("xmin", "xmax", "ymin", "ymax"))) == 0
}

is_tile_grid <- function(obj) inherits(obj, "tile_grid")

assert_bbox <- function(obj){
  if (!is_bbox(obj)){
    stop("bbox needs to be created with st_bbox() or be a named numeric vector with names: xmin, xmax, ymin, and ymax")
  } 
}
sp_bbox_to_sf <- function(sp_bbox) {
  stats::setNames(as.vector(sp_bbox), c("xmin", "ymin", "xmax", "ymax"))
}

##' Write a raster to PNG
##'
##' This function is a convenience wrapper for
##' writing rasters in PNG format.
##'
##' @title raster_to_png
##' @param tile_raster the raster to write to PNG
##' @param file_path the path to write the raster
##' @return nothing.
##' @export
raster_to_png <- function(tile_raster, file_path){

  if (!inherits(tile_raster, "RasterBrick")){
    stop("tile raster must be a RasterBrick. This is output from tg_composite().") 
  }

  ## png expects 0-1 values, so normalise:
  normalised_raster <-
    sweep(raster::as.array(tile_raster),
          MARGIN = 3,
          STATS = tile_raster@data@max,
          FUN = "/")

  png::writePNG(normalised_raster,
                target = file_path)
}


lol_to_df <- function(lol){
  lov <- purrr::map(purrr::transpose(lol), unlist)
  purrr::lift_dl(data.frame)(lov)
}
