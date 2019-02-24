globalVariables(c("A", "MAXEXTENT"), "slippymath") #ignore this in R CMD checks

# Convert lon/lat values to 900913 x/y.
A <- 6378137
MAXEXTENT <- 20037508.342789244;
# clamp values within range min/max
sm_clamp <- function(x, mn, mx) {
  x[x < mn] <- mn
  x[x > mx] <- mx
  x
}

# https://github.com/mapbox/sphericalmercator/blob/3f61128523aed26904be33463af87a142752a4ad/sphericalmercator.js#L175
#' Transform between spherical Mercator and longitude/latitude
#'
#' @param ll matrix of longitude / latitude
#' @param xy matrix of x / y Mercator
#' @return matrix of coordinates transformed forward or inverse
#' @export
#' @aliases merc_to_lonlat
lonlat_to_merc <- function(ll) {
  xy <-
      cbind(A * radians(ll[,1]),
          A * log(tan((pi*0.25) + (0.5 * radians(ll[,2])))))
  ## if xy value is beyond maxextent (e.g. poles), return maxextent.
  xy[, 1] <- sm_clamp(xy[, 1], -MAXEXTENT, MAXEXTENT)
  xy[, 2] <- sm_clamp(xy[, 2], -MAXEXTENT, MAXEXTENT)
  xy
}

## Convert 900913 x/y values to lon/lat.
#' @name lonlat_to_merc
#' @export
merc_to_lonlat <- function(xy) {
  xy <- cbind(degrees(xy[, 1] / A),
              degrees(((pi*0.5) - 2.0 * atan(exp(-xy[, 2] / A)))))
  xy
}

##' Are points in meters within Mercator extent?
##'
##' When doing maths with Mercator coordinates in m, you can end up outside the
##' Mercator extent with an undefined coordinate. This function returns true if
##' all xy lie within the Mercator extent.
##' @title within_mercator_extent
##' @param xy a matrix of Mercator xy coordinates.
##' @return TRUE or FALSE
##' @export
within_merc_extent <- function(xy){
  all(xy <= MAXEXTENT) & all(xy >= -MAXEXTENT)
}

##' Truncate coordinate to Mercator extent.
##'
##' If a point in m lies outside the Mercator extent, this function can be used
##' to truncate it to the boundary of the extent.
##' 
##' @title merc_truncate
##' @param xy a matrix of Mercator XY points.
##' @return a matrix of XY points.
##' @export
merc_truncate <- function(xy){
  xy[, 1] <- sm_clamp(xy[, 1], -MAXEXTENT, MAXEXTENT)
  xy[, 2] <- sm_clamp(xy[, 2], -MAXEXTENT, MAXEXTENT)
  xy
}

