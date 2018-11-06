radians <- function(angle_deg) angle_deg * pi /180

latlon_to_tilenum<-function(lat_deg, lon_deg, zoom){
  ## Implementing slippy map spec as per https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  lat_rad <- radians(lat_deg)
  lon_rad <- radians(lon_deg)

  x <- lon_rad
  y <- asinh(tan(lat_rad))

  x <- (1 + (x/pi))/2
  y <- (1 - (y/pi))/2

  n_tiles <- 2^zoom

  xtile <- floor(x * n_tiles)
  ytile <- floor(y * n_tiles)

  return( c(xtile, ytile))
}
