radians <- function(angle_deg) angle_deg * pi /180

is_bbox <- function(obj){
  inherits(obj, "bbox") ||
    is_bbox_vector(obj)
}

is_bbox_vector <- function(obj){
  is.vector(obj) &&
    lenfth(obj) == 4 &&
    length(setdiff(names(tsta), c("xmin", "xmax", "ymin", "ymax"))) == 0
}

assert_bbox <- function(obj){
  if (!is_bbox(obj)){
    stop("bbox needs to be created with st_bbox() or be a named numeric vector with names: xmin, xmax, ymin, and ymax")
  } 
}
sp_bbox_to_sf <- function(sp_bbox) {
  setNames(as.vector(sp_bbox), c("xmin", "ymin", "xmax", "ymax"))
}

