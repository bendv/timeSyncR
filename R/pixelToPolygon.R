#' @title Convert pixel to polygon
#' 
#' @description Convert a pixel to a polygon object by giving the raster object and cell number or xy coordinates vector
#' 
#' @param x Raster* object
#' @param cell Cell number (numeric) or vector of length 2 representing the x and y coordinate of the desired pixel
#' 
#' @return Object of class \code{SpatialPolygons} encompassing the outline of the pixel located at \code{cell}.
#' 
#' @export

pixelToPolygon <- function(x, cell, plot = FALSE) {
  
  # check if cell is a number or an xy vector
  if(length(cell) == 1) {
    cell <- as.vector(xyFromCell(x, cell))
  } else if(length(cell) > 2) {
    warning("taking the first 2 elemtns of cell.\n")
    cell <- cell[1:2]
  }
  
  # define extent
  # note that cellFromXY() retrieves the centre of the pixel -- we need the top-left for xmin,ymin
  shift <- res(x)[1] / 2
  e <- extent(c(
    cell[1] - shift, 
    cell[1] + shift, 
    cell[2] - shift, 
    cell[2] + shift))
  
  # convert to polygons and set projection
  e <- as(e, "SpatialPolygons")
  projection(e) <- projection(x)
  
  return(e)
}