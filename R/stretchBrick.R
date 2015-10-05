#' @title Multitemporal linear stretch
#' 
#' @description ...work in progress...
#' 
#' @param x RasterBrick
#' 
#' @export



stretchBrick <- function(x, ...) {
  
  INLO <- min(minValue(x))
  INUP <- max(maxValue(x))
  OUTLO <- 1
  OUTUP <- 256
  
  st <- mc.calc(x, fun = function(y) {
    ((y - INLO) * ((OUTUP - OUTLO)/(INUP-INLO))) + OUTLO
  }, ...)
  
  return(st)
  
}
