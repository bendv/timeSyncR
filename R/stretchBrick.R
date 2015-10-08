#' @title Multitemporal stretch
#' 
#' @description ...work in progress...
#' 
#' @param x RasterBrick
#' 
#' @export


stretchBrick <- function(x, inlo = min(minValue(x), na.rm = TRUE), inup = max(maxValue(x), na.rm = TRUE), outlo = 1, outup = 256, stretch = 'lin', ...) {
  
  ## linear stretch
  if(stretch == 'lin') {
    st <- function(y) {
      ((y - inlo) * ((outup - outlo)/(inup - inlo))) + outlo
    }
  } else if(stretch == 'hist') {
    z <- as.vector(getValues(x))
    cdf <- ecdf(z[!is.na(z)])
    st <- function(y) {
      cdf(y) * (outup - 1)
    }
  }
  
  res <- mc.calc(x, fun = st, ...)
  names(res) <- names(x)
  return(res)
  
}
