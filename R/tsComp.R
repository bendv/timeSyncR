#' @title Image chip composites
#' 
#' @description Display annual composites of image chips
#'
#' @param x RasterBrick. Image time series brick
#' @param loc Location. Can be a vector of length 2 representing the x,y coordinates, or a SpatialPolygons or SpatialPoints object of \code{nrow = 1} (the first row will be taken if \code{nrow(loc) > 1}), or an extent object (which will be extended if a buffer > 0 is given; see below)
#' @param fun Function to apply over each pixel for each year. See \code{\link{annualSummary}} for more info.
#' @param na.rm Logical. Remove NA's when calculating fun over each year? If \code{fun} is a function that does not take a \code{na.rm} value, set \code{na.rm} to \code{NULL}.
#' @param buff Numeric. Number of pixels to buffer the location in all directions. A higher buffer will essentially zoom out.
#' @param start Numeric. OptionaL: earliest year to display.
#' @param end Numeric. Optional: latest year to display.
#' @param cols Character. Name of colour map to use (see display.brewer.all()) or a character vector with two or more colour names or hexadecimal values (as strings) between which to interpolate.
#' @param nbks Numeric. Number of breaks in the colour map
#' @param nc/nr Numeric. Number of columns and rows to plot, respectively. If the number of layers is greater than \code{nc*nr}, a screen prompt will lead to the next series of plots. These cannot exceed 4.
#' @param plot Logical. Plot pixel time series?
#' @param plotlab Character. y-axis label for the pixel time series plot.
#' @param exportChips Logical. Export processed chips to workspace as a rasterBrick?
#' @param exportZoo Logical. Export pixel time series as a zoo object?
#' @param show Logical. Plot the chips? Can be set to \code{FALSE} if you just want to export the chips as rasterBrick with or without the ggplot object.
#'
#'
#' @import bfastSpatial
#' @import zoo
#' @import RColorBrewer
#' @export
#' 
#' @references
#' Cohen, W. B., Yang, Z., Kennedy, R. (2010). Detecting trends in forest disturbance and recovery using yearly Landsat time series: 2. TimeSync - Tools for calibration and validation. Remote Sensing of Environment, 114(12), 2911-2924.

tsComp <- function(x, loc, fun = mean, na.rm = TRUE, start = NULL, end = NULL, buff = 17, cols = "PiYG", nbks = 35, nc = 3, nr = 3, plot = FALSE, plotlab = 'data', exportChips = FALSE, exportZoo = FALSE, show = TRUE) {
  
  # get sceneinfo
  s <- getSceneinfo(names(x))
  
  # reformat buffer using image resolution
  buff <- buff * res(x)[1]
  
  # check location format and make a buffered extent object
  if(class(loc) == "numeric" & length(loc) != 2){
    stop("loc should be either a numeric vector of length 2 or a spatial object (polygon, points or extent).")
  } else if(class(loc) == "numeric"){
    e <- extent(c(loc[1] - buff, loc[1] + buff, loc[2] - buff, loc[2] + buff))
  } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialPoints", "SpatialPointsDataFrame")){
    if(length(loc) > 1){
      warning("only taking the 1st feature of loc")
      loc <- loc[1, ]
    }
    e <- extent(loc)
    e <- e + buff
  } else if(class(loc) == "extent"){
    e <- loc
    e <- e + buff
  }
  
  # crop input brick
  xe <- crop(x, e)
  se <- getSceneinfo(names(xe))
  
  # temporal subset
  se$year <- as.numeric(format(se$date, format = "%Y"))
  if(is.null(start))
    start <- min(se$year)
  if(is.null(end))
    end <- max(se$year)
  if(start > end)
    stop("start must be before end")
  xe <- raster::subset(xe, which(se$year >= start & se$year <= end))
  se <- getSceneinfo(names(xe))
  se$year <- as.numeric(format(se$date, format = "%Y"))
  yrs <- unique(se$year)
  xcomp <- annualSummary(xe, fun = fun, na.rm = na.rm, years = yrs)
  names(xcomp) <- paste("comp", yrs, sep = '')
  
  # colour map
  if(length(cols) == 1){
    require(RColorBrewer)
    cols <- colorRampPalette(brewer.pal(9, cols))(nbks)
  } else {
    cols <- colorRampPalette(cols)(nbks)
  }
  
  # breaks defined based on extreme values
  minbk <- minValue(xcomp)
  minbk[minbk == -Inf] <- NA
  if(all(is.na(minbk)))
    stop("No non-NA values in the defined image chips.")
  minbk <- min(minbk, na.rm = TRUE)
  maxbk <- maxValue(xcomp)
  maxbk[maxbk == Inf] <- NA
  if(all(is.na(maxbk)))
    stop("No non-NA values in the defined image chips.")
  maxbk <- max(maxbk, na.rm = TRUE)
  print(minbk)
  print(maxbk)
  breaks <- seq(minbk, maxbk, length = nbks)
  
  # add polygon or point to plot if given
  if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialPoints", "SpatialPointsDataFrame")){
    addfun <- function() plot(loc, extent = e, add=TRUE)
  } else {
    addfun <- function() NULL
  }
  
  # plots
  op <- par(mfrow = c(nr, nc))
  pps <- nc * nr
  nscreens <- ceiling(nlayers(xcomp) / pps)
  for(i in seq(1, nlayers(xcomp), by = pps)){
    if((nlayers(xcomp) - i) < pps){
      xs <- raster::subset(xcomp, subset = c(i:nlayers(xcomp)))
      par(op)
      plot(xs, breaks = breaks, col = cols, main = yrs[i:nlayers(xcomp)], legend=FALSE, nc = nc, nr = nr, addfun = addfun)
    } else {
      xs <- raster::subset(xcomp, subset = c(i:(i + pps - 1)))
      plot(xs, breaks = breaks, col = cols, main = yrs[i:(i + pps - 1)], legend=FALSE, nc = nc, nr = nr, addfun = addfun)
      readline("Press any key to continue to next screen: \n")
    }
  }
  
  # prepare zoo objects
  if(plot | exportZoo){
    if(is.numeric(loc)){
      z <- xcomp[cellFromXY(xcomp, loc)][1, ]
    } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
      z <- apply(extract(xcomp, loc)[[1]], 2, mean)
    } else if(class(loc) == 'Extent') {
      loc <- c(mean(xmin(e), xmax(e)), mean(ymin(e), ymax(e)))
      z <- xcomp[cellFromXY(xcomp, loc)][1, ]
    } else {
      z <- xcomp[cellFromXY(xcomp, as.vector(coordinates(loc)))][1, ]
    }
    z <- zoo(z, yrs)
    z <- na.omit(z)
    z <- window(z, start = start, end = end)
  }
  
  # plot pixel time series
  if(plot) {
    readline("Press any key to view time series plots: \n")
    par(mfrow = c(1, 1))
    plot(z, xlab = 'Time', type = 'p', ylab = plotlab)
  }
  
  # decide what to return
  if(exportChips & exportZoo){
    return(list(tsComp = xcomp, zoo = z))
  } else if(exportChips & !exportZoo) {
    return(xcomp)
  } else if(!exportChips & exportZoo) {
    return(z)
  } else {
    return(NULL)
  }
    
}