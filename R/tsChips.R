#' @title Plot TS Chips
#' 
#' @description Plot image TS chips based on a location, area buffer, and time buffer
#' 
#' @param x RasterBrick. Image time series brick
#' @param loc Location. Can be a vector of length 2 representing the x,y coordinates, or a SpatialPolygons or SpatialPoints object of \code{nrow = 1} (the first row will be taken if \code{nrow(loc) > 1}), or an extent object (which will be extended if a buffer > 0 is given; see below)
#' @param buff Numeric. Number of pixels to buffer the location in all directions. A higher buffer will essentially zoom out.
#' @param start Date. OptionaL: earliest date ("yyyy-dd-mm") to display.
#' @param end Date. Optional: latest date ("yyyy-dd-mm") to display.
#' @param percNA Numeric. Maximum allowable \% NA in the cropped image chips
#' @param cols Character. Name of colour map to use (see display.brewer.all()) or a character vector with two or more colour names or hexadecimal values (as strings) between which to interpolate.
#' @param nbks Numeric. Number of breaks in the colour map
#' @param nc/nr Numeric. Number of columns and rows to plot, respectively. If the number of layers is greater than \code{nc*nr}, a screen prompt will lead to the next series of plots. These cannot exceed 4.
#' @param plot Logical. Plot pixel time series?
#' @param plotlab Character. y-axis label for the pixel time series plot.
#' @param exportChips Logical. Export processed chips to workspace as a rasterBrick?
#' @param exportZoo Logical. Export pixel time series as a zoo object?
#' @param show Logical. Plot the chips? Can be set to \code{FALSE} if you just want to export the chips as rasterBrick with or without the ggplot object.
#'  
#' @return \code{NULL} if \code{ggplot = FALSE} or an object of class \code{ggplot} if \code{ggplot = TRUE}, with the side effect of time series chips being plotted in both cases. If \code{export = TRUE}, an object of class rasterBrick, and if both \code{ggplot} and \code{export} are \code{TRUE}, a list including a rasterBrick and a ggplot object.
#' 
#' @author Ben DeVries
#' 
#' @import raster
#' @import RColorBrewer
#' @export
#' 
#' @references
#' Cohen, W. B., Yang, Z., Kennedy, R. (2010). Detecting trends in forest disturbance and recovery using yearly Landsat time series: 2. TimeSync - Tools for calibration and validation. Remote Sensing of Environment, 114(12), 2911-2924.
#' 
#' @examples
#' \dontrun{
#' library(bfastSpatial)
#' data(tura)
#' 
#' tsChips(tura, loc = c(820796, 831198))
#' tsChips(tura, loc = c(820796, 831198), buff = 50) # zoom out
#' tsChips(tura, loc = c(820796, 831198), buff = 50, percNA = 80) # allow more NA's in field of view
#' tsChips(tura, loc = c(820796, 831198), start = "2007-01-01", end = "2012-01-01", plot = TRUE, plotlab = 'NDVI') # restrict dates and produce pixel time series plot afterwards
#' 
#' # alternative colour scales
#' library(RColorBrewer)
#' display.brewer.all()
#' tsChips(tura, loc = c(820796, 831198), start = "1999-01-01", cols = "Spectral")
#' tsChips(tura, loc = c(820796, 831198), start = "1999-01-01", cols = "Greens")
#' 
#' tsChips(tura, loc = c(820796, 831198), start = "1999-01-01", cols = c("red", "yellow", "blue"))
#' tsChips(tura, loc = c(820796, 831198), start = "1999-01-01", cols = c("#DEEBF7", "#3182BD"))
#' 
#' # export image chips as a raster brick
#' chips <- tsChips(tura, loc = c(820796, 831198), start = "1999-01-01", percNA = 0, exportChips = TRUE)
#' chips2 <- tsChips(tura, loc = c(820796, 831198), start = "1999-01-01", percNA = 100, exportChips = TRUE)
#' nlayers(chips)
#' nlayers(chips2)
#' 
#' # export centre pixel time series as a zoo object
#' z <- tsChips(tura, loc = c(820796, 831198), start = "1999-01-01", exportZoo = TRUE)
#' plot(z)
#' 
#' # draw a custom SpatialPoygons object and plot around that
#' plot(tura, 42)
#' pol <- drawPoly(sp = TRUE) # click 'Finish' in plot window when done
#' projection(pol) <- projection(tura)
#' plot(tura, 42); plot(pol, add=TRUE)
#' tsChips(tura, loc = pol, start = "1999-01-01", plot = TRUE, plotlab = 'NDVI')
#' }


tsChips <- function(x, loc, start = NULL, end = NULL, buff = 17, percNA = 20, cols = "PiYG", nbks = 35, nc = 3, nr = 3, plot = FALSE, plotlab = 'data', exportChips = FALSE, exportZoo = FALSE, show = TRUE) {
  
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
    e <- extent(c(xmin(e) - buff, xmax(e) + buff, ymin(e) - buff, ymax(e) + buff))
  } else if(class(loc) == "extent"){
    e <- loc
    e <- extent(c(xmin(e) - buff, xmax(e) + buff, ymin(e) - buff, ymax(e) + buff))
  }
  
  # crop input brick
  xe <- crop(x, e)
  se <- getSceneinfo(names(xe))
  
  # start and end dates
  if(!is.null(start)){
    start <- as.Date(start)
    xe <- raster::subset(xe, subset = which(se$date >= start))
    se <- getSceneinfo(names(xe))
  } else {
    start <- as.Date(min(se$date)) # to be used in ggplot later
  }
  
  if(!is.null(end)){
    end <- as.Date(end)
    xe <- raster::subset(xe, subset = which(se$date <= end))
    se <- getSceneinfo(names(xe))
  } else {
    end <- as.Date(max(se$date)) # to be used in ggplot later
  }
  
  # reorder scenes
  xe <- raster::subset(xe, subset = order(se$date))
  se <- getSceneinfo(names(xe))
  
  # filter out scenes with too many NA's
  if(percNA > 100)
    percNA <- 100
  nas <- sapply(freq(xe), FUN=function(x) as.numeric(x[is.na(x[, 1]), 2] / ncell(xe) * 100))
  nas[which(sapply(nas, length) == 0)] <- 0
  nas <- unlist(nas)
  if(percNA == 0){
    xe <- raster::subset(xe, subset = which(nas == percNA))
  } else {
    xe <- raster::subset(xe, subset = which(nas < percNA))
  }
  
  # final sceneinfo data.frame
  se <- getSceneinfo(names(xe))
  
  # colour map
  if(length(cols) == 1){
    require(RColorBrewer)
    cols <- colorRampPalette(brewer.pal(9, cols))(nbks)
  } else {
    cols <- colorRampPalette(cols)(nbks)
  }
  
  # breaks defined based on extreme values
  minbk <- minValue(xe)
  if(!any(!is.na(minbk)))
    stop("No non-NA values in the defined image chips.")
  minbk <- min(minbk)
  maxbk <- maxValue(xe)
  if(!any(!is.na(maxbk)))
    stop("No non-NA values in the defined image chips.")
  maxbk <- max(maxbk)
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
  nscreens <- ceiling(nlayers(xe) / pps)
  for(i in seq(1, nlayers(xe), by = pps)){
    if((nlayers(xe) - i) < pps){
      xes <- raster::subset(xe, subset = c(i:nlayers(xe)))
      par(op)
      plot(xes, breaks = breaks, col = cols, main = getSceneinfo(names(xes))$date, legend=FALSE, nc = nc, nr = nr, addfun = addfun)
    } else {
      xes <- raster::subset(xe, subset = c(i:(i + pps - 1)))
      plot(xes, breaks = breaks, col = cols, main = getSceneinfo(names(xes))$date, legend=FALSE, nc = nc, nr = nr, addfun = addfun)
      readline("Press any key to continue to next screen: \n")
    }
  }
  
  # prepare zoo objects
  if(plot | exportZoo){
    if(is.numeric(loc)){
      z <- x[cellFromXY(x, loc)][1, ]
    } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
      z <- apply(extract(x, loc)[[1]], 2, mean)
    } else if(class(loc) == 'Extent') {
      loc <- c(mean(xmin(e), xmax(e)), mean(ymin(e), ymax(e)))
      z <- x[cellFromXY(x, loc)][1, ]
    } else {
      z <- x$R[cellFromXY(x$R, as.vector(coordinates(loc)))][1, ]
    }
    z <- zoo(z, s$date)
    z <- na.omit(z)
    z <- window(z, start = start, end = end)
  }
  
  # plot pixel time series
  if(plot) {
    readline("Press any key to view time series plots: \n")
    par(mfrow = c(1, 1))
    plot(z, xlab = 'Time', type = 'b', pch = '*', ylab = plotlab)
  }
  
  # decide what to return
  if(exportChips & exportZoo){
    return(list(tsChips = xe, zoo = z))
  } else if(exportChips & !exportZoo) {
    return(xe)
  } else if(!exportChips & exportZoo) {
    return(z)
  } else {
    return(NULL)
  }
}
