#' @title Plot TS Chips (RGB version)
#' 
#' @description Plot image TS chips from 3 raster bricks (as RGB composites) based on a location, area buffer, and time buffer
#' 
#' @param xr/xg/xb RasterBrick. Image time series bricks representing the red, green and blue channel, respectively
#' @param loc Location. Can be a vector of length 2 representing the x,y coordinates, or a SpatialPolygons or SpatialPoints object of \code{nrow = 1} (the first row will be taken if \code{nrow(loc) > 1}), or an extent object (which will be extended if a buffer > 0 is given; see below)
#' @param buff Numeric. Number of pixels to buffer the location in all directions. A higher buffer will essentially zoom out.
#' @param start Date. OptionaL: earliest date ("yyyy-dd-mm") to display.
#' @param end Date. Optional: latest date ("yyyy-dd-mm") to display.
#' @param percNA Numeric. Maximum allowable \% NA in the cropped image chips
#' @param nc/nr Numeric. Number of columns and rows to plot, respectively. If the number of layers is greater than \code{nc*nr}, a screen prompt will lead to the next series of plots. These cannot exceed 4.
#' @param plot Logical. Plot individual band time series?
#' @param exportChips Logical. Export processed chips to workspace as a list of rasterBricks (R, G, B)?
#' @param exportZoo Logical. Export pixel time series as \code{zoo} objects?
#' @param textcol Character. Colour of text showing image date (can also be hexadecimal)
#' @param show Logical. Show image chips? Set to \code{FALSE} if you just want to export them to rasterBricks and/or export the \code{ggplot} object without viewing the chips.
#' @param plotlabs Character. Vector of length 3 indicating the labels for each of the zoo plots (if \code{plot=TRUE})
#' @param ... Arguments to be passed to \code{\link{plotRGB}}
#' 
#' @return \code{NULL} if \code{exportZoo = FALSE} and \code{exportChips = FALSE} or a list of \code{zoo} time series objects if \code{exportZoo = TRUE}, or a list of subsetted and cropped rasterBricks if \code{exportChips = TRUE}, with the side effect of time series chips being plotted in all cases. If both \code{exportChips} and \code{exportZoo} are \code{TRUE}, a list consisting both lists will be returned.
#' 
#' @details see \link{http://bendv.github.io/timeSyncR/} for a short tutorial on \code{tsChipsRGB}
#' 
#' @author Ben DeVries
#' 
#' @import zoo
#' @export
#' 
#' @references
#' Cohen, W. B., Yang, Z., Kennedy, R. (2010). Detecting trends in forest disturbance and recovery using yearly Landsat time series: 2. TimeSync - Tools for calibration and validation. Remote Sensing of Environment, 114(12), 2911-2924.
#' 

tsChipsRGB <- function(xr, xg, xb, loc, start = NULL, end = NULL, buff = 17, percNA = 20, nc = 3, nr = 3, plot = FALSE, exportChips = FALSE, exportZoo = FALSE, textcol = "white", plotlabs = c('red', 'green', 'blue'), ...) {
  
  # check that all bricks have the same number of layers and are comparable
  if(!compareRaster(xr, xg, xb))
    stop("Input RGB rasterBricks do not compare")
  if(length(unique(nlayers(xr), nlayers(xg), nlayers(xb))) > 1)
    stop("Input RGB rasterBricks have different number of layers")
  
  x <- list(R = xr, G = xg, B = xb)
  
  # get sceneinfo
  s <- getSceneinfo(names(x$R))
  
  # set z-dimensions of each brick
  x <- lapply(x, FUN=function(x) setZ(x, s$date))
  
  # reformat buffer using image resolution
  buff <- buff * res(x$R)[1]
  
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
  
  # crop input bricks
  xe <- lapply(x, FUN=function(x) crop(x, e))
  
  # start and end dates
  if(!is.null(start)){
    start <- as.Date(start)
    xe <- lapply(xe, FUN=function(x) raster::subset(x, subset = which(getZ(x) >= start)))
    xe <- lapply(xe, FUN=function(x) setZ(x, getSceneinfo(names(x))$date))
  } else {
    start <- as.Date(min(getZ(xe[[1]]))) # to be used in ggplot later
  }
  
  if(!is.null(end)){
    end <- as.Date(end)
    xe <- lapply(xe, FUN=function(x) raster::subset(x, subset = which(getZ(x) <= end)))
    xe <- lapply(xe, FUN=function(x) setZ(x, getSceneinfo(names(x))$date))
  } else {
    end <- as.Date(max(getZ(xe[[1]]))) # to be used in ggplot later
  }  
  
  # reorder scenes
  xe <- lapply(xe, FUN=function(x) raster::subset(x, subset = order(getZ(x))))
  
  # filter out scenes with too many NA's
  # done on 1st band, assuming mask has been applied uniformly
  if(percNA < 100){
    nas <- freq(xe[[1]], value = NA) / ncell(xe[[1]]) * 100
    for(i in 1:length(xe)){
      if(percNA == 0){
        xe[[i]] <- raster::subset(xe[[i]], subset = which(nas == percNA))
      } else {
        xe[[i]] <- raster::subset(xe[[i]], subset = which(nas < percNA))
      }
    }
  }
  
  # function to add spatial data (if present)
  if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialPoints", "SpatialPointsDataFrame")){
    addfun <- function() plot(loc, add=TRUE)
  } else {
    addfun <- function() NULL
  }
  
  # plots on separate screens if needed
  op <- par(mfrow = c(nr, nc))
  pps <- nc * nr
  nscreens <- ceiling(nlayers(xe[[1]]) / pps)
  
  for(i in seq(1, nlayers(xe[[1]]), by = pps)){
    if((nlayers(xe[[1]]) - i) <= pps){
      xes <- lapply(xe, FUN=function(y) raster::subset(y, subset = c(i:nlayers(y))))
      for(j in 1:nlayers(xes[[1]])){
        b <- brick(raster(xes[[1]], j), raster(xes[[2]], j), raster(xes[[3]], j))
        err <- try({
          plotRGB(b, 1, 2, 3, addfun=addfun, ...)
          text(x = (xmin(e) + xmax(e))/2, y = ymin(e) + 2*res(xr)[1], labels = getZ(xes[[1]])[j], col = textcol)
        }, silent = TRUE)
        if(class(err) == "try-error")
          plot.new()
      }
      par(op)
    } else {
      xes <- lapply(xe, FUN=function(y) raster::subset(y, subset = c(i:(i + pps - 1))))
      for(j in 1:nlayers(xes[[1]])){
        b <- brick(raster(xes[[1]], j), raster(xes[[2]], j), raster(xes[[3]], j))
        err <- try({
          plotRGB(b, 1, 2, 3, addfun=addfun, ...)
          text(x = (xmin(e) + xmax(e))/2, y = ymin(e) + 2*res(xr)[1], labels = getZ(xes[[1]])[j], col = textcol)
        }, silent = TRUE)
        #if(class(err) == "try-error")
        # plot.new()
      }
      readline("Press any key to see next screen:\n")
    }
  }
  
  
  # prepare zoo objects
  if(plot | exportZoo){
    if(is.numeric(loc)){
      z <- list(R = x$R[cellFromXY(x$R, loc)][1, ],
                G = x$G[cellFromXY(x$G, loc)][1, ],
                B = x$B[cellFromXY(x$B, loc)][1, ])
    } else if(class(loc) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
      z <- list(R = apply(extract(x$R, loc)[[1]], 2, mean),
                G = apply(extract(x$G, loc)[[1]], 2, mean),
                B = apply(extract(x$B, loc)[[1]], 2, mean))
    } else {
      z <- list(R = x$R[cellFromXY(x$R, as.vector(coordinates(loc)))][1, ],
                G = x$G[cellFromXY(x$G, as.vector(coordinates(loc)))][1, ],
                B = x$B[cellFromXY(x$B, as.vector(coordinates(loc)))][1, ])
    }
    z <- lapply(z, FUN=function(x) zoo(x, s$date))
    z <- lapply(z, na.omit)
    z <- lapply(z, FUN=function(x) window(x, start = start, end = end))
  }
  
  # plot
  if(plot) {
    readline("Press any key to view time series plots: \n")
    lo <- matrix(c(1:3), nr=3, nc=1)
    layout(lo)
    op <- par(mar = c(0, 5, 0, 5), oma = c(3, 3, 3, 3))
    plot(z$R, xlab = '', xaxt = 'n', type = 'b', pch = '*', ylab = plotlabs[1], col = 'red')
    plot(z$G, xlab = '', xaxt = 'n', yaxt = 'n', type = 'b', pch = '*', ylab = plotlabs[2], col = 'dark green')
    axis(4)
    plot(z$B, xlab = '', xaxt = 'n', type = 'b', pch = '*', ylab = plotlabs[3], col = 'blue')
    datelab <- seq.Date(as.Date(start), as.Date(end), by = '1 year')
    axis(1, at = datelab, labels = format(datelab, format = '%Y'))
    
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
