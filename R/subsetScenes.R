#' @title Subset Scenes
#' 
#' @description Subset scenes from a list or directory based on a given spatial object. Takes results of \code{selectScenes()} and subsets based on intersecting extents.
#' 
#' @param x A spatial object (polygon, or otherwise) to base the subset extent on
#' @param targ A character vector of filenames, or a list of raster layers to be subsetted
#' @param filename Character. Optional: If results are to be written to file, then a character vector of \code{length = length(targ)} should be provided. Not supported yet (leave blank, or it will return an error)
#' @param verbose Logical. Print status messages to the console?
#' 
#' @return A list of avialable rasters cropped to the extent of the input object + padding. Side effect: cropped rasters written to file if a filename vector is supplied (not supported yet).
#' 
#' @author Ben DeVries
#' 
#' @import raster
#' @import sp
#' @export

subsetScenes <- function(x, targ, padding=NULL, filename = "", verbose=TRUE, ...){
  
  # set padding (if not already done)
  if(!is.numeric(padding)){
    padding <- 0
  }
  
  # check if filenames correspond with targ
  if(filename != "" & length(filename) != length(targ)){
    stop("filename should be of same length as targ")
  }
  
  # narrow down targ vector/list based on extent overlaps
  targ <- selectScenes(x, targ, padding=padding, verbose=verbose)
  
  # adjust extent of the input object
  e <- extent(x)
  e <- extent(c(xmin(e) - padding,
                xmax(e) + padding,
                ymin(e) - padding,
                ymax(e) + padding))
  
  # read from file and subset these rasters and (optionally) write to output file
  if(is.character(targ)){
    # loop through files and subset and write separately
    b <- list()
    for(i in 1:length(targ)){
      b[[i]] <- brick(targ[i])
      # TODO: detect whether input files are single or multi-layered
      # if single, b <- raster(); else b <- brick()
      if(filename != ""){
        b[[i]] <- crop(b[[i]], e, filename=filename[i], ...)
      } else {
        b[[i]] <- crop(b[[i]], e)
      }
    }
  } else if(is.list(targ)){
    b <- targ
    # then, same as above:
    for(i in 1:length(b)){
      if(filename != ""){
        b[[i]] <- crop(b[[i]], e, filename=filename[i], ...)
      } else {
        b[[i]] <- crop(b[[i]], e)
      }
      # TODO: detect if resulting image is only NA's or defined background value (e.g. 0)
      # if yes, reject it
    }
  } else {
    stop("targ must be either a character vector (filenames) or a list of raster layers.")
  }
  
  # return a list of cropped raster layers
  return(b)
}