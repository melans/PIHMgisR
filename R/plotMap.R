
#' Plot multiple maps
#' \code{compareMaps} 
#' @param r List of raster or SpatialData
#' @param mfrow mfrow in par()
#' @param contour Whether plot the contour.
#' @param ... More options in par()
#' @export
#' @examples 
#' library(raster)
#' data(volcano)
#' r <- raster(volcano)
#' extent(r) <- c(0, 610, 0, 870)
#' r1= sin(r/100)
#' r2= cos(r/100)
#' compareMaps(list(r,rr), mfrow=c(1,2))
#' compareMaps(list(r,r1,r2, r1+r2), mfrow=c(2,2))
#' compareMaps(list(r,r1,r2, r1+r2), mfrow=c(2,2), contour = T)
compareMaps <- function(r, mfrow, contour=FALSE, ...){
  nr = length(r)
  is.Raster <- function(x)  {
    return((class(x)=="RasterLayer" || class(x)=="RasterBrick" || class(x)=="RasterStack"))
  }
  par(mfrow = mfrow, ...)
  for(i in 1:nr){
    raster::plot(r[[i]], axes=FALSE, box=FALSE)
    if(contour){
      if(is.Raster(r[[i]])){
        raster::contour(r[[i]], add=TRUE)
      }
    }
  }
  grid()
  par(mfrow=c(1,1))
}