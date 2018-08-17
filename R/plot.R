#' Raster plot the mesh data
#' \code{map2d}
#' @param x Vector or Matrix of mesh data. Length of nrow must be equal the number of cells
#' @param sp.riv The SpatialLines of river network. sp.riv=NULL means plot raster only.
#' @return Raster of the x (vector) or the last row of x (matrix)
#' @export  
map2d<-function(x=getElevation(),
                     sp.riv = sp.riv2shp() ){
  rmask = PIHM.mask()
  if(is.matrix(x) | is.data.frame(x)){
    y = as.numeric(x[nrow(x)])
  }else{
    y=x
  }
  r = MeshData2Raster(y, rmask)
  raster::plot(r)
  if(is.null(sp.riv)){
    #  bgcol= adjustcolor('gray80', alpha.f = 0.8)
    col = adjustcolor(sp.riv@data[,'Type'], alpha.f = 0.7)
    lwd=sp.riv@data[,'Type']
    # raster::plot(add=T, sp.riv, col=bgcol, lwd=lwd*3, lty=2)
    raster::plot(add=T, sp.riv, col=col, lwd=lwd)
  }
  r
}
