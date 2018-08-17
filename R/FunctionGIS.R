#' SpatialData to Raster
#' \code{sp2raster} 
#' @param sp SpatialPolygon
#' @param mask Raster mask of mesh domain.
#' @param ngrids Number of grid along x direction.
#' @param resolution Resolution, defaul = NULL, resolution = extent / ngrids
#' @param field Index of field
#' @return Raster map
#' @export
sp2raster <- function (sp, mask = get('PIHM.MASK', envir = .pihm),
                       ngrids=200, 
                       resolution=NULL, field=1) {
  if( is.null(mask) ){
    ext <-  raster::extent (sp)
    xlim=ext[1:2]
    ylim=ext[3:4]
    if ( resolution<=0 || is.null(resolution)){
      dx=diff(xlim) / ngrids;
    }else{
      dx=resolution
    }
    r <- raster::raster(ext, res=dx)
  }else{
    r = mask
  }
  ## Rasterize the shapefile
  rr <-raster::rasterize(sp, r, field=field)
  return(rr)
}

#' Generate the raster mask of Mesh domain
#' \code{PIHM.mask} 
#' @param pm \code{PIHM.mesh}
#' @param ngrids number of grid along x direction.
#' @param resolution resolution, defaul = NULL, resolution = extent / ngrids
#' @return Raster map
#' @export
PIHM.mask  <- function (pm = readmesh(), 
                        ngrids=200, resolution=NULL){
  # mesh=readmesh(shp=TRUE); ngrids=100; resolution=0
  # if(file.exists(RDSfile) & !(reload)){
  #   r <- readRDS(file=RDSfile)
  # }else{
  mask = get('PIHM.MASK', envir = .pihm)
  if(is.null(mask)){
    sp = sp.mesh2Shape(pm)
    
    ext <-  raster::extent (sp)
    xlim=ext[1:2]
    ylim=ext[3:4]
    if ( resolution<=0 || is.null(resolution)){
      dx=diff(xlim) / ngrids;
    }else{
      dx=resolution
    }
    r <- raster::raster(ext, res=dx)
    spd = rgeos::gUnionCascaded(sp)
    rr <-raster::rasterize(spd, r)
    # raster::plot(spd)
    # raster::plot(rr, add=T)
    # saveRDS(file=RDSfile, r)
    # }
    # assign("PIHM.MASK",rr, envir = .pihm)
    assign('PIHM.MASK', rr, envir=.pihm)
  }else{
    rr = mask
  }
  rr
}

#' SpatialData to Raster
#' \code{MeshData2Raster} 
#' @param x vector or matrix, length/nrow is number of cells.
#' @param mask mask of PIHM mesh
#' @return Raster map
#' @export
MeshData2Raster <- function(x=getElevation(),
                            mask=PIHM.mask()){
  #Interpolate and write ASC map out.
  # loadinglib('akima')
  if( is.matrix(x) | is.data.frame(x)){
    x = x[nrow(x),]
  }
  if(any(is.na(x)) ){
    x[is.na(x)] = 0
  }
  if (any(is.infinite(x))){
    x[is.infinite(x)] = 0
  }
  xy=getCentroid()[,1:2]
  tps <- fields::Tps(xy, x)
  
  # use model to predict values at all locations
  r <- raster::interpolate(mask, tps)
  ret <- raster::mask(r,mask)
}

