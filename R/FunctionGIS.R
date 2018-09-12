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
#' @param ngrids Number of grid along x direction.
#' @param rr Default mask in .pihm environment
#' @param resolution Resolution, defaul = NULL, resolution = extent / ngrids
#' @return Raster map
#' @export
PIHM.mask  <- function (pm = readmesh(), rr = get('PIHM.MASK', envir = .pihm),
                        ngrids=200, resolution=NULL){
  # mesh=readmesh(shp=TRUE); ngrids=100; resolution=0
  if(is.null(rr)){
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
    # raster::plot(rr, add=TRUE)
    # saveRDS(file=RDSfile, r)
    # }
    # assign("PIHM.MASK",rr, envir = .pihm)
    assign('PIHM.MASK', rr, envir=.pihm)
  }else{
    rr = rr
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
                            rmask=PIHM.mask()){
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
  r <- raster::interpolate(rmask, tps)
  ret <- raster::mask(r,rmask)
}


#' Remove the holes in polygons
#' \code{removeholes} 
#' @param sp SpatialPolygons or SpatialPolygonDataFrame
#' @return Raster map
#' @export
#' @examples 
#' library(sp)
#' p.out = Polygon(cbind(c(4,4,6,7,4),c(5,3,2,5,5))  )
#' p.hole = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4) ), hole = TRUE)
#' sp <- SpatialPolygons(list(Polygons(list(p.out, p.hole), "1")))
#' s = removeholes(sp)
#' par(mfrow=c(1,2))
#' plot(sp)
#' plot(s)
removeholes <- function(sp){
  x = sp
  nx = length(x)
  # vn = rownames(x@data)
  rl = list()
  for(i in 1:nx){
    spg = x@polygons[[i]]@Polygons
    npg = length(spg)
    ypg = list()
    k=1
    for(j in 1:npg){
      if(spg[[j]]@hole){
      }else{
        ypg[[k]] = spg[[j]]
        k = k+1
      }
    }
    rl[[i]] = sp::Polygons(ypg, ID=i)
  }
  ysp = sp::SpatialPolygons(rl)
  # ret = SpatialPolygonsDataFrame(ysp, data=x@data)
  ret = ysp
  if( !is.na(raster::crs(x)) & ! is.null(raster::crs(x)) ){
    raster::crs(ret) = raster::crs(x)
  }
  ret
}
#' Generatue fishnet
#' \code{fishnet} 
#' @param ext Extension of the fishnet. c(xmin, xmax, ymin, ymax)
#' @param dx Interval of x direction
#' @param dy Interval of y direction
#' @param crs Projection 
#' @param polygons Whether to export SpatialPolygons
#' @param points Whether to export SpatialPoints
#' @param lines Whether to export SpatialLines
#' @examples
#' library(raster)
#' pg=fishnet(ext=c(-80,80, -50,50), dx=5)
#' plot(pg)
fishnet <- function(ext, crs=sp::CRS("+init=epsg:4326"), dx=1, dy=dx,
                    lines=FALSE, polygons=TRUE, points=FALSE){
  xmin = ext[1]
  xmax = ext[2]
  ymin = ext[3]
  ymax = ext[4]
  xx=seq(ext[1], ext[2], by=dx)
  yy=seq(ext[3], ext[4], by=dy)
  nx = length(xx)
  ny = length(yy)
  if(lines){
    vline=cbind(xx, ymin, xx, ymax)
    hline =  cbind(xmin, yy, xmax, yy)
    mat = rbind(vline, hline)
    str=paste(paste('(', mat[,1],mat[,2],',', mat[,3], mat[,4], ')'), collapse = ',')
    spl=rgeos::readWKT(paste('MULTILINESTRING(', str, ')'))
    df = as.data.frame(mat)
    colnames(df) = c('x1', 'y1', 'x2','y2')
    spdf=sp::SpatialLinesDataFrame(spl, data = df)
    ret = spdf
  }
  if(polygons){
    xy=expand.grid(xx,yy)
    xm = matrix(xy[,1], nx,ny)
    ym = matrix(xy[,2], nx, ny)
    xloc=abind::abind(xm[-nx, -ny], xm[-nx, -1], xm[-1, -1], xm[-1, -ny], xm[-nx, -ny], along=3)
    yloc=abind::abind(ym[-nx, -ny], ym[-nx, -1], ym[-1, -1], ym[-1, -ny], ym[-nx, -ny], along=3)
    pgs=list()
    df=as.data.frame(matrix(0, nrow=(nx-1)*(ny-1), 6))
    colnames(df) = c('xmin','xmax','ymin', 'ymax','xcenter','ycenter')
    k=0
    for(i in 2:nx - 1){
      for(j in 2:ny - 1){   
        k=k+1
        ll = cbind(xloc[i,j, ], yloc[i,j, ])
        pg=sp::Polygon(ll)
        pgs[[k]] = sp::Polygons(list(pg), ID=k)
        df[k,]=c(min(ll[,1]), max(ll[,1]), min(ll[,2]), max(ll[,2]), mean(ll[,1]), mean(ll[,2]) )
      }
    }
    pg = sp::SpatialPolygons(pgs)
    pg.df = sp::SpatialPolygonsDataFrame(pg, data=df)
    ret = pg.df
  }
  # plot(ret, axes=T)
  raster::crs(ret) = crs
  ret
}
