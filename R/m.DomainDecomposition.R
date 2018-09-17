#' Generate triangle mesh domain
#' \code{m.DomainDecomposition} 
#' @param wb SpatialPolygon or SpatialLines which define the watershed boundary
#' @param riv SpatialLines of river network, optional
#' @param q minimum angle of triangle
#' @param ... more options in RTriangle::triangulate()
#' @return Coordinates of triangles centroids
#' @export
m.DomainDecomposition <- function(wb, riv=NULL, q=30, ...){
  #x is a spatialpolygons
  # wb=wb$wb.simp
  # riv=riv$Riv.simp
  ps1 = sp2PSLG(wb)
  if(!is.null(riv)){
    ps2 = sp2PSLG(riv)
    n1 = nrow(ps1$P)
    ps=list('P' = rbind(ps1$P, ps2$P),
            'S' = rbind(ps1$S, ps2$S + n1) )
  }else{
    ps = ps1
  }
  p = RTriangle::pslg(P=ps$P,
           S = ps$S)
  if(q >35){
    q = 35;
  }
  tri <- RTriangle::triangulate(p, q=q,...)
  # plot(tri, asp=1)
  # points(ps1$P, col=2)
  # points(ps2$P, col=3)
  # plot(riv, add=TRUE, col=4)
  tri
}

#' Generate triangle mesh domain
#' \code{sp.mesh2Shape} 
#' @param pm PIHM mesh object
#' @param dbf attribute table of the mesh triangles.
#' @return SpatialPolygons object
#' @export
sp.mesh2Shape <- function(pm=readmesh(), dbf=NULL){
  tt = pm@mesh[,2:4]
  pp=pm@point[,2:3]
  dd = pm@point[,4]
  zz = pm@point[,5]
  tri = list('T'=tt, 'P'=pp)
  if(is.null(dbf)){
    aqd = (dd[tt[,1]] + dd[tt[,2]] + dd[tt[,3]] ) /3
    zs = (zz[tt[,1]] + zz[tt[,2]] + zz[tt[,3]] ) /3
    dbf = cbind(pm@mesh, 'AqDepth'= aqd, 'Zsurf'=zs)
  }
  ret <- sp.Tri2Shape(tri, dbf=dbf)
}
#' Generate triangle mesh domain
#' \code{sp.Tri2Shape} 
#' @param tri triangulate 
#' @param dbf attribute table, data.frame or matrix
#' @param crs Projection 
#' @return Coordinates of triangles centroids
#' @export
sp.Tri2Shape <- function(tri, dbf=NULL, crs=NA){
  ta = tri$T
  pt = tri$P

  ncell = nrow(ta)
  p.x = pt[,1]
  p.y = pt[,2]
  ipt = t (ta[,c(1:3,1)] )
  xp = matrix( p.x[ipt], nrow=4)
  yp = matrix( p.y[ipt], nrow=4)
  pgs = list()
  ia = rep(0,ncell)
  for (i in 1:ncell){
    ipg = sp::Polygon(cbind(xp[,i], yp[,i]))
    ia[i] = polygonArea(xp[,i], yp[,i])
    pgs[[i]] = sp::Polygons(list(ipg), i)
  }
  pog = sp::SpatialPolygons(pgs)
  dbf=cbind(dbf, 'Area'=ia)
  rownames(dbf) = names(pog)
  ret = sp::SpatialPolygonsDataFrame(Sr=pog, data=as.data.frame(dbf))
  if(!is.na(crs) ){
    raster::crs(ret) = crs
  }
  ret
}

