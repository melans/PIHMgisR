#' Generate the PIHM mesh data from the triangulation
#' \code{pihmMesh} 
#' @param tri Triangles defination
#' @param dem Elevation. Projection of the DEM raster must be same as watershed boundary.
#' @param AqDepth Aquifer depth, numeric.
#' @param r.aq  Aquifer Thickness. Raster object
#' @return Triangle mesh of model domain.
#' @export
pihmMesh <- function(tri, dem, AqDepth = 10, r.aq = dem * 0 + AqDepth ){
  topo=triTopology(tri$T)
  pt = tri$P;
  m = data.frame(1:nrow(topo), tri$T, topo[,2:4])
  colnames(m) = c('ID', paste0('Node', 1:3), paste0('Nabr',1:3))
  zs=raster::extract(dem, pt)
  aq=raster::extract(r.aq, pt)
  pt = data.frame(1:nrow(pt), pt, aq, zs)
  colnames(pt) = c('ID', 'X','Y', 'AqDepth', 'Elevation');
  mm=PIHM.mesh(mesh=m, point=pt)
}

#' Centroids of the triangulation
#' \code{Tri2Centroid} 
#' @param tri Triangles defination
#' @return Centroids of the triangles, m x 2;
#' @export
Tri2Centroid <- function(tri){
  tt=tri$T;
  pt=tri$P;
  xc= (pt[tt[,1],1] + pt[tt[,2],1] + pt[tt[,3],1]) / 3;
  yc= (pt[tt[,1],2] + pt[tt[,2],2] + pt[tt[,3],2]) / 3;
  ret <- cbind(xc,yc)
}
#' extract Coordinates of SpatialLines or  SpatialPolygons
#' \code{pihmAtt} 
#' @param tri Triangles defination
#' @param r.soil raster of soil classification
#' @param r.geol raster of geology layer 
#' @param r.lc raster of land cover, LAI and Roughness Length
#' @param r.forc raster of forcing data
#' @param r.mf raster of melt factor
#' @param r.bc raster of boundary condition
#' @return data.frame of PIHM .att
#' @export
pihmAtt <- function(tri, r.soil =NULL, r.geol=NULL, r.lc=NULL, r.forc=NULL,
                    r.mf = NULL, r.bc = NULL){
  pt = Tri2Centroid(tri)
  ncell = nrow(pt)
  atthead=c( "INDEX",  "SOIL", "GEOL", "LC", 'FORC', 'MF', 'BC')
  nh = length(atthead)
  att = cbind(1:ncell, 1, 1, 1, 1, 1, 1)
  
  colnames(att) = atthead;
  if (!is.null(r.soil)){
    att[,2] = raster::extract(r.soil, pt)
  }
  if (!is.null(r.geol)){
    att[,3] = raster::extract(r.geol, pt)
  }
  if (!is.null(r.lc)){
    att[,4] = raster::extract(r.lc, pt)
  }
  if (!is.null(r.forc)){
    att[,5] = raster::extract(r.forc, pt)
  }
  if (!is.null(r.mf)){
    att[,6] = raster::extract(r.mf, pt)
  }
  if (!is.null(r.bc)){
    att[,7] = raster::extract(r.bc, pt)
  }
  att
}
