#' Calculate the elevation of mesh cells
#' \code{getElevation} 
#' @param pm \code{PIHM.mesh}
#' @return elevation
#' @export
getElevation <- function(pm = readmesh()){
  tt <- pm@mesh[,2:4];
  zz = pm@point[,5]
  zs = (zz[tt[,1]] + zz[tt[,2]] + zz[tt[,3]] ) /3
  ret <- zs
}
#' Calculate the AquiferDepth of mesh cells
#' \code{getAquiferDepth} 
#' @param pm \code{PIHM.mesh}
#' @return aquifer thickness
#' @export
getAquiferDepth <- function(pm = readmesh()){
  tt <- pm@mesh[,2:4]
  dd = pm@point[,4]
  aqd = (dd[tt[,1]] + dd[tt[,2]] + dd[tt[,3]] ) /3
  ret <- aqd
}
#============
#' Calculate the area of the cells
#' \code{getArea} 
#' @param pm \code{PIHM.mesh}
#' @return Area of cells
#' @export
getArea <-function(pm=readmesh() ){
  msh <- pm@mesh;
  pts <- pm@point;
  tri <- msh[,2:4];
  m <- nrow(tri);
  x=t(matrix(c(pts[tri[,1],2],pts[tri[,2],2],pts[tri[,3],2],pts[tri[,1],2]),m,4) );
  y=t(matrix(c(pts[tri[,1],3],pts[tri[,2],3],pts[tri[,3],3],pts[tri[,1],3]),m,4) );
  # z=t(matrix(c(pts[tri[,1],4],pts[tri[,2],4],pts[tri[,3],4],pts[tri[,1],4]),m,4) );
  iarea <- geometry::polyarea(x,y);
  iarea
}

#============
#' Calculate the Vertex of the cells
#' \code{getVertex} 
#' @param pm \code{PIHM.mesh}
#' @return Vertex of \code{PIHM.mesh}, dim = c(ncell, vertex, 4), 3-dimension = c('x','y','AqD', 'zmax')
#' @export
getVertex <- function(pm = readmesh() ){
  msh <- pm@mesh;
  pts <- pm@point;
  nabr<-pm@mesh[,5:7]   #nabor or each cell.
  node<-pm@mesh[,2:4]    #vetex of each cell.
  
  xv       =cbind(pts[node[,1],2],pts[node[,2],2],pts[node[,3],2]); #end with v, means vertex of triangles.
  yv       =cbind(pts[node[,1],3],pts[node[,2],3],pts[node[,3],3]);
  aqd    =cbind(pts[node[,1],4],pts[node[,2],4],pts[node[,3],4]);
  zsurfv   =cbind(pts[node[,1],5],pts[node[,2],5],pts[node[,3],5]);
  
  ret = abind::abind(xv, yv, aqd, zsurfv,
                     along=3)
  dimnames(ret) = list(paste0('Cell.', 1:nrow(msh) ),
                       paste0('Vertex.', 1:3),
                       c('X','Y', 'AqD', 'ZMAX') )
  ret
}

#' Get the centroids of the cells
#' \code{getCentroid} 
#' @param pm \code{PIHM.mesh}
#' @return centroids of \code{PIHM.mesh}, ncell x 4 c('x','y','AqD', 'zmax')
#' @export
getCentroid <- function(pm = readmesh() ){
  x = getVertex(pm=pm);
  xc      =rowMeans(x[,,1]);      #end of c, means centroid of the triangles.
  yc      =rowMeans(x[,,2]);
  if(ncol(pm@mesh) ==  9){
    aqd   =pm@mesh[,8]
    zsurfc  =pm@mesh[,9]
  }else{
    aqd   =rowMeans(x[,,3]);
    zsurfc  =rowMeans(x[,,4]);
  }
  ret = cbind(xc, yc, aqd, zsurfc)
  colnames(ret) = c('X','Y', 'aqd', 'ZMAX')
  ret
}