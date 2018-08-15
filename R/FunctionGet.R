#' Calculate the elevation and aquifer thickness of mesh cells
#' \code{getElevation} 
#' @param pm \code{PIHM.mesh}
#' @return elevation and aquifer thickness
#' @export
getElevation <- function(pm){
  tt <- pm@mesh[,2:4];
  pp=pm@point[,2:3]
  dd = pm@point[,4]
  zz = pm@point[,4]
  aqd = (dd[tt[,1]] + dd[tt[,2]] + dd[tt[,3]] ) /3
  zs = (zz[tt[,1]] + zz[tt[,2]] + zz[tt[,3]] ) /3
  ret <- cbind('AqD'= aqd, 'Zsurf' = zs)
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
