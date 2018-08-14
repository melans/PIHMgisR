#' Calculate the elevation and aquifer thickness of mesh cells
#' \code{getElevation} 
#' @param pm \code{PIHM.mesh}
#' @return elevation and aquifer thickness
#' @export
getElevation <- function(pm){
  pp=pm@point[,2:3]
  dd = pm@point[,4]
  zz = pm@point[,4]
  aqd = (dd[tt[,1]] + dd[tt[,2]] + dd[tt[,3]] ) /3
  zs = (zz[tt[,1]] + zz[tt[,2]] + zz[tt[,3]] ) /3
  ret <- cbind('AqD'= aqd, 'Zsurf' = zs)
}