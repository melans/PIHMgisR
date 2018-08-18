#' Find the centroid of the triangles
#' \code{toCentroid} 
#' @param tri Triangle, N x 3 dimension
#' @param point Coordinates.
#' @return Coordinates of triangles centroids
#' @export
toCentroid <- function(tri, point){
  x=cbind(point[tri[,1],1],
          point[tri[,2],1],
          point[tri[,3],1])
  y=cbind(point[tri[,1],2],
          point[tri[,2],2],
          point[tri[,3],2])
  xc=rowMeans(x)
  yc=rowMeans(y)
  ret <- cbind('x'=xc,'y'=yc)
}

#' Eudilic Distance 
#' \code{Eudist} 
#' @param p1 coordinates of point 1
#' @param p2 coordinates of point 2
#' @return Distance between \code{p1} and \code{p2}
#' @export
Eudist <- function(p1,p2){
  d = sqrt( sum( (p1-p2) ^2) )
  d
}

#' extract Coordinates of SpatialLines or  SpatialPolygons
#' \code{extractCoords} 
#' @param sp SpatialLines or SpatialPolygons
#' @param unique whether return unique coordinates or duplicated coordinates
#' @return coordinates of points, m x 2.
#' @export
extractCoords<-function(sp, unique=TRUE){
  spl <- methods::as(sp, "SpatialLines")  
  x = coordinates(spl)
  xy = NULL
  for(i in 1:length(x)){
    for(j in 1:length(x[[i]]) ){
      xy = rbind(xy, x[[i]][[j]])
    }
  }
  if (unique){
    xy = unique(xy)
  }
  xy
}

#' Convert the \code{xy} (X,Y) into Index in \code{coords}
#' \code{xy2ID} 
#' @param xy coordinate (x,y), subset of \code{coords}
#' @param coord coordinate set
#' @return Index in coodinate set
#' @export
xy2ID <- function(xy, coord){
  ng=nrow(xy)
  id=rep(0,ng)
  for(i in 1:ng){
    dd = which(rowMatch(xy[i,], coord))
    # print(dd)
    if(length(dd) > 0){
      id[i]= dd
    }
  }
  id
}

#' Convert the \code{xy} (X,Y) into Index in \code{coords}
#' \code{count} 
#' @param x target value
#' @param n specific frequency
#' @return count of specific frequency
#' @export
count <- function(x, n=NULL){
  if(is.null(n)){
    rt = table(x)
  }else{
    ct = table(x)
    rt = ct[which(ct %in% n)]
  }
  rt
}


#' Convert SpatialLines or SpatialPolygons a Planar Straight Line Graph object
#' \code{sp2PSLG} 
#' @param sp SpatialLines or SpatialPolygons
#' @return pslg class
#' @export 
sp2PSLG<-function(sp){
  sl = methods::as(sp, 'SpatialLines')
  nsl = length(sl)
  P = extractCoords(sl, unique = TRUE)
  S=NULL
  for(i in 1:nsl){
    cc = extractCoords(sl[i,], unique = FALSE)
    e = xy2ID(cc,P)  
    S = rbind(S, cbind(e[-length(e)], e[-1]))
  }
  ret <- list('P'=P,'S'=S)
}
