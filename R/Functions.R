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
#' @examples 
#' p1 = c(1,1)
#' p2 = c(2,2)
#' Eudist(p1,p2)
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
#' @examples 
#' data('sac')
#' wb=sac[['wbd']]
#' extractCoords(wb)
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
#' @examples 
#' coord = matrix(rnorm(10), 5,2)
#' xy = coord[c(5,1,3),]
#' xy2ID(xy, coord)
xy2ID <- function(xy, coord){
  if(is.matrix(xy) | is.data.frame(xy)){
  }else{
    xy = matrix(xy, ncol=2)
  }
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
#' @examples 
#' x=round(rnorm(100, 2))
#' count(x)
count <- function(x, n=NULL){
  if(is.null(n)){
    rt = table(x)
  }else{
    ct = table(x)
    rt = ct[which(ct %in% n)]
  }
  rt
}

#' Find the outliers in a dataset
#' \code{which_outliers} 
#' @param x dataset
#' @param na.rm Whether ignore the na value.
#' @param probs Range of non-outlier range
#' @param ... More options in quatile();
#' @return Index of the outliers
#' @export 
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' x <- c(-10, x, 10)
#' id <- which_outliers(x, probs=c(0.05,0.95))
#' y = x
#' y[id]=NA
#' par(mfrow = c(1, 2))
#' boxplot(x, ylim=range(x))
#' boxplot(y, ylim=range(x))
#' par(mfrow=c(1,1))
which_outliers <- function(x, na.rm = TRUE, probs=c(.5, .95),...) {
  qnt <- stats::quantile(x, probs=probs, na.rm = na.rm, ...)
  H <- 1.5 * stats::IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  id=which(is.na(y))
  id
}
#' par(mfrow=c(1,1))
#' Convert SpatialLines or SpatialPolygons a Planar Straight Line Graph object
#' \code{sp2PSLG} 
#' @param sp SpatialLines or SpatialPolygons
#' @return pslg class
#' @export 
#' @examples
#' library(rgeos) 
#' sl = readWKT("MULTIPOLYGON(((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((6 3,9 2,9 4,6 3)))")
#' x = sp2PSLG(sl)
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
