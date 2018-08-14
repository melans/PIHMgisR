#' Split SpatialLines into segments
#' \code{sp.Polylines2Lines} 
#' @param sl a SpatialLines* object
#' @return SpatialLines
#' @source  https://stackoverflow.com/questions/38700246/how-do-i-split-divide-polyline-shapefiles-into-equally-length-smaller-segments
#' @export
sp.Polylines2Lines <- function(sl){
  nsp = length(sl)
  n=1
  toArr <- function(x){
    n = nrow(x)
    arr = array(0, dim=c(2,2,n-1))
    for(i in 1:(n-1) ){
      arr[ , , i] = x[i+(0:1), ]
    }
    arr
  }
  arr2sl <-function(x){
    len = dim(x)[3]
    sls = list()
    for(i in 1:len){
      ln = sp::Line(arr[ , , i])
      sls[[i]] = sp::Lines(ln, ID = i)
    }
    sl = SpatialLines(sls)
  }
  
  arr = NULL
  for(i in 1:nsp){
    sub = toArr(extractCoords(sl[i,]))  
    arr = abind::abind(arr, sub)
  }
  
  ret <- arr2sl(arr)
  ret
}
