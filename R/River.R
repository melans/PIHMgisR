#' calculate river order
#' \code{pihmRiver} 
#' @param sp SpatialLines*
#' @param dem Raster of elevation
#' @return Stream Order of SpatialLines*
#' @export
pihmRiver <- function(sp, dem){
  sp.slt = sp.Polylines2Lines(sp)
  nsp = length(sp.slt)
  xy = data.frame(extractCoords(sp.slt))
  z = raster::extract(dem, xy)
  pt = cbind(1:nrow(xy), xy, z)
  colnames(pt) = c('Index','X', 'Y', 'Zmax')
  
  rivord = sp.RiverOrder(sp.slt)
  rivdown = sp.RiverDown(sp.slt)
  
  ft = FromToNode(sp.slt)
  row.names(sp.slt) = paste(1:nsp)
  df = data.frame('Index'=1:nsp, 
                  ft, 
                  'Down' = rivdown,
                  'Type' = rivord
  )
  sp.df = SpatialLinesDataFrame(sp.slt,
                                data=df)
  ntype = max(rivord)
  rtype = RiverType(ntype)
  
  PIHM.river(river = df, 
             rivertype = data.frame(rtype),
             point = data.frame(pt)
  )
}

#' Calculate the river length
#' \code{RiverLength} 
#' @param pr PIHM river class
#' @return River length, vector
#' @export
RiverLength<- function(pr){
  rv = pr@river
  pt = pr@point
  n = nrow(rv)
  pfr = rv[,2]
  pto = rv[,3]
  p0 = pt[pfr, 2:3]
  p1 = pt[pto, 2:3]
  d = rep(0, n)
  for(i in 1:n){
    d[i] <- Eudist(p0[i,],p1[i,])
  }
  d
}
#' Calculate the river slope
#' \code{RiverSlope} 
#' @param pr PIHM river class
#' @return River slopes, vector
#' @export
RiverSlope <- function(pr){
  rv = pr@river
  rt = pr@rivertype
  pt = pr@point
  
  pfr = rv[,'FrNode']
  pto = rv[,'ToNode']
  z0 = pt[pfr, 'Zmax']
  z1 = pt[pto, 'Zmax']
  len = RiverLength(pr)
  s0 = (z0 - z1) / len
  
  z = 0.5 * (z0 + z1)
  #=============
  idown = rv[, 'Down']
  zz = pt[pto, 'Zmax']
  oid=which(idown <= 0)
  idown[oid] = oid
  
  z0 = z
  z1 = z[idown]
  
  downlen = 0.5 * (len + len[idown] )
  s1 = (z0 - z1) / downlen
  s1[oid] = s0[oid]
  ret <- cbind(s0, s1)
}
#' Convert the bed slope of river, to avoid negative/zero slope
#' \code{correctRiverSlope} 
#' @param pr PIHM river class
#' @param minSlope Minimum slope 
#' @return SpatialLinesDataFrame
#' @export
correctRiverSlope <- function(pr, minSlope = 1e-4){
  # EPS = 1e-9
  # pr = ppr
  # minSlope = 1e-4
  nflag = 1
  pfr = pr@river[,'FrNode']
  pto = pr@river[,'ToNode']
  idown = pr@river[,'Down']
  len = RiverLength(pr)
  oid = getOutlets(pr)
  
  while(nflag > 0){
    pz = pr@point[, 'Zmax']
    s=RiverSlope(pr)
    rid = which(s[,1] < minSlope )
    nflag = length(rid)
    if(nflag >= 1){
      message(nflag, ' rivers in type 1 (reverse)')
      idn = idown[rid]
      od = which(idn %in% oid)
      idn[od] = rid[od]
      
        p1 = pfr[rid]
        p2 = pto[rid]
        p3 = pto[idn]
        l12 = len[rid]
        l23 = len[idn]
        ll = l12 + l23

      pz[p2] = (pz[p1] * l23 + pz[p3] * l12) / ll
      #pz[pto[rid]] = pz[pfr[rid]] - len[rid] * minSlope - EPS
      pr@point[,'Zmax'] = pz
    }
  }
  
  flag = 1
  idown = pr@river[,'Down']
  while(nflag > 0){
    pz = pr@point[, 'Zmax']
    s=RiverSlope(pr)
    rid = which(s[,2] < minSlope )
    nflag = length(rid)
    if(nflag > 0){
      message(nflag, ' rivers in type 2 (revers down)')
      p1 = pfr[rid]
      p2 = pto[rid]
      p3 = pto[idown[rid]]
      print(rid)
      pz[p2] = 0.5 * (pz[p1] + pz[p2])
      pr@point[,'Zmax'] = pz
    }
  }
  pr
}

#' Convert the PIHM.river class to SpatialLines
#' \code{sp.riv2shp} 
#' @param pr PIHM river class
#' @param dbf Attribute data for exported SpatialLines
#' @return SpatialLinesDataFrame
#' @export
sp.riv2shp <- function(pr = readriv(), dbf=NULL){
  pt = pr@point[,2:3]
  rt = pr@rivertype
  riv = pr@river
  
  nr = nrow(riv)
  nt = nrow(rt)
  np = nrow(pt)
  
  ft = riv[,2:3]
  sl=list()
  sls = list()
  for(i in 1:nr){
    coord = rbind(pt[ft[i,1], ], pt[ft[i,2], ])
    sl[[i]] = Line( coord )
    sls[[i]] = Lines(sl[[i]], i)
  }
  if(is.null(dbf)){
    df = data.frame(riv, rt[riv[, 'Type'], ])
  }else{
    df = dbf
  }
  spl = SpatialLines(sls)
  rownames(df) = names(spl)
  sp = SpatialLinesDataFrame(spl, data=df)
  sp
}

#' Generate the river segments table
#' \code{pihmRiverSeg} 
#' @param sl SpatialLinesDataFrame
#' @return river segments table
#' @export
pihmRiverSeg <- function(sl){
  df = data.frame('Index' = 1:length(sl), 
                  sl@data, 
                  'Length' = rgeos::gLength(sl, byid = T))
  df
}
#' Get outlet ID(s)
#' \code{getOutlets} 
#' @param pr PIHM river class
#' @return Index of outlets, numeric
#' @export
getOutlets <- function(pr){
  idown = pr@river[,'Down']
  ids = which(idown < 0)
  ids
}

