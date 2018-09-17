#' determine index of downstream
#' \code{sp.RiverOrder} 
#' @param sp SpatialLines*
#' @param coord coordinates of \code{sp}. 
#' @return Index of downstream for each segements
#' @export
sp.RiverDown <- function(sp, coord = extractCoords(sp)){
  ft = FromToNode(sp, coord = coord)
  nsp = length(sp)
  idown = rep(-3, nsp)
  for(i in 1:nsp){
    pto = ft[i,2]
    id = which(ft[,1] == pto)
    if(length(id) == 1){
      idown[i] = id
    }
  }
  idown
}
#' calculate river order
#' \code{sp.RiverOrder} 
#' @param sp SpatialLines*
#' @param coord coordinates of \code{sp}. 
#' @return Stream Order of SpatialLines*
#' @export
sp.RiverOrder <- function(sp, coord = extractCoords(sp)){
  get1st <- function(x){
    fr = x[,2]
    to = x[,3]
    tb= table(x[,2:3])
    pid = as.numeric(names(tb))
    ncount = as.numeric(tb)
    p.out = to[which(! to %in% fr)]
    p.jnt = pid[which(ncount> 2)]
    p.key = c(p.out, p.jnt)
    # print(p.key)
    tid=fr[ !fr %in% to] #target from-id
    # plot(riv.simp)
    # points(coord[ ])
    # points(coord[p.key,], col=2)
    nd = length(tid)
    ret = NULL
    # message('Number of Streams = ', nd)
    for(i in 1:nd){
      cr = tid[i]
      for(j in 1:100000){
        # message('Stream ',i,'\tSeg ', j, '\tFrom Node ', cr)
        rid = which(fr == cr)
        ret=c(ret, rid)
        sid= to[rid] #ID of to-point;
        if( sid  %in% p.key){ #if sid IS IN key-points(outlets or joint point).
          break;
        }else{
          cr = sid;
        }
      }
    }
    ret;
  }
  
  # ext=raster::extent(sp)
  # sp.tmp =  rgeos::gSimplify(sp, tol=max(diff(ext[1:2] ), diff(ext[3:4])) )
  ft = FromToNode(sp, coord)
  x = cbind(1:length(sp), ft)
  y =x
  x.ord =x[,1]*0
  for(i in 1:10000){
    ids = y[,1]
    message('Order = ', i)
    id = get1st(y)
    x.ord[ ids[id] ] = i
    y=y[-1 * id ,]
    ny = length(y)
    # message('Left Segments =', ny)
    if(ny<=0){ break }
  }
  x.ord
}

#' return the FROM and TO nodes index of the SpatialLines
#' \code{FromToNode} 
#' @param sp SpatialLines*
#' @param coord Coordinate of vertex in \code{sp}
#' @return FROM and TO nodes index of the SpatialLines
#' @export
FromToNode <- function(sp, coord = extractCoords(sp) ){
  nsp=length(sp)
  frto = matrix(0,nrow=nsp, ncol=2)
  for(i in 1:nsp){
    pg = extractCoords(sp[i,])
    id = xy2ID(pg, coord)
    frto[i,] = c(id[1], id[length(id)])
  }
  colnames(frto)=c('FrNode', 'ToNode')
  frto
}

#' parameters for river types
#' \code{RiverType} 
#' @param n number of types
#' @return data.frame of parameters
#' @export
RiverType <- function(n){
  cn = c('Index', 'Depth', 'BankSlope',
                      'Width', 'Sinuosity', 'Manning', 
                      'Cwr', 'KsatH')
  nc = length(cn)
  rtype = cbind(1:n,
                0.5 * 1:n, #Depth
                0, #bankslope
                2*1:n, #Width
                1.1, #Sinuosity
                4.63e-07, #manning's n, day/m^(1/3)
                0.6, #CWR
                0.1 #KsatH
                )
  colnames(rtype) = cn
  rtype
}
#' Cut the \code{PIHM.river} by \code{PIHM.mesh}
#' \code{sp.RiverSeg} 
#' @param pm \code{PIHM.mesh}
#' @param pr \code{PIHM.river} 
#' @return SpatialLinesDataFrame, the \code{PIHM.river} was cut by \code{PIHM.mesh}
#' @export
sp.RiverSeg <- function(pm, pr){
  sp  = sp.mesh2Shape(pm)
  sp@data = data.frame('iEle'=1:length(sp))
  sr = sp.riv2shp(pr)
  sr@data = data.frame('iRiv' = 1:length(sr))
  seg = raster::intersect(sr, sp)  
  seg
}

#' Dissolve River segments
#' \code{sp.RiverDissolve} 
#' @param sp SpatialLines
#' @return SpatialLinesDataFrame
#' @export
sp.DissolveLines <- function(sp){
  id2Line <- function(id, key){
    lp=key
    ct = count(as.numeric(id))
    cn = as.numeric( names(ct))
    # ctf = count(id[,1]) #count of frNode
    # ctt = count(id[,2])
    lfr = which(id[,1] == key)
    pto = which( cn == id[lfr,2] )
    if( ct[pto] == 2 ) {
      # 1 - header/outlet, >2 - joint point
      # message(key, '\t', pto)
      ret = id2Line(id, key = pto)
      return (c(lp, ret) )
    }else{
      return(c(key, cn[pto]) )
    }
  }
  cdu = extractCoords(sp, unique = T)
  ft=FromToNode(sp)
  ct= count(ft)
  pnode = as.numeric(names(ct))[which(ct != 2)]
  
  # plot(sp)
  sl=list()
  for(i in 1:length(pnode)){
    key = pnode[i]
    if( length(which(ft[,1] == key)) > 0){
      ids = id2Line(ft, key)
      # cdf[ids] = i
      ln = Line( cdu[ids,] )
      sl[[i]] = sp::Lines(ln, ID=i)
    }
  }
  sll = sp::SpatialLines(sl)
  # ord= sp.RiverOrder(sll)
  # df = data.frame('INDEX'=1:length(sl), 'order'= ord) 
  df = data.frame('INDEX'=1:length(sl))
  sld = sp::SpatialLinesDataFrame(sll, data = df)
  sld
}