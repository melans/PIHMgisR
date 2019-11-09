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
#' determine River path
#' \code{sp.RiverPath}
#' @param sp SpatialLines*
#' @param idown coordinates of \code{sp}.
#' @return List of River Path
#' @export
sp.RiverPath <- function(sp, idown = sp.RiverDown(sp, coord = coord)){
  coord = extractCoords(sp)
  ft = FromToNode(sp, coord = coord)
  nsp = length(sp)
  jid = which(table(as.matrix(ft)) > 2) #id of joint points
  keys = c(which(idown<0), which(ft[,2] %in% jid) )
  updown = cbind(1:nsp, idown)
  goUp <- function(updown, id0){
    uid = which(idown == id0) # id of my upstream
    if(length(uid) == 1){ #up stream exist
      ret = c(goUp(updown, uid), id0)
    }else{
      ret = id0
    }
    ret
  }
  StreamPath = lapply(keys, function(x) goUp(updown, x))
  #========Point ID==========
  nstr = length(StreamPath)
  pl = list()
  for(i in 1:nstr){
    sid = StreamPath[[i]]
    pid = c(ft[sid, 1], ft[sid[length(sid)], 2])
    pl[[i]] = pid
  }
  #=========Spatial Lines===============
  ll = list()
  for(i in 1:nstr){
    sid = StreamPath[[i]]
    pid = c(ft[sid, 1], ft[sid[length(sid)], 2])
    ll[[i]] = sp::Lines( sp::Line(coord[pid, ] ), ID = i)
  }
  spx = sp::SpatialLines(ll, proj4string = raster::crs(sp))
  ret <- list(SegIDs = StreamPath,
              PointIDs= pl,
              sp = spx)
}
#' calculate river order
#' \code{sp.RiverOrder}
#' @param sp SpatialLines*
#' @param coord coordinates of \code{sp}.
#' @return Stream Order of SpatialLines*
#' @export
sp.RiverOrder <- function(sp, coord = extractCoords(sp)){
  msg='sp.RiverOrder::'
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
    # message(msg, 'Number of Streams = ', nd)
    for(i in 1:nd){
      cr = tid[i]
      for(j in 1:100000){
        # message(msg, 'Stream ',i,'\tSeg ', j, '\tFrom Node ', cr)
        rid = which(fr %in% cr)
        ret=c(ret, rid)
        sid= to[rid] #ID of to-point;
        # if(length(sid) < 1 | length(sid)>1){
        #   print(sid)
        # }
        if( any(sid  %in% p.key) ){ #if sid IS IN key-points(outlets or joint point).
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
  ft = unique(FromToNode(sp, coord))
  x = cbind(1:length(sp), ft)
  y =x
  x.ord =x[,1]*0
  for(i in 1:10000){
    ids = y[,1]
    message(msg, 'Order = ', i)
    id = get1st(y)
    x.ord[ ids[id] ] = i
    y=rbind(y[-1 * id ,])
    ny = length(y)
    # message(msg, 'Left Segments =', ny)
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
  # plot(sp, axes=T);
  for(i in 1:nsp){
    pg = extractCoords(sp[i,])
    id = xy2ID(pg, coord)
    frto[i,] = c(id[1], id[length(id)])
    # points(coord[frto[i,], 1:2], col=i, pch=i)
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
         'Cwr', 'KsatH', 'BedThick')
  nc = length(cn)
  rtype = cbind(1:n,
                5.0 + 0.5 * 1:n, #Depth
                0, #bankslope
                2*1:n, #Width
                1.0, #Sinuosity
                4.63e-07, #manning's n, day/m^(1/3)
                0.6, #CWR
                0.1, #KsatH
                .1 # Bed Sediment Thickness.
  )
  colnames(rtype) = cn
  rtype
}
#' Cut the river by PIHM.mesh
#' \code{sp.RiverSeg}
#' @param sp.mesh PIHM mesh
#' @param sp.riv river
#' @return SpatialLinesDataFrame, the \code{PIHM.river} was cut by \code{PIHM.mesh}
#' @export
sp.RiverSeg <- function(sp.mesh, sp.riv){
  sp = sp::SpatialPolygonsDataFrame(sp.mesh, 
                                 data = data.frame('iEle'=1:length(sp.mesh) ),
                                 match.ID = FALSE)
  sr =sp::SpatialLinesDataFrame(sp.riv, 
                                data = data.frame('iRiv' = 1:length(sp.riv)),
                                match.ID = FALSE )
  seg = raster::intersect(sr, sp)
  seg
}

#' Dissolve River segments
#' \code{sp.RiverDissolve}
#' @param sp SpatialLines
#' @return SpatialLinesDataFrame
#' @export
sp.DissolveLines <- function(sp){
  msg='sp.DissolveLines::'
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
      # message(msg, key, '\t', pto)
      ret = id2Line(id, key = pto)
      return (c(lp, ret) )
    }else{
      return(c(key, cn[pto]) )
    }
  }
  cdu = extractCoords(sp, unique = T)
  ft=FromToNode(sp, coord = cdu)
  ct= count(ft)
  pnode = as.numeric(names(ct))[which(ct != 2)]

  # plot(sp)
  sl=list()
  k=0
  nnode = length(pnode)
  for(i in 1:nnode ){
    key = pnode[i]
    points(cdu[key, 1], cdu[key, 2], pch=20)
    if( length(which(ft[,1] == key)) > 0){
      k = k +1
      # message(msg, k, '\t', i, '/', nnode )
      ids = id2Line(ft, key)
      # cdf[ids] = k
      points(cdu[ids, ], col=k, pch=k)
      # text(cdu[ids[1], 1], cdu[ids[1], 2], paste(k), col=2 )
      ln = sp::Line( cdu[ids,] )
      sl[[k]] = sp::Lines(ln, ID=paste(k) )
    }else{
      # print(key)
    }
  }
  sll = sp::SpatialLines(sl)
  # ord= sp.RiverOrder(sll)
  df = data.frame('INDEX'=1:length(sl))
  sld = sp::SpatialLinesDataFrame(sll, data = df)
  sld
}

#' Find Shared points in SpatialLine*
#' \code{SharedPoints}
#' @param sp SpatialLines
#' @return Topologic relationship between components of SpatialLines.
#' @export
SharedPoints <- function(sp){
  msg='SharedPoints::'
#  sp=riv
  pl = extractCoords(sp, aslist = TRUE)
  nsp = length(sp)
  ret = matrix(0, nrow = nsp, ncol=nsp)
  for(i in 1:(nsp-1) ){
    for(j in (i+1):nsp ){
      plot(rbind(pl[[i]], pl[[j]]), asp=1); 
      points(pl[[i]], col=2)
      points(pl[[j]], col=3)
      cp = CommonPoints(pl[[i]], pl[[j]]) 
      if( is.null(cp) ){
        
      }else{
        message(msg, i, '/', j)
        # print(ret)
        # print(cp)
        # readline("go")
        ret[i,j] = cp[,1]
        ret[j,i] = cp[,2]
      }
    }
  }
  ret
}
