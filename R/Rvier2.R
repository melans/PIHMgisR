#' calculate river order
#' \code{pihmRiver2}
#' @param sp SpatialLines*
#' @param dem Raster of elevation
#' @param minslope  Miminum slope of river bed.
#' @return Stream Order of SpatialLines*
#' @export
pihmRiver2 <- function(sp, dem, minslope=1/1000){
  message('\nRiver Order')
  rivord = sp.RiverOrder(sp)

  message('\nRiver downstream')
  rivdown = sp.RiverDown(sp)


  message('\nRiver Table')
  nsp=length(sp)
  ft = FromToNode(sp)
  px=extractCoords(sp)
  z0 = raster::extract(dem, px[ft[,1],]);
  z1 = raster::extract(dem, px[ft[,2],])
  dz = z0 - z1
  len = rgeos::gLength(sp,byid = TRUE)
  s = dz /len
  id=which(s < minslope)
  dz[id] = len[id] * minslope;
  df = data.frame('Index'=1:nsp,
                  'Down' = rivdown,
                  'Type' = rivord,
                  'Slope' = s,
                  'Length' = len )

  sp.df = SpatialLinesDataFrame(sp, data=df)
  ntype = max(rivord)
  rtype = RiverType(ntype)
  PIHM.river(river = df, rivertype = data.frame(rtype), point = data.frame() )
}
# pr=pihmRiver2(riv.simp, dem)
