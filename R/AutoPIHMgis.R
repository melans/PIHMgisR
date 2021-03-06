
#' Automatically run PIHMgis based on the input data and parameters.
#' \code{autoPIHMgis}
#' @param indata  Input data, list of data.
#' @param forcfiles Filenames of .csv forcing data
#' @param prjname Projectname
#' @param outdir Output directory
#' @param a.max  Maximum area of triangles. Unit=m2
#' @param q.min Minimum of angles for each triangle
#' @param tol.riv Tolerance to simplify river lines, in meter
#' @param tol.wb Tolerance to simplify watershed boundary, in meter
#' @param tol.len Tolerance to simplyfy river by longituanal length, meter.
#' @param AqDepth Depth of aquifer bottom, which is surface elemvation minus bedrock elevation. meter.
#' @param years Years to generate the LAI, Meltfactor, Roughness Length, which must be same as the period of forcing data.
#' @param clean Whether clean the existing model files in output directory.
#' @param cfg.para  model configuration, parameter
#' @param cfg.calib model calibration
#' @param backup Whether to write backup files.
#' @param rm.outlier Whether to remove the outlier in soil/geol;
#' @param mf Meltfactor
#' @importFrom grDevices dev.off graphics.off png rgb topo.colors
#' @importFrom graphics grid hist lines par plot points
#' @importFrom methods as
#' @importFrom stats dist rnorm time
#' @importFrom utils read.table
#' @return \code{PIHM.mesh}
#' @export
#' @examples
#' data(sac)
#' indata = sac
#' sp.forc =indata[['forc']]
#' forc.fns = paste0(sp.forc@data[, 'NLDAS_ID'], '.csv')
#' forc.fns
# autoPIHMgis(sac, forcfiles = forc.fns, outdir=tempdir())
autoPIHMgis <- function(
  indata,
  forcfiles,
  prjname = 'sac',
  outdir = getwd(),
  a.max = 1e6 * .2,
  q.min = 33,
  tol.riv = 200,
  tol.wb = 200,
  tol.len = 500,
  AqDepth = 10,
  years = 2000:2010,
  clean = TRUE,
  cfg.para = pihmpara(nday = 365*length(years) +round(length(years)/4) ),
  cfg.calib = pihmcalib(),
  mf = MeltFactor(years = years),
  rm.outlier = TRUE, 
  backup=TRUE
){
  msg = paste0('autoPIHMgis(', prjname, ')::')
  pihmout = file.path(outdir)
  dir.create(pihmout, showWarnings = FALSE, recursive = TRUE)
  ny=length(years)

  inapth = file.path(prjname)

  fin <- PIHM.filein(prjname, inpath = pihmout)
  x=list.files(pihmout, pattern = utils::glob2rx(paste0(prjname, '.*.*')), full.names = T)

  pngout = file.path(pihmout, 'fig')
  gisout = file.path(pihmout, 'gis')
  dir.create(pihmout, showWarnings = F, recursive = T)
  dir.create(pngout, showWarnings = F, recursive = T)
  dir.create(gisout, showWarnings = F, recursive = T)

  # data(indata)
  wbd=indata[['wbd']]
  riv=indata[['riv']]
  dem=indata[['dem']]
  rsoil=indata[['rsoil']]
  rgeol=indata[['rsoil']]
  rlc=indata[['rlc']]
  asoil=indata[['asoil']]
  ageol=indata[['asoil']]
  alc =indata[['alc']]
  sp.forc =indata[['forc']]
  raster::plot(sp.forc)

  wbbuf = rgeos::gBuffer(wbd, width = max(c(2000, tol.wb) ) )
  dem = raster::crop(dem, wbbuf)

  png(filename = file.path(pngout, 'data_0.png'), height=11, width=11, res=100, units='in')
  raster::plot(dem); raster::plot(wbd, add=T, border=2, lwd=2); raster::plot(riv, add=T, lwd=2, col=4)
  dev.off()

  riv.s1 = rgeos::gSimplify(riv, tol=tol.riv, topologyPreserve = TRUE)
  riv.s2 = sp.simplifyLen(riv, tol.len)
  # raster::plot(riv.s1); raster::plot(riv.s2, add=T, col=3)

  wb.dis = rgeos::gUnionCascaded(wbd)
  wb.s1 = rgeos::gSimplify(wb.dis, tol=tol.wb, topologyPreserve = TRUE)
  wb.s2 = sp.simplifyLen(wb.s1, tol.len)

  png(filename = file.path(pngout, 'data_1.png'), height=11, width=11, res=100, units='in')
  raster::plot(dem); raster::plot(wb.s2, add=TRUE, border=2, lwd=2);
  raster::plot(riv.s2, add=T, lwd=2, col=4)
  dev.off()


  # shp.riv =raster::crop(riv.simp, wb.simp)
  # shp.wb = raster::intersect( wb.simp, riv.simp)
  wb.simp = wb.s2
  riv.simp = riv.s2

  tri = m.DomainDecomposition(wb=wb.simp,q=q.min, a=a.max)
  # graphics.off()
  # plot(tri, asp=1)
  message(msg, 'Number of Triangles = ', nrow(tri$T))
  # cin = readline(prompt = "See the plot. Go on?\n")
  # if( cin %in% c('n','N')){
  #   return(NULL);
  # }
  # generate PIHM .mesh
  pm=pihmMesh(tri,dem=dem, AqDepth = AqDepth)
  spm = sp.mesh2Shape(pm, crs = raster::crs(wb.s2))
  writeshape(spm, raster::crs(wbd), file=file.path(gisout, 'domain'))
  raster::plot(spm)
  png(filename = file.path(pngout, 'data_Mesh.png'), height=11, width=11, res=100, units='in')
  raster::plot(spm)
  dev.off()

  # generate PIHM .att
  pa=pihmAtt(tri, r.soil = rsoil, r.geol = rgeol, r.lc = rlc, r.forc = sp.forc )

  writeforc(forcfiles,  file=fin['md.forc'])

  # generate PIHM .riv
  pr=pihmRiver(riv.simp, dem)
  oid = getOutlets(pr)
  message(msg, 'Number of Rivers = ', nrow(pr@river))
  # Correct river slope to avoid negative slope

  # PIHMriver to Shapefile
  spr = riv.simp
  writeshape(spr, raster::crs(wbd), file=file.path(gisout, 'river'))

  if(length(oid)>1){
    warning("There are ", length(oid), ' outlets in streams')
    dev.off();
    raster::plot(spr); raster::plot(spr[oid, ], add=TRUE, col=2, lwd=3)
    flag = readline('Continue?')
    if(flag =='N' | flag =='n'){
      stop('Exit the autoPIHMgis')
    }
  }
  # Cut the rivers with triangles
  sp.seg = sp.RiverSeg(spm, spr)
  writeshape(sp.seg, raster::crs(wbd), file=file.path(gisout, 'seg'))

  # Generate the River segments table
  prs = pihmRiverSeg(sp.seg)

  # Generate initial condition
  pic = pihm.init(nrow(pm@mesh), nrow(pr@river))

  # Generate shapefile of river
  # spp.riv = sp.riv2shp(pr);
  

  # Generate shapefile of mesh domain
  message(msg, 'Generate shapefile of mesh domain')
  sp.dm = sp.mesh2Shape(pm)
  png(filename = file.path(pngout, 'data_2.png'), height=11, width=11, res=100, units='in')
  zz = sp.dm@data[,'Zsurf']
  ord=order(zz)
  col=grDevices::terrain.colors(length(sp.dm))
  raster::plot(sp.dm[ord, ], col = col)
  raster::plot(spr, col=pr@river$Type+1 , add=TRUE, lwd=3)
  dev.off()

  #soil/geol/landcover
  lc = unlist(alc)
  para.lc = PTF.lc(lc)
  para.soil = PTF.soil(asoil, rm.outlier = rm.outlier)
  para.geol = PTF.geol(asoil, rm.outlier = rm.outlier)


  # 43-mixed forest in NLCD classification
  # 23-developed, medium
  # 81-crop land
  # 11-water
  lr=fun.lairl(lc, years=years)
  graphics.off()
  png(filename = file.path(pngout, 'data_lairl.png'), height=11, width=11, res=100, units='in')
  graphics::par(mfrow=c(2,1))
  col=1:length(lc)

  zoo::plot.zoo(lr$LAI, col=col, main='LAI');
  graphics::legend('top', paste0(lc), col=col, lwd=1)
  zoo::plot.zoo(lr$RL, col=col, main='Roughness Length');
  graphics::legend('top', paste0(lc), col=col, lwd=1)
  dev.off()
  write.tsd(backup=backup,lr$LAI, file = fin['md.lai'])
  write.tsd(backup=backup,lr$RL, file = fin['md.rl'])

  write.tsd(backup=backup,mf, file=fin['md.mf'])

  # write PIHM input files.
  writemesh(backup=backup,pm, file = fin['md.mesh'])
  writeriv(backup=backup,pr, file=fin['md.riv'])
  writeinit(backup=backup,pic, file=fin['md.ic'])

  write.df(backup=backup,pa, file=fin['md.att'])
  write.df(backup=backup,prs, file=fin['md.rivseg'])
  write.df(backup=backup,para.lc, file=fin['md.lc'])
  write.df(backup=backup,para.soil, file=fin['md.soil'])
  write.df(backup=backup,para.geol, file=fin['md.geol'])

  write.config(backup=backup,cfg.para, fin['md.para'])
  write.config(backup=backup,cfg.calib, fin['md.calib'])
  message(msg, 
          'Ncell=', nrow(pm@mesh), '\t',
          'Nriv=', nrow(pr@river), '\t',
          'Nseg=', nrow(prs), '\n'
          )
  pm <- pm
}
