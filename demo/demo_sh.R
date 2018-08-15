rm(list=ls())
clib=c('rgdal', 'rgeos', 'raster', 'sp')
x=lapply(clib, library, character.only=T)
# fns=list.files('R/', glob2rx('*.R'), full.names = T)
# x=lapply(fns, source)
# fns=list.files('src/', glob2rx('*.cpp'), full.names = T)
# x=lapply(fns, Rcpp::sourceCpp)

library(PIHMgisR)
#test_check("PIHMgisR")
prjname = 'sac'

pihmout <- file.path('demo', prjname)
fin <- PIHM.filein(prjname, indir = pihmout)
if (dir.exists(pihmout)){
  unlink(pihmout, recursive = T, force = T)
}
dir.create(pihmout, showWarnings = F, recursive = T)

a.max = 1000;
q.min = 33;
tol.riv=50
tol.wb=50


data(sh)
wbd=sh[['wbd']]
riv=sh[['riv']]
dem=sh[['dem']]




riv.simp = rgeos::gSimplify(riv, tol=10, topologyPreserve = T)

wb.dis = rgeos::gUnionCascaded(wb)
length(wb.dis)
wb.simp = rgeos::gSimplify(wb.dis, tol=10, topologyPreserve = T)

# shp.riv =raster::crop(riv.simp, wb.simp)
# shp.wb = raster::intersect( wb.simp, riv.simp)

tri = m.DomainDecomposition(wb=wb.simp,q=q.min, a=a.max)

# generate PIHM .mesh 
pm=pihmMesh(tri,dem=dem, AqDepth = 10)

# generate PIHM .att
pa=pihmAtt(tri)

# generate PIHM .riv
pr=pihmRiver(riv.simp)

# Cut the rivers with triangles
sp.seg = sp.RiverSeg(pm, pr)
# Generate the River segments table
prs = pihmRiverSeg(sp.seg)

# Generate initial condition
pic = pihm.init(nrow(pm@mesh), nrow(pr@river))

spp.riv = sp.riv2shp(pr); 
plot(tri); plot(spp.riv, col=spp.riv@data[,5] + 1 , add=T, lwd=3)

# model configuration, parameter
cfg.para = pihmpara()
# calibration
cfg.calib = pihmcalib()

# write PIHM input files.
writemesh(pm, file = fin['md.mesh'])
writeriv(pr, file=fin['md.riv'])
writeinit(pic, file=fin['md.ic'])

write.df(pa, file=fin['md.att'])
write.df(prs, file=fin['md.rivseg'])
write.pc(cfg.para, fin['md.para'])
write.pc(cfg.calib, fin['md.calib'])
