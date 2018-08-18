rm(list=ls())
clib=c('rgdal', 'rgeos', 'raster', 'sp')
x=lapply(clib, library, character.only=T)
# fns=list.files('R/', glob2rx('*.R'), full.names = T)
# x=lapply(fns, source)
# fns=list.files('src/', glob2rx('*.cpp'), full.names = T)
# x=lapply(fns, Rcpp::sourceCpp)

library(PIHMgisR)
#test_check("PIHMgisR")
prjname = 'sac1'
PRJNAME=prjname
inapth = 'demo/sac1/'

pihmout <- file.path('demo', prjname)
fin <- PIHM.filein(prjname, indir = pihmout)
x=list.files(pihmout, pattern = glob2rx(paste0(prjname, '.*.*')), full.names = T)
file.remove(x)

pngout = file.path(pihmout, 'fig')
gisout = file.path(pihmout, 'gis')
dir.create(pihmout, showWarnings = F, recursive = T)
dir.create(pngout, showWarnings = F, recursive = T)
dir.create(gisout, showWarnings = F, recursive = T)

a.max = 1e6 * 2;
q.min = 33;
tol.riv = 200
tol.wb = 200
tol.len = 500
AqDepth = 10
ss = 'sac2'

data(sac2)
wbd=sac2[['wbd']]
riv=sac2[['riv']]; plot(riv)
dem=sac2[['dem']]
wbbuf = rgeos::gBuffer(wbd, width = 2000)
dem = raster::crop(dem, wbbuf)


riv.s1 = rgeos::gSimplify(riv, tol=tol.riv, topologyPreserve = T)
riv.s2 = sp.simplifyLen(riv, tol.len)
plot(riv.s1); plot(riv.s2, add=T, col=3)

wb.dis = rgeos::gUnionCascaded(wbd)
wb.s1 = rgeos::gSimplify(wb.dis, tol=tol.wb, topologyPreserve = T)
wb.s2 = sp.simplifyLen(wb.s1, tol.len)

png(file = file.path(pngout, 'data_1.png'), height=11, width=11, res=100, unit='in')
plot(dem); plot(wb.s2, add=T, border=2, lwd=2); 
plot(riv.s2, add=T, lwd=2, col=4)
dev.off()


# shp.riv =raster::crop(riv.simp, wb.simp)
# shp.wb = raster::intersect( wb.simp, riv.simp)
wb.simp = wb.s2
riv.simp = riv.s2

tri = m.DomainDecomposition(wb=wb.simp,q=q.min, a=a.max)
plot(tri, asp=1)

# generate PIHM .mesh 
pm=pihmMesh(tri,dem=dem, AqDepth = AqDepth)
sm = sp.mesh2Shape(pm)
writeshape(sm, crs(wbd), file=file.path(gisout, 'domain'))

# generate PIHM .att
pa=pihmAtt(tri)

# generate PIHM .riv
pr=pihmRiver(riv.simp, dem)
# Correct river slope to avoid negative slope
pr = correctRiverSlope(pr)
# PIHMriver to Shapefile
sriv = sp.riv2shp(pr)

# Cut the rivers with triangles
sp.seg = sp.RiverSeg(pm, pr)

# Generate the River segments table
prs = pihmRiverSeg(sp.seg)

# Generate initial condition
pic = pihm.init(nrow(pm@mesh), nrow(pr@river))

# Generate shapefile of river
spp.riv = sp.riv2shp(pr); 

# Generate shapefile of mesh domain
sp.dm = sp.mesh2Shape(pm)


# model configuration, parameter
cfg.para = pihmpara(nday = 300)
# calibration
cfg.calib = pihmcalib()
para.lc = pihmlandcover()
para.geol = pihmgeol()
para.soil = pihmsoil()

lc = c(43, 23, 81, 11) 
# 43-mixed forest in NLCD classification
# 23-developed, medium           
# 81-crop land
# 11-water
lr=fun.lairl(lc, years=2000:2001)
