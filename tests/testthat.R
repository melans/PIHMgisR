# rm(list=ls())
# clib=c('rgdal', 'rgeos', 'raster', 'sp')
# x=lapply(clib, library, character.only=T)
# # fns=list.files('R/', glob2rx('*.R'), full.names = T)
# # x=lapply(fns, source)
# # fns=list.files('src/', glob2rx('*.cpp'), full.names = T)
# # x=lapply(fns, Rcpp::sourceCpp)
# 
# library(PIHMgisR)
# #test_check("PIHMgisR")
# 
# a.max = 1e6 * 2;
# q.min = 33;
# tol.riv = 200
# tol.wb = 200
# tol.len = 500
# AqDepth = 10
# 
# years = 2000:2001
# data(sac)
# wbd=sac[['wbd']]
# riv=sac[['riv']]; plot(riv)
# dem=sac[['dem']]
# rsoil=sac[['rsoil']]
# rgeol=sac[['rsoil']]
# rlc=sac[['rlc']]
# asoil=sac[['asoil']]
# ageol=sac[['asoil']]
# alc =sac[['alc']]
# sp.forc =sac[['forc']]
# 
# wbbuf = rgeos::gBuffer(wbd, width = 2000)
# dem = raster::crop(dem, wbbuf)
# 
# 
# riv.s1 = rgeos::gSimplify(riv, tol=tol.riv, topologyPreserve = T)
# riv.s2 = sp.simplifyLen(riv, tol.len)
# plot(riv.s1); plot(riv.s2, add=T, col=3)
# 
# wb.dis = rgeos::gUnionCascaded(wbd)
# wb.s1 = rgeos::gSimplify(wb.dis, tol=tol.wb, topologyPreserve = T)
# wb.s2 = sp.simplifyLen(wb.s1, tol.len)
# 
# 
# # shp.riv =raster::crop(riv.simp, wb.simp)
# # shp.wb = raster::intersect( wb.simp, riv.simp)
# wb.simp = wb.s2
# riv.simp = riv.s2
# 
# tri = m.DomainDecomposition(wb=wb.simp,q=q.min, a=a.max)
# plot(tri, asp=1)
# 
# # generate PIHM .mesh 
# pm=pihmMesh(tri,dem=dem, AqDepth = AqDepth)
# sm = sp.mesh2Shape(pm)
# 
# # generate PIHM .att
# pa=pihmAtt(tri)
# 
# # generate PIHM .riv
# pr=pihmRiver(riv.simp, dem)
# # Correct river slope to avoid negative slope
# pr = correctRiverSlope(pr)
# # PIHMriver to Shapefile
# sriv = sp.riv2shp(pr)
# 
# # Cut the rivers with triangles
# sp.seg = sp.RiverSeg(pm, pr)
# 
# # Generate the River segments table
# prs = pihmRiverSeg(sp.seg)
# 
# # Generate initial condition
# pic = pihm.init(nrow(pm@mesh), nrow(pr@river))
# 
# # Generate shapefile of river
# spp.riv = sp.riv2shp(pr); 
# 
# # Generate shapefile of mesh domain
# sp.dm = sp.mesh2Shape(pm)
# 
# 
# # model configuration, parameter
# cfg.para = pihmpara(nday = 300)
# # calibration
# cfg.calib = pihmcalib()
# 
# #soil/geol/landcover
# lc = c(43, 23, 81, 11) 
# # 43-mixed forest in NLCD classification
# # 23-developed, medium           
# # 81-crop land
# # 11-water
# para.lc = PTF.lc(lc)
# para.soil = PTF.soil()
# para.geol = PTF.geol()
# lr=fun.lairl(lc, years=years)
# 
# mf = MeltFactor(years)