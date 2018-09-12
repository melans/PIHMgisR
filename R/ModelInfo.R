
#' Summary of the basic information of PIHM input data.
#' \code{ModelInfo} 
#' @param  path Where to export the basic model infomation.
#' @return Basic model infomation, figures and tables
#' @export
ModelInfo <- function(path=get('outpath', envir=.pihm) ){
  outdir = file.path(path, 'ModelInfo')
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  pm=readmesh()
  PIHM.mask(pm = pm)
  
  r=MeshData2Raster()
  
  ia = data.frame('Area' =  getArea() )
  png.control(fn = paste0('AreaHist','.png'), path=outdir, ratio = 11/9 ) 
  nb=20
  p<-ggplot2::ggplot(ia, aes(Area)) + ggplot2::labs( x = "Area (m2)" ) +
    ggplot2::geom_histogram(bins=nb) + 
    ggplot2::geom_freqpoly(bins=nb, col=2, lty=2)
  print(p)
  dev.off()
  
  
  myhist <- function(x, icols, pp){
    par(mfrow = pp)
    cn=colnames(x)
    for(i in 1:length(icols)){
      hist(x[, icols[i]], xlab=cn[ icols[i] ], main='')
    }
  }
  #======Soil============
  s=readsoil()
  png.control(fn = paste0('soil','.png'), path=outdir, ratio = 11/9 ) 
  myhist(s, icols=c(2, 3, 6,7,9), pp=c(2,3))
  dev.off()
  
  #======Geol============
  g=readgeol()
  png.control(fn = paste0('geol','.png'), path=outdir, ratio = 11/9 ) 
  myhist(g, icols=c(2,3,4,4,7,8), pp=c(2,3))
  dev.off()
}
