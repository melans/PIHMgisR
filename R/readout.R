#' Read output file from PIHM
#' @param keyword keyword of the output file. File name format is: projectname.keyword.dat
#' @param file  Full path of output file. 
#' @param path path of the outputfile
#' @keywords read output. Could be used for reading mesh and river format.
#' @return A TimeSeries data. This list require support of xts packages.
#' @export  
readout <- function(keyword,
                    path=get('outpath', envir=.pihm) ,
                    file = file.path(path, paste0(get('PRJNAME', envir=.pihm),'.', keyword,'.dat') ) 
){
  fid=file(file, 'rb');
  nc=readBin(fid, what=integer(), n=1)
  st=readBin(fid, what=integer(), n=1, size = 8) #Long integer
  tmp=readBin(fid, what=numeric(), n=1e9, size=8)
  close(fid)
  nrr = length(tmp)/(nc+1)
  nr = round(nrr)
  if(nr < nrr){
    message('File may not completed. ', nrr, "X", nc+1)
  }
  mat=t(matrix(tmp[1:( nr*(nc+1) )], nrow=nc+1))
  tsec = ( mat[,1] - mean(diff(mat[,1])) ) * 60 # time must shift back ONE dt.
  xt = as.POSIXct(as.character(st), format='%Y%m%d') + tsec
  tsd = xts::as.xts(mat[,-1], order.by = xt)
  tsd
}
#' Read multiply PIHM output files and do time-series plot
#' @param varname vector of output keywords 
#' @param plot Whether do the time-series plot
#' @param imap Whether do the raster plot for Element data. Only works for element data
#' @param return Whether return the data. Some the results are too huge to load in memoery at once.
#' @keywords read output.
#' @return A list of TimeSeries data. 
#' @export  
BasicPlot <- function(
  varname=c(paste0('YEle',c( 'surf','unsat', 'gw') ), 
            paste0('qEle',c('prcp','etp','infil', 'rech') ),
            paste0('qEle',paste0('et',0:2) ),
            paste0('QRiv',c('flx', 'sub', 'surf')),
            paste0('YRiv','stage')
  ) ,
  plot=TRUE, imap=FALSE,
  return=T){
  graphics.off()
  print(varname)
  nv=length(varname)
  prjname = get('PRJNAME', envir = .pihm)
  if(imap ){
    sp.riv = sp.riv2shp()
  }
  if(plot){
    path = file.path(get('anapath', envir = .pihm), 'BasicPlot')
    if(!dir.exists(path)){
      dir.create(path, recursive = T, showWarnings = F)
    }
  }
  
  ret=list()
  for(i in 1:nv){
    vn=varname[i]
    fn= paste0(prjname,'_', vn, '.png')
    message(i, '/', nv, '\t', vn, '\t', fn)
    x=readout(vn);
    if(return){
      ret[[i]] = x;
    }
    if(plot){
      time =time(x)
      t0=min(time)
      t1=max(time)
      tr=paste( strftime(t0) ,'-', strftime(t1))
      png(file.path(path, fn), width=11, height=9, res=100, units = 'in')
      pp=xts::plot.xts(x, main=vn)
      print(pp)
      dev.off()
      if(imap ){
        if( grepl('^YEle', vn) | grepl('^qEle', vn) ){
          fn= paste0('Map.', prjname,'_', vn, '.png')
          y = colMeans(x)
          png(file.path(path, fn), width=11, height=9, res=100, units = 'in')
          map2d(y)
          raster::plot(sp.riv, col=rgb(1,0,0,0.7), lwd=2, add=T)
          dev.off()
        }
      }
    }
  }
  if(return){
    names(ret) = varname;
    ret
  }
}
