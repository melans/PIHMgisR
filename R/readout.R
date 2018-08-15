#' Read output file from PIHM
#' @param keyword keyword of the output file. File name format is: projectname.keyword.dat
#' @param file  Full path of output file. 
#' @param path path of the outputfile
#' @keywords read output. Could be used for reading mesh and river format.
#' @return A TimeSeries data. This list require support of xts packages.
#' @export  
readout <- function(keyword,
                    path=get('outpath', envir=.pihmgis) ,
                    file = file.path(path, paste0(get('PRJNAME', envir=.pihmgis),'.', keyword,'.dat') ) 
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
  xt = as.POSIXct(as.character(st), format='%Y%m%d') + mat[,1] * 60
  tsd = xts::as.xts(mat[,-1], order.by = xt)
  tsd
}
