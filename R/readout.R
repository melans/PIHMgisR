#' Read output file from PIHM
#' @param keyword keyword of the output file. File name format is: projectname.keyword.dat
#' @param theFile  Full path of output file. 
#' @param path path of the outputfile
#' @param start Time of the first row. 
#' @keywords read output. Could be used for reading mesh and river format.
#' @return A TimeSeries data. This list require support of xts packages.
#' @examples
#' readout(keyword='YEleSurf',binary=FALSE)
#' @export  
readout <- function(keyword,
                    path=outpath,
                    theFile = file.path(path, paste0(PRJNAME,'.', keyword,'.dat') ) ,
                    start=as.Date(STARTDATE) ){
  nmax=1e9
  fid=file(theFile, 'rb');
  nc=readBin(fid, what=integer(), n=1)
  tmp=readBin(fid, what=numeric(), n=1e9, size=8)
  close(fid)
  nrr = length(tmp)/(nc+1)
  nr = round(nrr)
  if(nr < nrr){
    message('File may not completed. ', nrr, "X", nc+1)
  }
  
  mat=t(matrix(tmp[1:( nr*(nc+1) )], nrow=nc+1))
  time = start + lubridate::minutes(mat[,1])
  tsd = as.xts(mat[,-1], order.by = time)
  tsd
}