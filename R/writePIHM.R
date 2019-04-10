#' Write a backup file if the file exist
#' \code{filebackup}
#' @param file file name
#' @param backup if backup, TRUE/FALSE
#' @export
filebackup <- function(file, backup = TRUE){
  if(file.exists(file) & backup){
    bakFile <- paste0(file,format(Sys.time(), '%Y%m%d.%H%M%S'),'-',rnorm(1));
    file.copy(file,bakFile)
  }
}
#' Write xts data out into file
#' \code{write.tsd}
#' @param x xts data
#' @param file file name
#' @param append whether append
#' @param header Header of the file. Default is the dimension of data.frame.
#' @param quite TRUE/FALSE, if quiet mode
#' @param backup TRUE/FALSE, if backup the existing file
#' @export
write.tsd <- function(x, file, append = F, quite=F, header = NULL, backup=TRUE){
  filebackup(file, backup = backup)
   # x=lr$LAI
  mat = as.matrix(rbind(x))
  nr = nrow(x)
  nc = ncol(x)
  if(!quite){
    message('Writing ', file)
  }
  tt = stats::time(x)
  tday = as.numeric( difftime(tt, tt[1], units = 'days') )
  if(is.null(header)){
    t0 = format(time(x)[1], '%Y%m%d')
    header = c(nr, nc+1, t0)
  }
  dd = data.frame('Time_Day' = tday, mat)
  write(header,file = file, ncolumns = length(header), append = append, sep = '\t')
  write(colnames(dd), file = file, ncolumns = nc+1, append = T, sep = '\t')
  write(t(dd), file = file, ncolumns = nc + 1, append = T, sep = '\t')
}
#' Write data.frame out into file
#' \code{write.df}
#' @param x data.frame
#' @param file file name
#' @param append whether append
#' @param header Header of the file. Default is the dimension of data.frame.
#' @param quite TRUE/FALSE, if quiet mode
#' @param backup TRUE/FALSE, if backup the existing file
#' @export
write.df <- function(x, file, append = F, quite=F, header = NULL, backup=TRUE){
  filebackup(file, backup = backup)
  x=as.matrix(rbind(x))
  nr = nrow(x)
  nc = ncol(x)
  if(is.null(header)){
    header = c(nr,nc)
  }
  if(!quite){
    message('Writing ', file)
  }
  write(header, file = file, append = append, sep = '\t',  ncolumns = length(header))
  write(colnames(x), file = file, ncolumns = nc,append = T, sep = '\t')
  write(t(x), file = file, ncolumns = nc, append = T, sep = '\t')
}
#' Write PIHM .mesh file
#' \code{writemesh}
#' @param pm PIHMmesh class
#' @param file file name
#' @export
writemesh <- function(pm, file){
  filebackup(file)
  ncell= nrow(pm@mesh)
  np = nrow(pm@point)
  message('Writing ', file)
  write.df(pm@mesh, file=file, append=F, quite = T, backup = F)
  write.df(pm@point, file=file, append=T, quite = T, backup = F)
  # write(c(ncell, np),file = file, append = F)
  # write.table(pm@mesh, file = file, append = T, quote = F, row.names = F)
  # write.table(pm@point, file = file, append = T, quote = F, row.names = F)
}
#' Write PIHM .mesh file
#' \code{writemesh}
#' @param pr PIHMmesh class
#' @param file file name
#' @export
writeriv <- function(pr, file){
  filebackup(file)

  message('Writing ', file)
  write.df(pr@river, file=file, append=F, quite = T, backup = F)
  write.df(pr@rivertype, file=file, append=T, quite = T, backup = F)
  if(length(pr@point) >0 ){
    write.df(pr@point, file=file, append=T, quite = T, backup = F)
  }
}
#' Write PIHM .ic file
#' \code{writeinit}
#' @param x Initial condition, list()
#' @param file file name
#' @export
writeinit <- function(x, file){
  filebackup(file)
  message('Writing ', file)
  write.df(x[[1]], file=file, append=F, quite = T, backup = F)
  write.df(x[[2]], file=file, append=T, quite = T, backup = F)
}
#' Write PIHM .para or .calib file
#' \code{write.pc}
#' @param x PIHM model configure parameter or calibration
#' @param file file name
#' @param backup TRUE/FALSE
#' @export
write.pc <-function(x, file, backup=TRUE){
  if(backup){ filebackup(file) }
  message('Writing ', file)
  out=rbind(names(x), x)
  write(out, file, append = F, ncolumns = 2, sep = '\t')
}

#' Write PIHM .forc file
#' \code{writeforc}
#' @param fns  filenames; Vector of character,
#' @param path Common path of the files.
#' @param startdate Start Date. Character. e.g. 20000101
#' @param file file name
#' @export
writeforc <- function(fns, path='', startdate='20000101', file){
  filebackup(file)
  nf=length(fns)
  message('Writing ', file)
  write( paste(nf, startdate), file=file, append=F)
  write( path, file=file, append=T,  ncolumns = 1)
  write( fns, file=file, append=T,  ncolumns = 1)
}
