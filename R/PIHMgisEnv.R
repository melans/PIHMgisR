#============ 
#' Setup .pihmgis environment.
#' \code{read.tsd} 
#' @param inpath path of PIHM input directory
#' @param outpath path of PIHM output directory
#' @param prjname charactor of PIHM project name.
#' @export
PIHM <- function(prjname, inpath = './', outpath = './'){
  assign('PRJNAME', prjname, envir=.pihmgis)
  assign('inpath', inpath, envir=.pihmgis)
  assign('outpath', outpath, envir=.pihmgis)
}