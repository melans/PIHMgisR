#============ 
#' Setup .pihm environment.
#' \code{read.tsd} 
#' @param inpath path of PIHM input directory
#' @param outpath path of PIHM output directory
#' @param prjname charactor of PIHM project name.
#' @param anapath charactor of PIHM project name.
#' @export
PIHM <- function(prjname, inpath = './', outpath = './', 
                 anapath = file.path(outpath, 'PIHManalysis')){
  assign('PRJNAME', prjname, envir=.pihm)
  assign('inpath', inpath, envir=.pihm)
  assign('outpath', outpath, envir=.pihm)
  
  dir.create(anapath, showWarnings = F, recursive = T)
  assign('anapath', anapath, envir = .pihm)
  
  assign('PIHM.MASK', NULL, envir=.pihm)
}
