#============ 
#' Setup .pihm environment.
#' \code{read.tsd} 
#' @param inpath path of PIHM input directory
#' @param outpath path of PIHM output directory
#' @param prjname charactor of PIHM project name.
#' @param anapath charactor of PIHM project name.
#' @export
PIHM <- function(prjname, inpath = file.path('input', prjname), 
                 outpath = file.path('output', paste0(prjname, '.out')), 
                 anapath = file.path(outpath, 'PIHManalysis')){
  assign('PRJNAME', prjname, envir=.pihm)
  assign('inpath', inpath, envir=.pihm)
  assign('outpath', outpath, envir=.pihm)
  
  dir.create(anapath, showWarnings = F, recursive = T)
  assign('anapath', anapath, envir = .pihm)
  assign('PIHM.MASK', NULL, envir=.pihm)
  print(prjname)
  print(inpath)
  print(outpath)
  print(anapath)
  return(list(prjname=prjname, inpath=inpath, outpath=outpath, anapath=anapath))
}
