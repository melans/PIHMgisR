#' Prepare Input file of PIHM
#' \code{PIHM.filein} 
#' @param projname Character, project name
#' @param indir PIHM input directory
#' @return Character of full path of input files for PIHM
#' @export
PIHM.filein <- function(projname, indir = getwd() ){
  fn.mesh = file.path(indir, paste0(projname, '.sp.mesh' ) );
  fn.att = file.path(indir, paste0(projname, '.sp.att' ) );
  fn.pt = file.path(indir, paste0(projname, '.sp.points' ) )
  fn.edge = file.path(indir, paste0(projname, '.sp.edges' ) )
  fn.mdriv = file.path(indir, paste0(projname, '.sp.riv' ) )
  fn.mdseg = file.path(indir, paste0(projname, '.sp.rivchn' ) )
  
  fn.ic = file.path(indir, paste0(projname, '.cfg.ic' ) )
  fn.para = file.path(indir, paste0(projname, '.cfg.para' ) )
  fn.calib = file.path(indir, paste0(projname, '.cfg.calib' ) )
  
  fn.soil = file.path(indir, paste0(projname, '.para.soil' ) )
  fn.geol = file.path(indir, paste0(projname, '.para.geol' ) )
  fn.lc = file.path(indir, paste0(projname, '.para.lc' ) )
  
  fn.forc = file.path(indir, paste0(projname, '.tsd.forc' ) )
  fn.bc = file.path(indir, paste0(projname, '.tsd.bc' ) )
  fn.lai = file.path(indir, paste0(projname, '.tsd.lai' ) )
  fn.rl = file.path(indir, paste0(projname, '.tsd.rl' ) )
  fn.mf = file.path(indir, paste0(projname, '.tsd.mf' ) )
  
  fns = c(
          fn.mesh, fn.pt, fn.edge, fn.mdseg, fn.mdriv, fn.att,
          fn.ic,fn.para, fn.calib,
          fn.soil, fn.geol, fn.lc,
          fn.forc, fn.bc, fn.lai, fn.rl, fn.mf
  )
  names(fns) = c("md.mesh","md.pt", "md.edge", "md.rivseg", "md.riv", "md.att",
                 'md.ic', 'md.para', 'md.calib',
                 'md.soil', 'md.geol', 'md.lc',
                 'md.forc', 'md.bc', 'md.lai', 'md.rl', 'md.mf' )
  fns
}