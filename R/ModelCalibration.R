
#' Apply population to calibration values.
#' \code{Pop2Calib}
#' @param x  Population data between 0 and 1.
#' @param calib calib data.
#' @param range Range of the calib value.
#' @export
Pop2Calib <- function(x, calib, range){
  nx = length(x)
  ret = calib
  para.name = names(range)
  para.id = which(range[1, ]!=0)
  if(length(x) !=  length(para.id)){
    cat('\nLenghth of parameter does not match number of parameters in range file')
    cat('\nParameter in range file:', para.name[para.id], '\n')
    cat('Population is: ', x, '\n')
    stop('\nERROR')
  }
  cn = para.name[para.id]
  names(x) = cn
  for(i in 1:nx){
    # message(i,'/', nx, '\t', cn[i])
    y = calib[cn[i]] #the value
    dy = x[cn[i]]
    lim = range[ c('min', 'max'), cn[i]] #limits
    ilog = range['log', cn[i] ] #if log
    if(ilog > 0){
      sig = diff(log10(lim) ) * 0.5
      val = 10^(log10(y) + sig * dy  )
    }else{
      sig = diff(lim )
      val = y +  sig * dy
    }
    # message(cn[i],'\t', y,'\t', sig, '\t', dy, '\t', val)
    # readline('goon')
    val = min(lim[2], max(val, lim[1]) )
    ret[ cn[i] ] = val
  }
  ret
}

#' Generate file path for calibration populations.
#' \code{pre.files}
#' @param iGen  Index of generations
#' @param pop Population of the generation
#' @param CV Calibration variables, list.
#' @export
pre.files <- function(iGen, pop, CV){
  prjname=CV$prjname
  calib=CV$calib
  calibrange = CV$range
  dir.out = as.character(as.matrix(CV$method$PATH_OUT))
  njob =ncol(pop)
  # dir.png = file.path(dir.out, 'png')
  dir.calib = file.path(dir.out, 'cfg.calib')
  dir.log = file.path(dir.out, 'log')
  dir.calib = file.path(dir.out, 'cfg.calib')
  dir.gen = file.path(dir.out, 'Generations')
  tmp=lapply(list(dir.out, dir.log, dir.calib, dir.gen), dir.create, showWarnings = FALSE, recursive = TRUE)
  # dir.create(dir.png, showWarnings = FALSE, recursive = TRUE)
  # dir.create(dir.calib, showWarnings = FALSE, recursive = TRUE)
  
  fn.calib = file.path(dir.calib, paste0(prjname, '_', iGen, '-', 1:njob, '.cfg.calib'))
  fn.log = file.path(dir.log, paste0(prjname, '_', iGen, '-', 1:njob, '.calib.log'))
  
  # fn.sim = file.path(outpath, paste0(prjname, '_', 1:njob), paste0(prjname, '.', ext, '.dat') )
  
  dir.modelout = file.path('output', paste0(prjname, '_', 1:njob) )
  # fn.png = file.path(pngpath, paste0(prjname, '_Step', iGen, '_Job',1:njob,'.png') )
  calibmat=NULL
  for(i in 1:ncol(pop)){
    calibmat=rbind(calibmat, Pop2Calib(pop[,i], calib, calibrange))
  }
  # calibmat = apply(pop, 2, function(x) Pop2Calib(x, calib, calibrange))
  print(calibmat)
  fn.mat = file.path(dir.gen, paste0('Generation_', iGen, '.csv') )
  write.table(calibmat, file=fn.mat, quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE, sep = '\t')
  
  for(i in 1:njob){
    icb = calibmat[i, ]
    write.config(icb, file = fn.calib[i], backup = FALSE)
  }
  
  fn.pop = file.path(dir.gen, paste0('DataPop', '.csv') )
  fn.cal = file.path(dir.gen, paste0('DataCalib', '.csv') )
  
  # x1 = cbind(t(pop))
  # x2 = cbind('step'=iGen, t(calibmat))
  x1=pop
  x2=calibmat
  if(iGen <= 1){
    write.df(x=x1, file=fn.pop, append = FALSE, header = iGen, quite = TRUE, backup = FALSE)
    write.df(x=x2, file=fn.cal, append = FALSE, header = iGen, quite = TRUE, backup = FALSE)
  }else{
    write.df(x=x1, file=fn.pop, append = TRUE, header = iGen, quite = TRUE, backup = FALSE)
    write.df(x=x2, file=fn.cal, append = TRUE, header = iGen, quite = TRUE, backup = FALSE)
  }
  ret <- list(att=
                data.frame(
                  'prjname' = prjname,
                  'fn.calib' = fn.calib,
                  'fn.log'= fn.log,
                  'dir.in' = file.path('input', prjname),
                  'dir.out' = file.path('output', paste0(prjname, '.out'), paste0(prjname, 1:njob) ) 
                ),
              'fn.cmat' = fn.mat,
              'calibmat' = calibmat)
}
# pop=rbind(rep(1, 48), rep(1,48), rep(1,48))
# x=pre.files(1, pop, CV = CV)

# pre.files(2, pop, CV = CV)

#' Execute the program in terminal.
#' \code{EXEC}
#' @param CV Calibration variables, list.
#' @param CMD.EXE  Command.
#' @param calibfile Filepath to calibration parameters.
#' @param outpath Path to output data.
#' @param fn.log Path of log file saving program stdout.
#' @export
EXEC <- function(CV, CMD.EXE, calibfile, outpath, fn.log){
  calibfile=as.character(calibfile)
  outpath=as.character(outpath)
  fn.log=as.character(fn.log)
  CMD.EXE=as.character(CMD.EXE)
  walltime = CV$method$WALLTIME
  cmd = paste(paste0(CMD.EXE),  paste('-c', calibfile), paste('-o', outpath), prjname)
  message('Walltime: ',walltime)
  if(grepl('Darwin', Sys.info()['sysname'])){  
    message(cmd)
    sret = system(cmd, intern = TRUE, ignore.stdout = FALSE,
                  wait = TRUE, timeout=walltime)
  }else{
    message(cmd)
    sret = system(cmd, intern = TRUE, ignore.stdout = FALSE, 
                  wait = TRUE, timeout=walltime)
  }
  write(sret, file= fn.log)
  message('Finish model, to output:', outpath)
}
